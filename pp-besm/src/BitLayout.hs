{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeOperators #-}

module BitLayout where

import Data.BitVector.Sized (BV (..), mkBV, pattern BV)
import qualified Data.BitVector.Sized as BV hiding (bitVector')
import Data.Foldable
import Data.Parameterized
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import GHC.TypeNats

data Chunk (w :: Nat) :: * where
  Chunk ::
    NatRepr w -> -- width of range
    Natural -> -- index of range start
    Chunk w

deriving instance Show (Chunk w)

instance ShowF Chunk where
  showF = show

chunk :: KnownNat w => Natural -> Chunk w
chunk = Chunk knownNat

data BitLayout (t :: Nat) (s :: Nat) :: * where
  BitLayout :: NatRepr t -> NatRepr s -> Seq (Some Chunk) -> BitLayout t s

deriving instance Show (BitLayout t s)

-- | Construct an empty 'BitLayout'.
empty :: KnownNat t => BitLayout t 0
empty = BitLayout knownNat knownNat S.empty

-- First, extract the appropriate bits as a BV t, where the relevant bits
-- start at the LSB of the vector (so, mask and shiftL). Then, truncate to a
-- BV s, and shiftinto the starting position.
extractChunk ::
  (KnownNat s, 1 <= s) =>
  -- | width of output
  NatRepr s ->
  -- | where to place the chunk in the result
  Natural ->
  -- | location/width of chunk in the input
  Some Chunk ->
  -- | input vector
  BV t ->
  BV s
extractChunk sRepr sStart (Some (Chunk chunkRepr chunkStart)) tVec =
  BV.ashr knownNat extractedChunk sStart
 where
  extractedChunk =
    case decideLeq (incNat chunkRepr) sRepr of
      Left LeqProof -> BV.zext sRepr (BV.select' chunkStart chunkRepr tVec)
      Right _ -> error ""

extractAll ::
  (1 <= s, KnownNat s) =>
  -- | determines width of output vector
  NatRepr s ->
  -- | current position in output vector
  Natural ->
  -- | list of remaining chunks to place in output vector
  [Some Chunk] ->
  -- | input vector
  BV t ->
  BV s
extractAll sRepr _ [] _ = BV.zero sRepr
extractAll sRepr outStart (chk@(Some (Chunk chunkRepr _)) : chunks) tVec =
  extractChunk sRepr outStart chk tVec
    `BV.or` extractAll sRepr (outStart + chunkWidth) chunks tVec
 where
  chunkWidth = fromInteger (intValue chunkRepr)

-- | Use a 'BitLayout' to extract a smaller vector from a larger one.
extract ::
  (1 <= s, KnownNat s) =>
  -- | The layout
  BitLayout t s ->
  -- | The larger vector to extract from
  BV t ->
  BV s
extract (BitLayout _ sRepr chunks) = extractAll sRepr 0 (toList chunks)

-- TODO: Should this be in Maybe?

{- | Add a 'Chunk' to a 'BitLayout'. If the 'Chunk' does not fit, either because the
 resulting 'BitLayout' would be too long or because it would overlap with a 'Chunk'
 that is already in the 'BitLayout', we throw an error.
-}
(<:) ::
  -- | chunk to add
  Chunk r ->
  -- | layout we are adding the chunk to
  BitLayout t s ->
  BitLayout t (r + s)
chk@(Chunk rRepr _) <: bl@(BitLayout tRepr sRepr chunks) =
  if chk `chunkFits` bl
    then BitLayout tRepr (rRepr `addNat` sRepr) (chunks S.|> Some chk)
    else
      error $
        "chunk "
          ++ show chk
          ++ " does not fit in layout of size "
          ++ show (natValue tRepr)
          ++ ": "
          ++ show bl

-- TODO: check precedence (associativity is correct)
infixr 6 <:

chunkFits :: Chunk r -> BitLayout t s -> Bool
chunkFits chk@(Chunk rRepr start) (BitLayout tRepr sRepr chunks) =
  (natValue rRepr + natValue sRepr <= natValue tRepr)
    && (fromIntegral start + natValue rRepr <= natValue tRepr) -- widths are ok
    && (0 <= start) -- chunk lies within the bit vector
    && noOverlaps chk (toList chunks)

noOverlaps :: Chunk r -> [Some Chunk] -> Bool
noOverlaps chk = all (chunksDontOverlap (Some chk))

chunksDontOverlap :: Some Chunk -> Some Chunk -> Bool
chunksDontOverlap (Some (Chunk chunkRepr1 start1)) (Some (Chunk chunkRepr2 start2)) =
  if start1 <= start2
    then start1 + chunkWidth1 <= start2
    else start2 + chunkWidth2 <= start1
 where
  chunkWidth1 = fromIntegral (natValue chunkRepr1)
  chunkWidth2 = fromIntegral (natValue chunkRepr2)

{- | Given a starting position, insert (via "or") a smaller 'BV' @s@ with a larger
 'BV' @t@ at that position.
-}
bvOrAt ::
  forall s t.
  (KnownNat s, KnownNat t, 1 <= t, s + 1 <= t) =>
  Natural ->
  BV s ->
  BV t ->
  BV t
bvOrAt start sVec tVec =
  BV.ashr knownNat (BV.zext knownNat sVec) start `BV.or` tVec

{- | Given a list of 'Chunk's, inject each chunk from a source 'BV' @s@ into a
 target 'BV' @t@.
-}
bvOrAtAll ::
  (KnownNat s, KnownNat t, 1 <= s, 1 <= t, s + 1 <= t) =>
  NatRepr t ->
  [Some Chunk] ->
  BV s ->
  BV t
bvOrAtAll tRepr [] _ = BV.zero tRepr
bvOrAtAll tRepr (Some (Chunk chunkRepr chunkStart) : chunks) sVec =
  bvOrAt chunkStart (BV.truncBits chunkWidth sVec) (bvOrAtAll tRepr chunks (BV.ashr knownNat sVec (-chunkWidth)))
 where
  chunkWidth = fromIntegral (natValue chunkRepr)

-- | Use a 'BitLayout' to inject a smaller vector into a larger one.
inject ::
  (KnownNat s, KnownNat t, 1 <= t, 1 <= s, s + 1 <= t) =>
  -- | The layout
  BitLayout t s ->
  -- | The larger vector to inject into
  BV t ->
  -- | The smaller vector to be injected
  BV s ->
  BV t
inject (BitLayout tRepr _ chunks) tVec sVec =
  bvOrAtAll tRepr (toList chunks) sVec `BV.or` tVec

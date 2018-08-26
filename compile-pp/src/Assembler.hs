{-# LANGUAGE DataKinds #-}
module Assembler where

import Syntax

import qualified Data.Map as M
import           Data.Map (Map)

import Data.List as L
import Data.Maybe
import Data.Foldable
import Control.Monad

import Control.Category ((>>>))
import Data.Function ((&))

import Data.BitVector.Sized

-- Dummy Definitions
type ConstantDefs = [(String, ConstantInfo)] -- map of constant name to value
type Output = [BitVector 39]

{-

  1. Linearize blocks and insert explicit jumps
  2. Resolve constants and assign relative addresses in instructions
  3. Give absolute addresses
-}

data RelativeAddr
  = Rel Section Int -- Constant and operator references
  | Abs Int -- For statically known addresses 'aka standard cells'
  deriving (Show, Eq)

data Section = Text | Data
  deriving (Show, Eq)

data Alignment = AlignLeft | AlignRight

assemble :: ConstantDefs -> Alignment -> [BB Address] -> Output
assemble defs a =
      segmentize
  >>> resolve    defs
  >>> absolutize defs a
  >>> render     defs a

render :: ConstantDefs -> Alignment -> [RawBlock] -> Output
render defs a blks = let
  textS = blks >>= asmToCell
  dataS = defs >>= (constantToCell . snd)
  total = dataS ++ textS
  in case a of
    AlignLeft -> (replicate 15 (bitVector 0)) ++ total
    AlignRight -> replicate (1023 - length total) (bitVector 0) ++ total

constantToCell :: ConstantInfo -> [BitVector 39]
constantToCell (Size i) = replicate i (bitVector 0)
constantToCell (Val  i) = [bitVector $ fromIntegral i]

absolutize :: ConstantDefs -> Alignment -> [BB RelativeAddr] -> [RawBlock]
absolutize defs align blks = let
  cSize = sum (map (constantSize . snd) defs)
  bSize = sum (map instLen blks)
  maxVal = 1023
  offset = case align of
    AlignLeft  -> 0x10 + cSize
    AlignRight -> 1023 - bSize + 1
  in map (fmap (absolve cSize offset)) blks
  where
  absolve _ offset (Rel Text i) = offset + i
  absolve c offset (Rel Data i) = offset - c + i
  absolve _ _      (Abs i)      = i

missingConstants :: ConstantDefs -> [BB Address] -> [Address]
missingConstants defs blks = let
  needed = nub $ map toList blks >>= filter isUnknown
  have = nub $ map (Unknown . fst) defs

  in needed \\ have

resolve :: ConstantDefs -> [BB Address] -> [BB RelativeAddr]
resolve conses blks = let
  (_, constantOffsets) = mapAccumL (\off (c, info) -> (off + constantSize info, (Unknown c, Rel Data off))) 0 conses
  bbOffsets = blockOffsets 0 blks
  in map (fmap (relativize (M.fromList $ bbOffsets ++ constantOffsets))) blks

blockOffsets :: Int -> [BB Address] -> [(Address, RelativeAddr)]
blockOffsets off (bb : blks) = let
  rest = blockOffsets (off + instLen bb) blks
  in case (baseAddress bb, terminator bb) of
    (o@(Offset a _), RetRTC _) -> (o, Rel Text (off + instLen bb - 1)) : rest -- l + p + 2
    (o             , RetRTC _) -> (o, Rel Text off) : (RTC o, Rel Text (off + instLen bb - 1) ) : rest
    (Offset _ _    , _) -> rest
    (o             , _) ->      (o, Rel Text off) : rest

blockOffsets _ [] = []

relativize :: Map Address RelativeAddr -> Address -> RelativeAddr
relativize m (Procedure _) =  Abs 0x1 -- stop instruction
relativize m u@(Unknown s)   = case u `M.lookup` m of
  Just cons -> cons
  Nothing -> error $ "Missing constant offset for " ++ show s
relativize m o@(Operator n)  = case o `M.lookup` m of
  Just o -> o
  Nothing -> error $ "Missing operator offset for " ++ show o
relativize m r@(RTC a)       = case r `M.lookup` m of
  Just rtc -> rtc  -- l + p + 2
  Nothing  -> error $ "Missing RTC return for " ++ show a
relativize m (Offset a o)  = case relativize m a of
  Abs i -> Abs (i + o)
  Rel s i -> Rel s (i + o)
relativize m (Absolute a) = Abs a

toUnsegmentedMap :: [BB Address] -> Map Address (BB Address)
toUnsegmentedMap bbs = M.fromList $ L.map (\bb -> (baseAddress bb, bb)) bbs

{-
  Break up the CFG into a series of linear chunks, add explicit jumps between segments
  and merge it back together in a linearized CF
-}
segmentize :: [BB Address] -> [BB Address]
segmentize blks = join . reverse $ map addJump $ go (toUnsegmentedMap blks) []
  where
  go map acc | map == M.empty = acc
  go map acc = let
    (map', a) = segment map (head $ M.keys map)
    in go map' (a : acc)

{-
  Extract a linear segment from the block map.
-}

segment :: Map Address (BB Address) -> Address -> (Map Address (BB Address), [BB Address])
segment map addr = case addr `M.lookup` map of
  Just bb -> case (implicitJumps (terminator bb)) of
    Just tar -> fmap (bb :) $ segment (M.delete addr map) tar
    Nothing  -> (M.delete addr map, [bb])
  Nothing -> (map, [])

addJump :: [BB Address] -> [BB Address]
addJump [bb] = case implicitJumps (terminator bb) of
  Just iJ -> let
    bb' = bb { terminator = CCCC (baseAddress jB) } -- add hard jump
    jB = jumpBlk bb iJ
    in [bb', jB]
  Nothing -> [bb]
  where
  jumpBlk fromB to = BB [] (CCCC to) ((baseAddress fromB) `offAddr` (instLen fromB))
addJump (bb : bbs) = bb : addJump bbs
addJump [] = []

directJumps (Comp      _ _ a _) = Just a
directJumps (CompWord  _ _ a _) = Just a
directJumps (CompMod   _ _ a _) = Just a
directJumps (CCCC          _)   = Nothing
directJumps (CCCCSnd     _ a)   = Just a
directJumps (Stop)              = Nothing
directJumps (SwitchStop)        = Nothing
directJumps (Chain     _)       = Nothing
directJumps (RetRTC _)          = Nothing

-- These are the jumps that could potentially be optimized away.
implicitJumps :: Term a -> Maybe a
implicitJumps (Comp      _ _ _ a) = Just a
implicitJumps (CompWord  _ _ _ a) = Just a
implicitJumps (CompMod   _ _ _ a) = Just a
implicitJumps (CCCC          a)   = Just a
implicitJumps (CCCCSnd     _ _)   = Nothing
implicitJumps (Stop)              = Nothing
implicitJumps (SwitchStop)        = Nothing
implicitJumps (Chain     a)       = Just a
implicitJumps (RetRTC _)          = Nothing

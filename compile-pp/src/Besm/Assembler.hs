{-# LANGUAGE DataKinds, BinaryLiterals, DeriveFunctor,
  LambdaCase, DataKinds, KindSignatures, TypeFamilies,
  StandaloneDeriving, RecordWildCards, FlexibleContexts, FlexibleInstances
#-}
module Besm.Assembler where

import Besm.Assembler.Syntax

import qualified Data.Map as M
import           Data.Map (Map)

import Data.List as L
import Data.Maybe
import Data.Foldable
import Control.Monad

import Control.Category ((>>>))
import Data.Function ((&))

import Data.BitVector.Sized

import Besm.Put

import Data.Either (partitionEithers)
import Data.Bifunctor (first)

type ConstantDefs = [(String, ConstantInfo Address)] -- map of constant name to value
type Output = [BitVector 64]

{-

  1. Linearize blocks and insert explicit jumps
  2. Resolve constants and assign relative addresses in instructions
  3. Give absolute addresses
-}

data Alignment = AlignLeft | AlignRight

data Stage = Input | LaidOut | Relativized | Absolutized

type family AddressType (s :: Stage) where
  AddressType Relativized = RelativeAddr
  AddressType Absolutized = Int
  AddressType a = Address

type family ConstantMap (s :: Stage) where
  ConstantMap Absolutized = [(String, ConstantInfo Int)]
  ConstantMap Relativized = [(String, ConstantInfo RelativeAddr)]
  ConstantMap a = [(String, ConstantInfo Address)]

data ModuleAssembly (stage :: Stage) = Mod
  { offsetMap   :: Maybe (Map Address Int)
  , relativeMap :: Maybe (Map Address RelativeAddr)
  , constants   :: ConstantMap stage
  , procs       :: [Procedure (AddressType stage)]
  }

deriving instance Show (ModuleAssembly 'Absolutized)

debugRender :: Alignment -> ModuleAssembly Absolutized -> IO ()
debugRender align mod = let
  (o, dataS) = mapAccumL (\off (s, v) -> (off + constantSize v, (off, s ++ " " ++ show v))) 0 (constants mod)
  text  = procs mod >>= blocks >>= renderBlock
  textS = zip [o..] text
  len = o + length textS
  offset = case align of
            AlignLeft -> 16
            AlignRight -> 1024 - len
  in forM_ (dataS ++ textS) $ \(addr, inst) -> putStrLn $ show addr ++ ": " ++ inst

  where

  renderBlock :: BB Int -> [String]
  renderBlock bb = map show (instrs bb) ++ termToString (terminator bb)

  termToString :: Term Int -> [String]
  termToString (RetRTC a) = [show $ AI 0b10100001111 (a+1) (a+1), "zero"]
  termToString (Chain     _) = []
  termToString c = [show c]


assemble :: ConstantDefs -> Alignment -> [Procedure Address] -> Either [String] Output
assemble defs a = compile defs a >=> pure . render     a

compile :: ConstantDefs -> Alignment -> [Procedure Address] -> Either [String] (ModuleAssembly 'Absolutized)
compile defs align =
  pure . Mod Nothing Nothing defs
  >=> checkForMissingConstantDefs defs
  >=> pure . layout
  >=> pure . internalizeModule
  >=> relativize
  >=> pure . absolutize align

  where

  checkForMissingConstantDefs :: ConstantDefs -> ModuleAssembly 'Input -> Either [String] (ModuleAssembly 'Input)
  checkForMissingConstantDefs defs mod = let
    (ms, es) = unzip $ map (missingConstants defs) (procs mod)
    in case (anyMissing ms, allExtra es) of
      ([], []) -> Right mod
      (ms, []) -> Left $ "Missing constants: " : map show ms
      ([], es) -> Left $ "Extra constants: " : map show es
      (ms, es) -> Left $ "Why??" : ("Missing constants" : map show ms ++ "extra constants" : map show es)
    where
    anyMissing :: Eq a => [[a]] -> [a]
    anyMissing ms = foldl1 union ms

    allExtra :: Eq a => [[a]] -> [a]
    allExtra es = foldl1 intersect es

  missingConstants :: ConstantDefs -> Procedure Address -> ([String], [String])
  missingConstants defs proc = let
    templateNeeds = filter (isTemplate . snd) defs >>= toList >>= toList >>= unknowns
    needed = nub $ (blocks proc >>= toList >>= unknowns) ++ templateNeeds
    have = nub $ map fst defs

    in (needed \\ have, have \\ needed)
    where
    isTemplate (Template _) = True
    isTemplate _ = False

-- * Rendering output

-- | Print the hex for a module
render :: Alignment -> ModuleAssembly Absolutized -> Output
render a mod = let
  textS = zip [1..] (procs mod) >>= uncurry renderProc
  dataS = constants mod >>= \cons -> map (b0 <:>) $ constantToCell (snd cons)
  total = dataS ++ textS
  in case a of
    AlignLeft -> (replicate 15 (bitVector 0)) ++ total
    AlignRight -> replicate (1023 - length total) (bitVector 0) ++ total

  where

  renderProc ix proc = let
    procIx = bitVector ix :: BV 4
    in zip [1..] (blocks proc) >>= \(i, b) ->
      map (procIx <:> bitVector i <:> (b0 :: BitVector 9) <:>) (asmToCell b)
-- * Absolutization

-- | Given an alignment for a module, assign concrete addresses to everything.
absolutize :: Alignment -> ModuleAssembly Relativized -> ModuleAssembly Absolutized
absolutize align (Mod {..}) = let
  constants' = forgetNames cSize offset segmentOffsets constants
  cSize = sum (map (constantSize . snd) constants')
  (bSize, segmentOffsets) = buildOffsetMap procName (sum . map blockLen . blocks) procs
  offset = case align of
    AlignLeft  -> 0x10 + cSize
    AlignRight -> 1023 - bSize + 1
  in Mod
    { procs = map (absolveProc cSize offset segmentOffsets) procs
    , offsetMap = (M.map (absolve cSize offset segmentOffsets) <$> relativeMap )
    , constants = constants', ..
    }
  where
  absolveProc :: Int -> Int -> [(String, Int)] -> Procedure RelativeAddr -> Procedure Int
  absolveProc cSize offset textLens proc = proc
    { blocks = map (fmap (absolve cSize offset textLens)) (blocks proc)
    }

  forgetNames :: Int -> Int -> [(String, Int)] -> ConstantMap Relativized -> ConstantMap Absolutized
  forgetNames cSize offset segmentOffsets = map (fmap (fmap (absolve cSize offset segmentOffsets)))

  absolve _ offset textLens (Rel (Text p) i) = case p `lookup` textLens of
    Just off -> offset + off + i
  absolve c offset _ (Rel Data i) = offset - c + i
  absolve _ _ _    (Abs i)      = i

-- * Relativization

-- | A simplified address representation.
data RelativeAddr
  = Rel Section Int -- Constant and operator references
  | Abs Int -- For statically known addresses 'aka standard cells'
  deriving (Show, Eq)

-- | Relative addresses are either within a specific procedure or they refer to the data segment
data Section = Text String | Data
  deriving (Show, Eq)

{- |
  Drastically simplify the address represenation, converting everything
  to simple offsets from section heads.
-}
relativize :: ModuleAssembly LaidOut -> Either [String] (ModuleAssembly Relativized)
relativize (Mod{..}) = do
  let dict = mkRelativizationDict constants procs
  procs <- first concat . unzipEithers $ map (relativizeProc dict) procs
  constants <- relativizeConstants dict constants
  pure $ Mod{ relativeMap = Just dict, .. }

  where
  relativizeProc :: Map Address RelativeAddr -> Procedure Address -> Either [String] (Procedure RelativeAddr)
  relativizeProc dict proc = do
    relativized <- unzipEithers $ map (traverse (relativizeAddr dict)) (blocks proc)

    pure $ proc { blocks = relativized }

  relativizeConstants :: Map Address RelativeAddr -> ConstantMap LaidOut -> Either [String] (ConstantMap Relativized)
  relativizeConstants dict constants = unzipEithers $ map (
    \(key, val) -> (,) <$> pure key <*> traverse (relativizeAddr dict) val
    ) constants

  relativizeAddr ::  Map Address RelativeAddr -> Address -> Either String RelativeAddr
  relativizeAddr m p@(Procedure n a) = case p `M.lookup` m of
    Just relAddr -> Right relAddr
    Nothing -> Left $ "Missing " ++ show a ++ " for procedure " ++ n
  relativizeAddr m a@(Unknown _)  = case a `M.lookup` m of
    Just constant -> Right constant
    Nothing -> Left $ "Unknown constant " ++ show a
  relativizeAddr m r@(RTC a)        = case r `M.lookup` m of
    Just relAddr -> Right relAddr
    Nothing -> Left $ "could not find rtc for " ++ show a
  relativizeAddr m (Offset a o)   = relativizeAddr m a >>= \case
    Abs i -> Right $ Abs (i + o)
    Rel s i -> Right $ Rel s (i + o)
  relativizeAddr m (Absolute a)   = Right $ Abs a

-- | Build up a map giving the relative offset of every constant and block
mkRelativizationDict :: ConstantDefs -> [Procedure Address] -> Map Address RelativeAddr
mkRelativizationDict constants procs = let
  constantOffsets = dataOffsets constants
  bbOffsets = procs >>= \proc -> blockOffsets (procName proc) 0 (blocks proc)
  in M.fromList $ bbOffsets ++ constantOffsets

dataOffsets :: [(String, ConstantInfo Address)] -> [(Address, RelativeAddr)]
dataOffsets = map (fmap (Rel Data)) . snd . buildOffsetMap (Unknown . fst) (constantSize . snd)

-- | For a procedure, and a starting offset, give the relative address of every operator
blockOffsets :: String -> Int -> [BB Address] -> [(Address, RelativeAddr)]
blockOffsets nm off (bb : blks) = let
  rest = blockOffsets nm (off + blockLen bb) blks
  entry = [(baseAddress bb, Rel (Text nm) off)]
  rtcEntry = case terminator bb of
    RetRTC _ -> [ ((RTC $ baseAddress bb), Rel (Text nm) (off + blockLen bb - 1))]
    _ -> []
  in entry ++ rtcEntry ++ rest
blockOffsets _ _ [] = []

-- * Internalization

{- $internalize
  Wrap every address with the corresponding procedure, making them unambiguous when mixed
  with other procedures.
-}
internalizeModule :: ModuleAssembly LaidOut -> ModuleAssembly LaidOut
internalizeModule mod = mod
  { procs = map internalizeAddresses (procs mod)
  }

internalizeAddresses :: Procedure Address -> Procedure Address
internalizeAddresses proc =
  proc { blocks = map (fmap internalizeAddress) (blocks proc) }
  where
  internalizeAddress (Operator n) = Procedure (procName proc) (Operator n)
  internalizeAddress (RTC a) = RTC $ internalizeAddress a
  internalizeAddress (Offset a o) = Offset (internalizeAddress a) o
  internalizeAddress i = i

-- * Utilities

unzipEithers :: [Either a b] -> Either [a] [b]
unzipEithers es = case partitionEithers es of
  ([], e) -> Right e
  (a, _)  -> Left a

buildOffsetMap :: (a -> nm) -> (a -> Int) -> [a] -> (Int, [(nm, Int)])
buildOffsetMap key size elems = mapAccumL (\off elem ->
  (off + size elem, (key elem, off))
  ) 0 elems

-- * Layout
{- $layout
  The layout step performs one of the most important optimizations in the assembler.

  It decides how to breakup the CFG into linear sequences of instructions in such a way
  that minimizes explicit jumps between instructions.

  The overall idea is to build up a 'segment map' which shows the blocks reachable
  from each segment head. The @segment@ function extract a single segment by performing
  a DFS of the CFG continuing until it hits an already segmented block.

  Once all blocks have been segmented, @addJump@ reifies any 'implicit' jumps.
-}
{- |
  Break up the CFG into a series of linear chunks, add explicit jumps between segments
  and merge it back together in a linearized CFG.
-}
layout :: ModuleAssembly Input -> ModuleAssembly LaidOut
layout (Mod {..}) = Mod
  { procs = map segmentProcedure procs
  , constants = layoutConstants constants
  , ..
  }

  where

  segmentProcedure proc = proc
    { blocks = join $ map (addJump . fromSegment) . M.elems $ go (toUnsegmentedMap (blocks proc))
    }

  fromSegment (Segmented x) = x

  go map = go' map (M.keys map)

  go' map (k : eys) = go' (segment map k) eys
  go' map [] = map

layoutConstants :: ConstantMap Input -> ConstantMap Input
layoutConstants constants = let
  (cells, rem)       = partition (isCell . snd) constants
  (blocks, rem')     = partition (isSize . snd) rem
  (templates, rem'') = partition (isTemplate . snd) rem'
  in blocks ++ templates ++ cells ++ (sortOn fst rem'')

  where

  isCell Cell = True
  isCell _ = False

  isSize (Size _) = True
  isSize _ = False

  isTemplate (Template _) = True
  isTemplate _ = False

{- | Segments are non-empty, ordered sets of basic blocks where control flow goes
     linear from start to end. The @Chain@ terminators in those blocks will be eliminated
     as well as the second target of comparison instructions.
-}
data Segment = Segmented [BB Address] | Unsegmented (BB Address)

toUnsegmentedMap :: [BB Address] -> Map Address (Segment)
toUnsegmentedMap bbs = M.fromList $ L.map (\bb -> (baseAddress bb, Unsegmented bb)) bbs

{-
  Extract a linear segment from the block map.
-}
segment :: Map Address Segment -> Address -> Map Address Segment
segment map addr = case addr `M.lookup` map of
  Just (Unsegmented bb) -> fromMaybe (M.insert addr (Segmented [bb]) map) $ do
    tar <- implicitJumps (terminator bb)
    let map' = segment (M.delete addr map) tar

    tar `M.lookup` map' >>= \case
      Segmented seg -> pure $ M.insert addr (Segmented $ bb : seg) (M.delete tar map')
      _ -> pure $ map
  _ -> map

{- |
  Reify any implicit jumps at the end of a segment. This will convert a @Chain@ into
  a @CCCC@ and will add a @CCCC@ after any comparison.
-}
addJump :: [BB Address] -> [BB Address]
addJump [bb] = case implicitJumps (terminator bb) of
  Just iJ -> let
    bb' = case terminator bb of
      Comp     l r b i -> bb { terminator = Comp l r b (baseAddress jB)}
      CompMod  l r b i -> bb { terminator = CompMod l r b (baseAddress jB)}
      CompWord l r b i -> bb { terminator = CompWord l r b (baseAddress jB)}
      _ -> bb { terminator = Chain (baseAddress jB) } -- add hard jump
    jB = jumpBlk bb iJ
    in [bb', jB]
  Nothing -> [bb]
  where
  jumpBlk fromB to = BB [] (CCCC to) (baseAddress fromB `offAddr` (blockLen fromB + 1))
addJump (bb : bbs) = bb : addJump bbs
addJump [] = []

-- | Direct jumps are jumps that an instruction must always perform. These jumps can't be optimized away
directJumps :: Term a -> Maybe a
directJumps (Comp      _ _ a _) = Just a
directJumps (CompWord  _ _ a _) = Just a
directJumps (CompMod   _ _ a _) = Just a
directJumps (CCCC          _)   = Nothing
directJumps (CCCCSnd     _ a)   = Just a
directJumps (Stop)              = Nothing
directJumps (SwitchStop)        = Nothing
directJumps (Chain     _)       = Nothing
directJumps (RetRTC _)          = Nothing

-- | These are the jumps that could potentially be optimized away.
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

{-# LANGUAGE DataKinds, BinaryLiterals, DeriveFunctor, LambdaCase #-}
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

data Section = Text String | Data
  deriving (Show, Eq)

data Alignment = AlignLeft | AlignRight

data ModuleAssembly a = Mod
  { offsetMap :: Maybe (Map Address Int)
  , relativeMap :: Maybe (Map Address RelativeAddr)
  , procs :: [Procedure a]
  } deriving (Show)

debugAssemble defs a =
  pure . Mod Nothing Nothing
  >=> pure . segmentize
  >=> pure . internalizeModule
  >=> resolve    defs
  >=> pure . absolutize defs a
  >=> pure . debugRender
  where

  debugRender :: ModuleAssembly Int -> ([(Int, String)], ModuleAssembly Int)
  debugRender mod = let
    (o, dataS) = mapAccumL (\off (s, v) -> (off + constantSize v, (off, s ++ " " ++ show v))) 0 defs
    blks  = procs mod >>= blocks
    text = blks >>= (\bb -> map show (instrs bb) ++ termToString (terminator bb))
    textS = zip [o..] text
    len = o + length textS
    in case a of
      AlignLeft ->  (map (\(i, s) -> (16 + i, s)) $ dataS ++ textS, mod)
      AlignRight -> (map (\(i, s) -> (1024 - len + i , s)) $ dataS ++ textS, mod)

  termToString :: Term Int -> [String]
  termToString (RetRTC a) = [show $ AI 0b10100001111 (a+1) (a+1), "zero"]
  termToString (Chain     _) = []
  termToString c = [show c]

prop1 :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
prop1 a b = a >=> b

assemble :: ConstantDefs -> Alignment -> [Procedure Address] -> Either [String] Output
assemble defs a =
  pure . Mod Nothing Nothing
  >=> checkForMissingConstantDefs defs
  >=> pure . segmentize
  >=> pure . internalizeModule
  >=> resolve    defs
  >=> pure . absolutize defs a
  >=> pure . render     defs a

  where

  checkForMissingConstantDefs defs mod = let
    missing = unzip $ map (missingConstants defs) (procs mod)
    in case missing of
      (ms, es) | anyMissing ms == [] && allExtra es == [] -> Right mod
      (ms, es) | allExtra es == [] -> Left $ "Missing constants: " : map show ms
      (_, es) -> Left $ "Extra constants: " : map show es
      (ms, es) -> Left $ "Why??" : ("Missing constants" : map show ms ++ "extra constants" : [show (allExtra es)])
    where
    anyMissing :: Eq a => [[a]] -> [a]
    anyMissing ms = foldl1 union ms

    allExtra :: Eq a => [[a]] -> [a]
    allExtra es = foldl1 intersect es

missingConstants :: ConstantDefs -> Procedure Address -> ([String], [String])
missingConstants defs proc = let
  needed = nub $ (blocks proc) >>= toList >>= unknowns
  have = nub $ map fst defs

  in (needed \\ have, have \\ needed)

render :: ConstantDefs -> Alignment -> ModuleAssembly Int -> Output
render defs a mod = let
  blks  = procs mod >>= blocks
  textS = blks >>= asmToCell
  dataS = defs >>= (constantToCell . snd)
  total = dataS ++ textS
  in case a of
    AlignLeft -> (replicate 15 (bitVector 0)) ++ total
    AlignRight -> replicate (1023 - length total) (bitVector 0) ++ total

constantToCell :: ConstantInfo -> [BitVector 39]
constantToCell (Size i) = replicate i (bitVector 0)
constantToCell (Val  i) = [numberToBesmFloating i]
constantToCell (Raw  i) = [bitVector $ fromIntegral i]
constantToCell (Addr _) = [bitVector 0] -- FINISH LATER

absolutize :: ConstantDefs -> Alignment -> ModuleAssembly RelativeAddr -> ModuleAssembly Int
absolutize defs align mod = let
  cSize = sum (map (constantSize . snd) defs)
  (bSize, segmentOffsets) = buildOffsetMap (procName) (sum . (map instLen) . blocks) (procs mod)
  offset = case align of
    AlignLeft  -> 0x10 + cSize
    AlignRight -> 1023 - bSize + 1
  in mod
    { procs = map (absolveProc cSize offset segmentOffsets) (procs mod)
    , offsetMap = (M.map (absolve cSize offset segmentOffsets) <$> (relativeMap mod))
    }
  where
  absolveProc :: Int -> Int -> [(String, Int)] -> Procedure RelativeAddr -> Procedure Int
  absolveProc cSize offset textLens proc = proc
    { blocks = map (fmap (absolve cSize offset textLens)) (blocks proc)
    }

  absolve _ offset textLens (Rel (Text p) i) = case p `lookup` textLens of
    Just off -> offset + off + i
  absolve c offset textLEns (Rel Data i) = offset - c + i
  absolve _ _ _    (Abs i)      = i

resolve :: ConstantDefs -> ModuleAssembly Address -> Either [String] (ModuleAssembly RelativeAddr)
resolve conses mod = let
  dict = mkRelativizationDict conses (procs mod)
  resolved = first concat . unzipEithers $ map (relativizeProc dict) (procs mod)
  in resolved >>= \procs' -> pure $ mod
    { procs = procs'
    , relativeMap = Just dict
    }
  where
  relativizeProc :: Map Address RelativeAddr -> Procedure Address -> Either [String] (Procedure RelativeAddr)
  relativizeProc dict proc = let
    relativized = unzipEithers $ map (traverse (relativize (procName proc) dict)) (blocks proc)
    in relativized >>= \rel -> pure $ proc
      { blocks = rel
      }

unzipEithers :: [Either a b] -> Either [a] [b]
unzipEithers es = case partitionEithers es of
  ([], e) -> Right e
  (a, _)  -> Left a

mkRelativizationDict :: ConstantDefs -> [Procedure Address] -> Map Address RelativeAddr
mkRelativizationDict consts procs = let
  constantOffsets = map (fmap (Rel Data)) . snd $ buildOffsetMap (Unknown . fst) (constantSize . snd) consts
  bbOffsets = procs >>= \proc -> blockOffsets (procName proc) 0 (blocks proc)
  in M.fromList $ bbOffsets ++ constantOffsets

blockOffsets :: String -> Int -> [BB Address] -> [(Address, RelativeAddr)]
blockOffsets nm off (bb : blks) = let
  rest = blockOffsets nm (off + instLen bb) blks
  entry = case baseAddress bb of
    (o             ) ->
      [ (o, Rel (Text nm) off)
      ]
  rtcEntry = case terminator bb of
    RetRTC _ ->
      [ ((RTC $ baseAddress bb), Rel (Text nm) (off + instLen bb - 1))
      ]
    _ -> []
  in entry ++ rtcEntry ++ rest
blockOffsets _ _ [] = []

relativize :: String -> Map Address RelativeAddr -> Address -> Either String RelativeAddr
relativize nm m p@(Procedure n a) = case p `M.lookup` m of
  Just relAddr -> Right relAddr
  Nothing -> case a of
    _ -> Right $ Abs 0x9999
    (Procedure _ _) -> Left $ "Wtf " ++ show a ++ show p
    (Operator o) -> Left $ "Missing operator offset for " ++ show p
    (RTC r) -> Left $ "Missing RTC return for " ++ show r ++ show p
relativize nm m a@(Unknown _)  = case a `M.lookup` m of
  Just constant -> Right constant
  Nothing -> Left $ "Unknown constant " ++ show a
relativize nm m a@(Operator _) = relativize nm m (Procedure nm a)
relativize nm m r@(RTC a)        = case r `M.lookup` m of
  Just relAddr -> Right relAddr
  Nothing -> Left $ "could not find rtc for " ++ show a
relativize nm m (Offset a o)   = relativize nm m a >>= \case
  Abs i -> Right $ Abs (i + o)
  Rel s i -> Right $ Rel s (i + o)
relativize nm m (Absolute a)   = Right $ Abs a

toUnsegmentedMap :: [BB Address] -> Map Address (Segment)
toUnsegmentedMap bbs = M.fromList $ L.map (\bb -> (baseAddress bb, Unsegmented bb)) bbs

internalizeModule mod = mod {
  procs = map internalizeAddresses (procs mod)
  }

internalizeAddresses :: Procedure Address -> Procedure Address
internalizeAddresses proc =
  proc { blocks = map (fmap internalizeAddress) (blocks proc) }
  where
  internalizeAddress (Operator n) = Procedure (procName proc) (Operator n)
  internalizeAddress (RTC a) = RTC $ internalizeAddress a
  internalizeAddress (Offset a o) = Offset (internalizeAddress a) o
  internalizeAddress i = i
{-
  Break up the CFG into a series of linear chunks, add explicit jumps between segments
  and merge it back together in a linearized CF
-}
segmentize :: ModuleAssembly Address -> ModuleAssembly Address
segmentize mod = mod
  { procs = map segmentProcedure (procs mod)
  }

  where

  segmentProcedure proc = proc
    { blocks = join $ map (addJump . fromSegment) . M.elems $ go (toUnsegmentedMap (blocks proc))
    }
  fromSegment (Segmented x) = x

  go map = go' map (M.keys map)

  go' map (k : eys) = go' (segment map k) eys
  go' map [] = map

{-
  Extract a linear segment from the block map.
-}

data Segment = Segmented [BB Address] | Unsegmented (BB Address)

segment :: Map Address (Segment) -> Address -> Map Address (Segment)
segment map addr = case addr `M.lookup` map of
  Just (Unsegmented bb) -> case (implicitJumps (terminator bb)) of
    Just tar -> let
      map' = segment (M.delete addr map) tar
      in case tar `M.lookup` map' of
        Just (Segmented seg) -> M.insert addr (Segmented $ bb : seg) (M.delete tar map')
        Nothing -> M.insert addr (Segmented [bb]) map
        _ -> map
    Nothing  -> (M.insert addr (Segmented [bb]) map)
  _ -> map

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
  jumpBlk fromB to = BB [] (CCCC to) (baseAddress fromB `offAddr` (instLen fromB + 1))
addJump (bb : bbs) = bb : addJump bbs
addJump [] = []

buildOffsetMap :: (a -> nm) -> (a -> Int) -> [a] -> (Int, [(nm, Int)])
buildOffsetMap key size elems = mapAccumL (\off elem ->
  (off + size elem, (key elem, off))
  ) 0 elems

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

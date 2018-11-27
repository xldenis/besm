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

type Output = [BitVector 64]

{-

  1. Linearize blocks and insert explicit jumps
  2. Resolve constants and assign relative addresses in instructions
  3. Give absolute addresses
-}

data Alignment = AlignLeft | AlignRight
  deriving (Show)

data Stage = Input | LaidOut | Relativized | Absolutized

-- Back on my type level bullshit
type family AddressType (s :: Stage) where
  AddressType Relativized = RelativeAddr
  AddressType Absolutized = Int
  AddressType a = Address

type family GlobalMap (s :: Stage) where
  GlobalMap Absolutized = [(String, Constant Int)]
  GlobalMap Relativized = [(String, Constant RelativeAddr)]
  GlobalMap a = [(String, Constant Address)]

type family OffsetMap (s :: Stage) where
  OffsetMap Absolutized = Map Address Int
  OffsetMap a = ()

type family RelativeMap (s :: Stage) where
  RelativeMap Relativized = Map Address RelativeAddr
  RelativeMap Absolutized = Map Address RelativeAddr
  RelativeMap a = ()

-- Please don't hate me
data ModuleAssembly (stage :: Stage) = Mod
  { offsetMap   :: OffsetMap stage
  , relativeMap :: RelativeMap stage
  , globals     :: GlobalMap stage
  , alignment   :: Alignment
  , procs       :: [Procedure (AddressType stage)]
  }

deriving instance Show (ModuleAssembly 'Absolutized)

assemble :: Alignment -> [Procedure Address] -> Either [String] Output
assemble a = compile a >=> pure . render

compile :: Alignment -> [Procedure Address] -> Either [String] (ModuleAssembly 'Absolutized)
compile align =
  pure . Mod () () [] align
  >=> validateModule
  >=> pure . layout
  >=> pure . internalizeModule
  >=> relativize
  >=> pure . absolutize

  where

-- * Validating input

validateModule :: ModuleAssembly 'Input -> Either [String] (ModuleAssembly 'Input)
validateModule mod = let
  procs' = unzipEithers $ map checkConstants (procs mod)
  in first concat procs' >> checkExternsDefined mod

checkConstants :: Procedure Address -> Either [String] (Procedure Address)
checkConstants proc = let
  templateNeeds = constDefs proc >>= templateArgs
  needed  = nub $ (blocks proc >>= toList >>= unknowns) ++ templateNeeds
  have    = nub $ map (constantName) (constDefs proc)
  extra   = have \\ needed
  missing = needed \\ have

  in if null extra && null missing
  then Right proc
  else Left $ map (\e -> "Missing constant in " ++ procName proc ++ " : " ++ show e) missing
           ++ map (\e -> "Extra constants in " ++ procName proc ++ " : "  ++ show e) extra
  where

  templateArgs :: ConstantDef Address -> [String]
  templateArgs (Def _ _ c) | isTemplate c = toList c >>= unknowns
  templateArgs _ = []

  isTemplate (Template _) = True
  isTemplate _ = False

checkExternsDefined :: ModuleAssembly 'Input -> Either [String] (ModuleAssembly 'Input)
checkExternsDefined mod = let
  externs = nub $ catMaybes $ map externName (procs mod >>= constDefs)
  globals = nub $ catMaybes $ map globalName (procs mod >>= constDefs)
  in case externs \\ globals of
    [] ->   Right mod
    nms -> Left $ map ("Missing global constant definition: " ++) nms

  where
  globalName :: ConstantDef a -> Maybe String
  globalName (Def Global nm _) = Just nm
  globalName _ = Nothing

  externName :: ConstantDef a -> Maybe String
  externName (Extern nm) = Just nm
  externName _ = Nothing

-- * Rendering output

debugRender :: ModuleAssembly Absolutized -> IO ()
debugRender mod = let
  (o, dataS) = mapAccumL (\off (s, v) -> showConstant off s v) 0 (globals mod)
  (len,textS) = mapAccumL (\off p -> debugProc off p) o (procs mod)
  offset = case (alignment mod) of
            AlignLeft -> 0
            AlignRight -> 1024 - len
  in forM_ (concat $ dataS ++ textS) $ \(addr, inst) -> putStrLn $ show (addr + offset) ++ ": " ++ inst
  where

  showConstant :: Int -> String -> Constant Int -> (Int, [(Int, String)])
  showConstant off str (Table cs) = let
    (off', out) = mapAccumL (\off c -> showConstant off str c) off cs
    in (off', concat out)
  showConstant off str cons = (off + constantSize cons, [(off, str ++ " " ++ show cons)])

  debugProc ix proc = let
    (off, consts) = mapAccumL (\off (Def _ s v) -> showConstant off s v) ix (constDefs proc)
    textS = zip [off..] $ blocks proc >>= renderBlock
    (len, _) = last textS
    in (len, concat consts ++ textS)

  renderBlock :: BB Int -> [String]
  renderBlock bb = map show (instrs bb) ++ termToString (terminator bb)

debugOffsets :: ModuleAssembly Absolutized -> IO ()
debugOffsets mod = void $ printMap (offsetMap mod)
  where
  printMap = mapM_ (\(k, v) -> putStrLn $ show v ++ " " ++ formatAddr k ) . M.toList
  -- where

renderSourceMap :: ModuleAssembly Absolutized -> String
renderSourceMap mod = let
  flippedList =  M.toAscList $ offsetMap mod
  in unlines $ map (\(v, k) -> show k ++ " " ++ formatAddr v) flippedList


termToString :: Term Int -> [String]
termToString (RetRTC a) = [show $ AI 0b10100001111 (a+1) (a+1), "zero"]
termToString (Chain     _) = []
termToString c = [show c]

-- | Print the hex for a module
render :: ModuleAssembly Absolutized -> Output
render mod = let
  textS = zip [1..] (procs mod) >>= uncurry renderProc
  dataS = globals mod >>= \cons -> map (b0 <:>) $ constantToCell (snd cons)
  total = dataS ++ textS
  in case (alignment mod) of
    AlignLeft -> (replicate 0 (bitVector 0)) ++ total
    AlignRight -> replicate (1023 - length total) (bitVector 0) ++ total

renderProc :: Integer -> Procedure Int -> Output
renderProc ix proc = let
  procIx = bitVector ix :: BV 4
  in
    concatMap renderLocal (constDefs proc) ++
    (zip [1..] (blocks proc) >>= \(i, b) -> renderBlock procIx i b)

  where

  renderLocal c = map (b0 <:>) (constantToCell $ fromDef c)

  renderBlock procIx i b = map (procIx <:> bv i <:> (b0 :: BV 9) <:>) (asmToCell b)

  fromDef (Def _ _ c) = c
-- * Absolutization

-- | Given an alignment for a module, assign concrete addresses to everything.
absolutize :: ModuleAssembly Relativized -> ModuleAssembly Absolutized
absolutize (Mod {..}) = let
  globals' = forgetNames cSize offset segmentOffsets globals
  cSize = sum (map (constantSize . snd) globals')
  (bSize, segmentOffsets) = buildOffsetMap procName (sum . map blockLen . blocks) procs
  offset = case alignment of
    AlignLeft  -> cSize
    AlignRight -> 1023 - bSize + 1
  in Mod
    { procs = map (absolveProc cSize offset segmentOffsets) procs
    , offsetMap = (M.map (absolve cSize offset segmentOffsets) relativeMap )
    , globals = globals', ..
    }
  where
  absolveProc :: Int -> Int -> [(String, Int)] -> Procedure RelativeAddr -> Procedure Int
  absolveProc cSize offset textLens proc = proc
    { blocks = map (fmap (absolve cSize offset textLens)) (blocks proc)
    , constDefs = map (fmap (absolve cSize offset textLens)) (constDefs proc)
    }

  forgetNames :: Int -> Int -> [(String, Int)] -> GlobalMap Relativized -> GlobalMap Absolutized
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
  let dict = mkRelativizationDict globals procs
  procs <- first concat . unzipEithers $ map (relativizeProc dict) procs
  globals <- relativizeConstants dict globals
  pure $ Mod{ relativeMap = dict, .. }

  where
  relativizeProc :: Map Address RelativeAddr -> Procedure Address -> Either [String] (Procedure RelativeAddr)
  relativizeProc dict proc = do
    relativized <- unzipEithers $ map (traverse (relativizeAddr dict)) (blocks proc)

    constDefs <- unzipEithers $ map (traverse (relativizeAddr dict)) (constDefs proc)
    pure $ proc
      { blocks = relativized
      , constDefs = constDefs
      }

  relativizeConstants :: Map Address RelativeAddr -> GlobalMap LaidOut -> Either [String] (GlobalMap Relativized)
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
mkRelativizationDict :: GlobalMap Input -> [Procedure Address] -> Map Address RelativeAddr
mkRelativizationDict constants procs = let
  constantOffsets = dataOffsets constants
  bbOffsets = procs >>= \proc -> let
    (o, constOffset) = buildOffsetMap (Unknown . constantName) (constantSize . fromDef) (constDefs proc)
    nm = procName proc
    in (map (fmap (Rel (Text nm))) constOffset) ++ blockOffsets nm o (blocks proc)
  in M.fromList $ bbOffsets ++ constantOffsets

dataOffsets :: GlobalMap Input -> [(Address, RelativeAddr)]
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
  internalizeAddress (Block a) = Procedure (procName proc) (Block a)
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
  , globals = procs >>= globalDefs
  , ..
  }

  where

  globalDefs proc = catMaybes . map isGlobal $ constDefs proc

  isGlobal (Def Global nm c) = Just (nm, c)
  isGlobal _ = Nothing

  segmentProcedure proc = proc
    { blocks = join $ map (addJump . fromSegment) . M.elems $ go (toUnsegmentedMap (blocks proc))
    , constDefs = layoutConstants' (constDefs proc)
    }

  fromSegment (Segmented x) = x

  go map = go' map (M.keys map)

  go' map (k : eys) = go' (segment map k) eys
  go' map [] = map


layoutConstants' :: [ConstantDef a] -> [ConstantDef a]
layoutConstants' constants = let
  (_, locals)        = partition (isGlobal) constants
  (cells, rem)       = partition (isCell . def) locals
  (blocks, rem')     = partition (isSize . def) rem
  (templates, rem'') = partition (isTemplate . def) rem'
  (_, rem''')        = partition (isExtern) rem''
  in cells ++ blocks ++ templates ++ (sortOn constantName rem''')

  where

  isGlobal (Def Global _ _) = True
  isGlobal _ = False

  def (Def _ _ c) = Just c
  def _ = Nothing

  isExtern (Extern _) = True
  isExtern _ = False

  isCell (Just Cell) = True
  isCell _ = False

  isSize (Just (Size _)) = True
  isSize _ = False

  isTemplate (Just (Template _)) = True
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

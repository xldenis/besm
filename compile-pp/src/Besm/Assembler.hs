{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# LANGUAGE TypeFamilies #-}

module Besm.Assembler where

import Besm.Assembler.Syntax

import Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Data.Foldable
import Data.List as L
import Data.Maybe

import Control.Category ((>>>))

import Data.BitVector.Sized (BV)
import qualified Data.BitVector.Sized as BV

import Besm.Put

import Data.Bifunctor (first)
import Data.Either (partitionEithers)

import Data.Function

import Debug.Trace

type Output = [BV 64]

{-

  1. Linearize blocks and insert explicit jumps
  2. Resolve constants and assign relative addresses in instructions
  3. Give absolute addresses

  Layout of Programs in Memory

  +--------------------------------------------------+
  |                 |         |         |            |
  | Pinned Sections | Padding | Globals | Procedures |
  |                 |         |         |            |
  +--------------------------------------------------+
  ^                                                  ^
  1                                                 1023

-}

data Stage = Input | LaidOut | Relativized | Absolutized

-- Back on my type level bullshit
type family AddressType (s :: Stage) where
  AddressType Relativized = RelativeAddr
  AddressType Absolutized = Int
  AddressType a = Address

type GlobalMap (s :: Stage) = [(String, Section, Constant (AddressType s))]

type family OffsetMap (s :: Stage) where
  OffsetMap Absolutized = Map Address Int
  OffsetMap a = ()

type family RelativeMap (s :: Stage) where
  RelativeMap Relativized = Map Address RelativeAddr
  RelativeMap Absolutized = Map Address RelativeAddr
  RelativeMap a = ()

-- Please don't hate me
data ModuleAssembly (stage :: Stage) = Mod
  { offsetMap :: OffsetMap stage
  , relativeMap :: RelativeMap stage
  , globals :: GlobalMap stage
  , segments :: [LayoutSegment]
  , procs :: [Procedure (AddressType stage)]
  , packSize :: Bool
  }

deriving instance Show (ModuleAssembly 'Absolutized)

data LayoutSegment = MkSeg {segProcs :: [String]}
  deriving (Show, Eq)

simpleModule :: [Procedure Address] -> ModuleAssembly 'Input
simpleModule procs = Mod () () [] segs procs False
 where
  segs = map (MkSeg . pure . procName) procs

assemble :: Integer -> ModuleAssembly 'Input -> Either [String] Output
assemble i = compile >=> pure . (render i)

compile :: ModuleAssembly 'Input -> Either [String] (ModuleAssembly 'Absolutized)
compile =
  validateModule
    >=> pure . layout
    >=> pure . internalizeModule
    >=> relativize
    >=> pure . absolutize

debugComp =
  validateModule
    >=> pure . layout
    >=> pure . internalizeModule
    >=> relativize
-- * Validating input

validateModule :: ModuleAssembly 'Input -> Either [String] (ModuleAssembly 'Input)
validateModule mod =
  let
    procs' = unzipEithers $ map checkConstants (procs mod)
   in
    first concat procs' >> checkExternsDefined mod

checkConstants :: Procedure Address -> Either [String] (Procedure Address)
checkConstants proc =
  let
    templateNeeds = constDefs proc >>= templateArgs
    needed = nub $ (blocks proc >>= toList >>= unknowns) ++ templateNeeds
    have = nub $ map constantName (constDefs proc)
    globals = mapMaybe globalName (constDefs proc)
    extra = (have \\ globals) \\ needed
    missing = needed \\ have
   in
    if null extra && null missing
      then Right proc
      else
        Left $
          map (\e -> "Missing constant in " ++ procName proc ++ ": " ++ redString e) missing
            ++ map (\e -> "Extra constants in " ++ procName proc ++ ": " ++ e) extra

checkExternsDefined :: ModuleAssembly 'Input -> Either [String] (ModuleAssembly 'Input)
checkExternsDefined mod =
  let
    externs = nub $ mapMaybe externName (procs mod >>= constDefs)
    globals = nub $ mapMaybe globalName (procs mod >>= constDefs)
   in
    case externs \\ globals of
      [] -> Right mod
      nms -> Left $ map (\s -> "Missing global constant definition: " ++ redString s) nms

-- * Rendering output

debugRender :: forall a. Show (AddressType a) => ModuleAssembly a -> IO ()
debugRender mod =
  let
    (pinned, def) = partition (\(_, s, _) -> s /= DefaultData) (globals mod)
    (o, pinnedS) = mapAccumL (\off (s, _, v) -> showConstant off s v) 1 pinned
    (o', dataS) = mapAccumL (\off (s, _, v) -> showConstant off s v) (o - 1) def
    (len, textS) = mapAccumL (\off p -> debugProc (off + 1) p) (o' - 1) (procs mod)
    padding = 1023 - len
   in
    do
      forM_ (concat pinnedS) $ \(addr, inst) -> putStrLn $ show addr ++ ": " ++ inst
      forM_ (concat dataS) $ \(addr, inst) -> putStrLn $ show (addr + padding) ++ ": " ++ inst
      forM_ (concat textS) $ \(addr, inst) -> putStrLn $ show (addr + padding) ++ ": " ++ inst
 where
  showConstant :: Int -> String -> Constant (AddressType a) -> (Int, [(Int, String)])
  showConstant off str (Table cs) =
    let
      (off', out) = mapAccumL (`showConstant` str) off cs
     in
      (off', concat out)
  showConstant off str cons = (off + constantSize cons, [(off, str ++ " " ++ show cons)])

  debugProc :: Int -> Procedure (AddressType a) -> (Int, [(Int, String)])
  debugProc ix proc =
    let
      (off, consts) = mapAccumL (\off (Def _ s v) -> showConstant off s v) ix (constDefs proc)
      textS = zip [off ..] $ blocks proc >>= renderBlock
      (len, _) = last textS
     in
      (len, concat consts ++ textS)

  renderBlock :: BB (AddressType a) -> [String]
  renderBlock bb = map show (instrs bb) ++ termToString (terminator bb)

  termToString :: Term (AddressType a) -> [String]
  termToString (RetRTC a) = ["ret rtc", "zero"] -- [show $ AI 0b10100001111 (a+1) (a+1), "zero"]
  termToString (Chain _) = []
  termToString c = [show c]

debugOffsets :: ModuleAssembly Absolutized -> IO ()
debugOffsets mod = void $ printMap (offsetMap mod)
 where
  printMap = mapM_ (\(k, v) -> putStrLn $ show v ++ " " ++ formatAddr k) . M.toList

-- where

debugRelative :: ModuleAssembly Absolutized -> IO ()
debugRelative mod = void $ printMap (relativeMap mod)
 where
  printMap = mapM_ (\(k, v) -> putStrLn $ printRel v ++ " " ++ formatAddr k) . M.toList

  printRel (Rel s o) = show s <> " + " <> show o
  printRel (Abs i) = show i

renderSourceMap :: ModuleAssembly Absolutized -> String
renderSourceMap mod =
  let
    flippedList = M.toAscList $ offsetMap mod
   in
    unlines $ map (\(v, k) -> show k ++ " " ++ formatAddr v) flippedList

-- | Print the hex for a module
-- Pass number: 0 for PP1, 1 for PP2, 2 for PP3
{- |
  Generate the final disk image as a sequence of 64-bit words.

  Layout: [pinned sections] [padding] [default data] [procedures]

  When packSize is enabled, Size constants are omitted from the output, reducing the
  disk image size. The bootloader compensates for this by using packedCells offsets
  when loading data into memory.
-}
render :: Integer -> ModuleAssembly Absolutized -> Output
render passNum mod =
  let
    textS = zip [0 ..] (procs mod) >>= uncurry (renderProc passNum)
    dataS = globals mod >>= \(_, s, c) -> map (\c -> (s, b0 <:> c)) $ renderGlobal c
    total = length dataS + length textS
    padding = replicate (1023 - total) (bitVector 0)
    (pinned, def) = partition (\(s, _) -> s /= DefaultData) dataS
   in
    map snd pinned ++ padding ++ map snd def ++ textS
 where
  renderGlobal (Size _) | (packSize mod) = []
  renderGlobal a = constantToCell a

renderProc :: Integer -> Integer -> Procedure Int -> Output
renderProc passNum ix proc =
  let
    passIx = bitVector passNum :: BV 2
    procIx = bitVector ix :: BV 4
   in
    concatMap renderLocal (constDefs proc)
      ++ (zip [1..] (blocks proc) >>= uncurry (renderBlock passIx procIx))
 where
  renderLocal c = map (b0 <:>) (constantToCell $ fromDef c)

  renderBlock passIx procIx i b = map (passIx <:> procIx <:> (bitVector i :: BV 10) <:> (b0 :: BV 9) <:>) (asmToCell b)

  fromDef (Def _ _ c) = c

modInfo :: ModuleAssembly 'Absolutized -> IO ()
modInfo mod = void $ do
  globalInfo (globals mod)
  mapM_ procInfo (procs mod)

  print "Memory Layout"
  print $ memoryLayout mod
  print "Disk Layout"
  print $ diskLayout mod
 where
  procInfo proc = putStrLn $ "proc: " <> procName proc <> " " <> show (procedureLen proc)
  globalInfo globals = putStrLn $ "globals: " <> show (sum $ map (\(_, _, c) -> constantSize c) globals)
-- * Absolutization

{- |
  Memory or disk layout information for a module.

  size: Total cells used (excluding padding)
  offsets: Absolute addresses for each section
  padding: Available space after layout. Negative values indicate the module
           exceeds capacity and cannot be assembled.
  packedCells: For each section, the number of cells saved by packing Size constants.
               Used by bootloader to calculate correct load addresses.
-}
data MemoryLayout = MkLayout
  { size :: Int
  , offsets :: [(Section, Int)]
  , padding :: Int
  , packedCells :: [(Section, Int)]
  }
  deriving (Show, Eq)

-- | Given an alignment for a module, assign concrete addresses to everything.
absolutize :: ModuleAssembly Relativized -> ModuleAssembly Absolutized
absolutize mod@Mod{..} =
  let
    memory = memoryLayout mod
    disk = diskLayout mod

    globals' = forgetNames memory globals
   in
    Mod
      { procs = map (absolveProc memory disk) procs
      , offsetMap = M.map (absolve memory) relativeMap
      , globals = globals'
      , ..
      }
 where
  absolveProc :: MemoryLayout -> MemoryLayout -> Procedure RelativeAddr -> Procedure Int
  absolveProc mem disk proc =
    proc
      { blocks = map (absoluteBlock mem disk) (blocks proc)
      , constDefs = map (fmap (absolve mem)) (constDefs proc)
      }

  forgetNames :: MemoryLayout -> GlobalMap Relativized -> GlobalMap Absolutized
  forgetNames segmentOffsets = map (\(n, s, c) -> (n, s, fmap (absolve segmentOffsets) c))

  absolve :: MemoryLayout -> RelativeAddr -> Int
  absolve textLens (Rel sec i) = case sec `lookup` offsets textLens of
    Just off -> off + i
    Nothing -> error $ show sec
  absolve _ (Abs i) = i

  absoluteBlock :: MemoryLayout -> MemoryLayout -> BB RelativeAddr -> BB Int
  absoluteBlock mem disk (BB i t b) =
    BB
      (map (absoluteInstr mem disk) i)
      (fmap (absolve mem) t)
      (absolve mem b)

  absoluteInstr mem disk (Ma o a s) = Ma (absolve mem o) (absolve disk a) (absolve mem s)
  absoluteInstr mem disk (Mb b) = Mb (absolve disk b)
  absoluteInstr mem disk i = fmap (absolve mem) i

{- |
  PP-2 and PP-3 hot-swap the individual routines in memory. This means that theh position of
  the code in memory will not be the same as it's position in disk. This is reflected by the
  memoryLayout and diskLayout functions that calculate the position of everything in memory
  and disk respectively. The disk layout should only be used in Ma/Mb commands when we are
  trying to load data from a disk.
-}

-- | Return a list of globals grouped by section
globalSections :: Eq (AddressType a) => ModuleAssembly a -> [GlobalMap a]
globalSections (Mod{..}) = groupBy ((==) `on` (\(_, s, _) -> s)) $ sortOn (\(_, s, _) -> s) globals

globalMapSize :: (Constant a -> Int) -> [(String, Section, Constant a)] -> Int
globalMapSize sf m = sum (map (\(_, _, c) -> sf c) m)

{- |
  Calculate the memory layout for a module at runtime.

  This determines absolute memory addresses for all code and data. Unlike diskLayout,
  all constants (including Size) consume their full memory allocation regardless of packSize.

  Segments enable procedure hot-swapping: multiple procedures in a segment share the same
  memory region, allowing PP-2 and PP-3 to load different sub-routines on demand without
  exceeding the 256-cell limit. Segment size is the maximum of all procedures within it.

  Padding spaces globals and procedures to align with the 1023-cell address space boundary.
-}
memoryLayout :: (Show (AddressType a), Eq (AddressType a)) => ModuleAssembly a -> MemoryLayout
memoryLayout m@Mod{..} =
  let
    (secs, sizes) = unzip $ map (\gs -> (getSection $ head gs, globalMapSize constantSize gs)) (globalSections m)

    globalOffs = scanl (+) 0 sizes

    procSizes = map (\p -> (procName p, procedureLen p)) procs

    -- Procedures are grouped into segments which share the same memory block. This means the size of a given
    -- segment will be the maximum of the sizes of all procedures inside that segment.
    segSizes = map (\(MkSeg seg) -> maximum . map snd $ filter (\p -> fst p `elem` seg) procSizes) segments
    segOffs = scanl (+) (sum sizes) segSizes
    procOffs = concatMap (\(MkSeg seg, size) -> map (\s -> (Text s, size)) seg) (zip segments segOffs)
    (pinned, default') = partition (\(s, _) -> s /= DefaultData) (zip secs globalOffs)
    usedSpace = sum segSizes + sum sizes
    padding = 1023 - usedSpace + 1
   in
    MkLayout
      { size = usedSpace
      , offsets = pinned ++ map (fmap (+ padding)) (default' ++ procOffs)
      , padding = padding
      , packedCells = []
      }
 where
  getSection (_, s, _) = s

{- |
  Calculate the disk storage requirement for a constant.

  Size constants declare memory regions but contain no data. When packSize is enabled,
  they are allocated in memory but not written to disk, reducing disk image size.
  This is critical for PP-2 and PP-3 which use large Size blocks (e.g. 750 cells) that
  would otherwise exceed the 1023 cell limit.
-}
constantDiskSize :: Constant a -> Int
constantDiskSize (Size _) = 0
constantDiskSize a = constantSize a

{- |
  Calculate the disk layout for a module, determining where code and data reside on disk.

  Unlike memoryLayout, this reflects the actual disk image written to magnetic drum storage.
  When packSize is True, Size constants occupy zero disk space but retain their memory allocation.
  The bootloader uses packedCells offsets to account for this discrepancy when loading modules.

  Padding is calculated as available space minus used space. Negative padding indicates the
  module exceeds the 1023 cell limit and cannot be stored on disk. This typically occurs when:
  - Large Size constants are not packed (packSize = False)
  - Total procedure size exceeds available space

  For PP-2 and PP-3, packSize must be True to fit within disk constraints.
-}
diskLayout :: Eq (AddressType a) => ModuleAssembly a -> MemoryLayout
diskLayout m@(Mod{..}) =
  let
    packingMeasure = if packSize then (\c -> constantSize c - constantDiskSize c) else const 0
    constantMeasure = if packSize then constantDiskSize else constantSize
    (secs, sizes, packed) = unzip3 $ map (\gs -> (getSection $ head gs, globalMapSize constantMeasure gs, globalMapSize packingMeasure gs)) (globalSections m)
    globalOffs = scanl (+) 0 sizes

    procSizes = map procedureLen procs
    procOffs = scanl (+) (sum sizes) procSizes

    usedSpace = sum procSizes + sum sizes
    padding = 1023 - usedSpace
    -- padding = 0
    (pinned, default') = partition (\(s, _) -> s /= DefaultData) (zip secs globalOffs)
   in
    MkLayout
      { size = usedSpace
      , offsets = pinned ++ map (fmap (+ padding)) (default' ++ zip (map (Text . procName) procs) procOffs)
      , padding = padding
      , packedCells = zip secs packed
      }
 where
  getSection (_, s, _) = s

-- * Relativization

-- | A simplified address representation.
data RelativeAddr
  = Rel Section Int -- Constant and operator references
  | Abs Int -- For statically known addresses 'aka standard cells'
  deriving (Show, Eq)

-- | Relative addresses are either within a specific procedure or they refer to the data segment
data Section = Text String | Data String | DefaultData
  deriving (Show, Eq, Ord)

{- |
  Drastically simplify the address representation, converting everything
  to simple offsets from section heads.
-}
relativize :: ModuleAssembly LaidOut -> Either [String] (ModuleAssembly Relativized)
relativize (Mod{..}) = do
  let dict = mkRelativizationDict globals procs
  procs <- first concat . unzipEithers $ map (relativizeProc dict) procs
  globals <- relativizeConstants dict globals
  pure $ Mod{relativeMap = dict, ..}
 where
  relativizeProc :: Map Address RelativeAddr -> Procedure Address -> Either [String] (Procedure RelativeAddr)
  relativizeProc dict proc = do
    relativized <- unzipEithers $ map (traverse (relativizeAddr dict)) (blocks proc)

    constDefs <- unzipEithers $ map (traverse (relativizeAddr dict)) (constDefs proc)
    pure $
      proc
        { blocks = relativized
        , constDefs = constDefs
        }

  relativizeConstants :: Map Address RelativeAddr -> GlobalMap LaidOut -> Either [String] (GlobalMap Relativized)
  relativizeConstants dict constants =
    unzipEithers $
      map
        ( \(key, s, val) -> pure ((,,) key) <*> pure s <*> traverse (relativizeAddr dict) val
        )
        constants

  relativizeAddr :: Map Address RelativeAddr -> Address -> Either String RelativeAddr
  relativizeAddr m p@(Procedure n a) = case p `M.lookup` m of
    Just relAddr -> Right relAddr
    Nothing -> Left $ "Missing " ++ show a ++ " for procedure " ++ n
  relativizeAddr m a@(Unknown _) = case a `M.lookup` m of
    Just constant -> Right constant
    Nothing -> Left $ "Unknown constant " ++ show a
  relativizeAddr m r@(RTC a) = case r `M.lookup` m of
    Just relAddr -> Right relAddr
    Nothing -> Left $ "could not find rtc for " ++ show a
  relativizeAddr m (Offset a o) =
    relativizeAddr m a >>= \case
      Abs i -> Right $ Abs (i + o)
      Rel s i -> Right $ Rel s (i + o)
  relativizeAddr m (Absolute a) = Right $ Abs a
  relativizeAddr m a = case a `M.lookup` m of
    Just relAddr -> Right relAddr
    Nothing -> Left $ "could not relativize " ++ show a

-- | Build up a map giving the relative offset of every constant and block
mkRelativizationDict :: GlobalMap Input -> [Procedure Address] -> Map Address RelativeAddr
mkRelativizationDict constants procs =
  let
    constantOffsets = dataOffsets constants
    bbOffsets =
      procs >>= \proc ->
        let
          (o, constOffset) = buildOffsetMap (Unknown . constantName) (constantSize . fromDef) (constDefs proc)
          nm = procName proc
         in
          (ProcStart nm, Rel (Text nm) 0)
            : (ProcEnd nm, Rel (Text nm) (procedureLen proc - 1))
            : map (fmap (Rel (Text nm))) constOffset
            ++ blockOffsets nm o (blocks proc)
   in
    M.fromList $ bbOffsets ++ constantOffsets

dataOffsets :: GlobalMap Input -> [(Address, RelativeAddr)]
dataOffsets globals =
  let
    globalGroups = groupBy ((==) `on` \(_, s, _) -> s) globals
    globalMap = concatMap (snd . buildOffsetMap toKey toVal) globalGroups
   in
    map (\((s, n), v) -> (n, Rel s v)) globalMap
 where
  toKey (n, s, _) = (s, Unknown n)
  toVal (_, _, c) = constantSize c

-- | For a procedure, and a starting offset, give the relative address of every operator
blockOffsets :: String -> Int -> [BB Address] -> [(Address, RelativeAddr)]
blockOffsets nm off (bb : blks) =
  let
    rest = blockOffsets nm (off + blockLen bb) blks
    entry = [(baseAddress bb, Rel (Text nm) off)]
    rtcEntry = case terminator bb of
      RetRTC _ -> [(RTC $ baseAddress bb, Rel (Text nm) (off + blockLen bb - 1))]
      _ -> []
   in
    entry ++ rtcEntry ++ rest
blockOffsets _ _ [] = []

-- * Internalization

{- $internalize
  Wrap every address with the corresponding procedure, making them unambiguous when mixed
  with other procedures.
-}
internalizeModule :: ModuleAssembly LaidOut -> ModuleAssembly LaidOut
internalizeModule mod =
  mod
    { procs = map internalizeAddresses (procs mod)
    }

internalizeAddresses :: Procedure Address -> Procedure Address
internalizeAddresses proc =
  proc{blocks = map (fmap internalizeAddress) (blocks proc)}
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
  (a, _) -> Left a

buildOffsetMap :: (a -> nm) -> (a -> Int) -> [a] -> (Int, [(nm, Int)])
buildOffsetMap key size = mapAccumL
    ( \off elem ->
        (off + size elem, (key elem, off))
    )
    0

redString :: String -> String
redString str = "\27[31;1m" ++ str ++ "\27[0m"

templateArgs :: ConstantDef Address -> [String]
templateArgs (Def _ _ c) | isTemplate c = toList c >>= unknowns
templateArgs _ = []

globalName :: ConstantDef a -> Maybe String
globalName (Def Global nm _) = Just nm
globalName (Def (Pinned _) nm _) = Just nm
globalName _ = Nothing

isTemplate (Template _) = True
isTemplate _ = False

isGlobal (Def Local _ _) = False
isGlobal _ = True

def (Def _ _ c) = Just c
def _ = Nothing

isExtern (Extern _) = True
isExtern _ = False

isCell (Just Cell) = True
isCell _ = False

isSize (Just (Size _)) = True
isSize _ = False

externName :: ConstantDef a -> Maybe String
externName (Extern nm) = Just nm
externName _ = Nothing

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
layout (Mod{..}) =
  Mod
    { procs = map segmentProcedure procs
    , globals = sortBy sortGlobals (procs >>= globalDefs)
    , ..
    }
 where
  globalDefs proc = mapMaybe isGlobal $ constDefs proc

  {-
    The objective of this ordering function is to put Size < Cell < Template < All the others

  -}
  sortGlobals (n, s, c) (m, t, d)
    | s == t = case (c, d) of
        (Size _, Size _) -> n `compare` m
        (Size _, _) -> LT
        (Cell, Size _) -> GT
        (Cell, Cell) -> n `compare` m
        (Cell, _) -> LT
        (Template _, Size _) -> GT
        (Template _, Cell) -> GT
        (Template _, Template _) -> n `compare` m
        (_, Size _) -> GT
        (_, Cell) -> GT
        (_, Template _) -> GT
        (_, _) -> n `compare` m
    | otherwise = s `compare` t

  isGlobal (Def Global nm c) = Just (nm, DefaultData, c)
  isGlobal (Def (Pinned s) nm c) = Just (nm, Data s, c)
  isGlobal _ = Nothing

  -- We store all the local variables of a procedure with the procedure itself so that it can
  -- be efficiently hot-swapped in from disk.
  segmentProcedure proc =
    proc
      { blocks = join $ map (addJump . fromSegment) . M.elems $ go (toUnsegmentedMap (blocks proc))
      , constDefs = layoutConstants' (constDefs proc)
      }

  fromSegment (Segmented x) = x

  go map = go' map (M.keys map)

  go' map (k : eys) = go' (segment map k) eys
  go' map [] = map

{-

  Sort the constants according to the order we want to lay them out in memory

  +--------------+---------------+-----------+-------+
  | Empty Blocks | Working Cells | Templates | Other |
  +--------------+---------------+-----------+-------+

  This allows us to also skip loading empty blocks and only load from the working cells.
-}

layoutConstants' :: Show a => [ConstantDef a] -> [ConstantDef a]
layoutConstants' constants =
  let
    (_, locals) = partition isGlobal constants
    (cells, rem) = partition (isCell . def) locals
    (blocks, rem') = partition (isSize . def) rem
    (templates, rem'') = partition (or . fmap isTemplate . def) rem'
    (_, rem''') = partition isExtern rem''
   in
    blocks ++ cells ++ templates ++ sortOn constantName rem'''
 where

{- | Segments are non-empty, ordered sets of basic blocks where control flow goes
     linear from start to end. The @Chain@ terminators in those blocks will be eliminated
     as well as the second target of comparison instructions.
-}
data Segment = Segmented [BB Address] | Unsegmented (BB Address)

toUnsegmentedMap :: [BB Address] -> Map Address Segment
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
      _ -> pure map
  _ -> map

{- |
  Reify any implicit jumps at the end of a segment. This will convert a @Chain@ into
  a @CCCC@ and will add a @CCCC@ after any comparison.
-}
addJump :: [BB Address] -> [BB Address]
addJump [bb] = case implicitJumps (terminator bb) of
  Just iJ ->
    let
      bb' = case terminator bb of
        Comp l r b i -> bb{terminator = Comp l r b (baseAddress jB)}
        CompMod l r b i -> bb{terminator = CompMod l r b (baseAddress jB)}
        CompWord l r b i -> bb{terminator = CompWord l r b (baseAddress jB)}
        _ -> bb{terminator = Chain (baseAddress jB)} -- add hard jump
      jB = jumpBlk bb iJ
     in
      [bb', jB]
  Nothing -> [bb]
 where
  jumpBlk fromB to = BB [] (CCCC to) (baseAddress fromB `offAddr` (blockLen fromB + 1))
addJump (bb : bbs) = bb : addJump bbs
addJump [] = []

-- | Direct jumps are jumps that an instruction must always perform. These jumps can't be optimized away
directJumps :: Term a -> Maybe a
directJumps (Comp _ _ a _) = Just a
directJumps (CompWord _ _ a _) = Just a
directJumps (CompMod _ _ a _) = Just a
directJumps (CCCC _) = Nothing
directJumps (CCCCSnd _ a) = Just a
directJumps Stop = Nothing
directJumps SwitchStop = Nothing
directJumps JCC = Nothing
directJumps (Chain _) = Nothing
directJumps (RetRTC _) = Nothing

-- | These are the jumps that could potentially be optimized away.
implicitJumps :: Term a -> Maybe a
implicitJumps (Comp _ _ _ a) = Just a
implicitJumps (CompWord _ _ _ a) = Just a
implicitJumps (CompMod _ _ _ a) = Just a
implicitJumps (CCCC a) = Just a
implicitJumps (CCCCSnd _ _) = Nothing
implicitJumps Stop = Nothing
implicitJumps SwitchStop = Nothing
implicitJumps JCC = Nothing
implicitJumps (JCCChain a) = Just a
implicitJumps (Chain a) = Just a
implicitJumps (RetRTC _) = Nothing

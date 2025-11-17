module TraceVisualizer where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
  ( Attribute (FontSize, RankDir, RankSep, Sep)
  , RankDir (FromTop)
  , DPoint (DVal)
  )
import Data.GraphViz.Commands

import Data.List (nub, partition, sortOn, isPrefixOf)
import Data.Tuple (swap)
import System.Environment

import Data.Bifunctor
import Data.String
import Data.Text.Lazy (pack)

import Data.Bool
import qualified Data.Map as M
import System.FilePath (takeBaseName)
import Data.Char (toUpper)

toEdgeList :: [a] -> [(a, a)]
toEdgeList (x : y : xs) = (x, y) : toEdgeList (y : xs)
toEdgeList _ = []

type Label = (String, String)
type ProcedureName = String
type SubprogramID = Int
type SourceMapByProc = M.Map SubprogramID [(Int, Label)]

readSourceMap :: String -> IO SourceMapByProc
readSourceMap mapFile = do
  raw <- readFile mapFile
  let entries = map ((\(k, v, subprogID) -> (read k, v, subprogID)) . toPair) $ lines raw

  let grouped = M.fromListWith (++) [(subprogID, [(addr, label)]) | (addr, label, subprogID) <- entries]

  return $ M.map (sortOn fst) grouped
 where
  toPair line =
    let
      k : val = splitOn ' ' line

      (proc, op) = splitAt 2 val

      procName = extractProcName (unwords proc)
      subprogID = procNameToSubprogramID procName
     in
      (k, (unwords proc, unwords op), subprogID)

  extractProcName :: String -> String
  extractProcName s =
    -- Extract text between quotes: "proc. \"MP-1\"" -> "MP-1"
    case dropWhile (/= '"') s of
      ('"':rest) -> takeWhile (/= '"') rest
      _ -> "unknown"

  -- Convert procedure name to subprogram ID
  -- MP-* -> 0, PP-*-N -> N, (Roman)-PP-* -> roman number
  -- todo: change the sourcemap format to make this easier
  procNameToSubprogramID :: String -> SubprogramID
  procNameToSubprogramID name
    | "MP-" `isPrefixOf` name = 0  -- Main programs
    | otherwise = case words $ map (\c -> if c == '-' then ' ' else c) name of
        ("PP" : _phase : num : _) -> read num  -- PP-1-2 -> 2
        (roman : "PP" : _) -> romanToInt roman  -- I-PP-2 -> 1
        _ -> 0

  romanToInt :: String -> Int
  romanToInt "I" = 1
  romanToInt "II" = 2
  romanToInt "III" = 3
  romanToInt "IV" = 4
  romanToInt "V" = 5
  romanToInt _ = 0

-- Parse the phase from a trace line (PP1, PP2, PP3, etc.)
getPhase :: String -> String
getPhase line =
  case words line of
    (phase:_) -> takeWhile (/= '-') phase  -- Extract "PP1", "PP2", "PP3" etc.
    _ -> "PP1"  -- Default to PP1 if can't parse

-- Parse the subprogram ID from a trace line (field 1)
getSubprogramID :: String -> SubprogramID
getSubprogramID line =
  case splitOn ' ' line of
    (_:subprogID:_) -> read subprogID
    _ -> 0

sourceMapLookup :: M.Map String SourceMapByProc -> String -> Label
sourceMapLookup mapDict line = do
  let phase = getPhase line
  let subprogID = getSubprogramID line
  let addr = read $ splitOn ' ' line !! 3  -- Address is the 4th field (index 3)

  case M.lookup phase mapDict of
    Nothing -> (phase, "addr " ++ show addr)  -- No source map, leave untranslated
    Just subprogMap -> case M.lookup subprogID subprogMap of
      Nothing -> (phase ++ " subprog " ++ show subprogID, "addr " ++ show addr)
      Just entries -> case findNearest addr entries of
        Just label -> label
        Nothing -> (phase ++ " subprog " ++ show subprogID, "addr " ++ show addr)
 where
  findNearest :: Int -> [(Int, Label)] -> Maybe Label
  findNearest i ((l, nm) : r@((u, _) : es))
    | i == l = Just nm
    | l < i && i < u = Just nm
    | otherwise = findNearest i r
  findNearest _ [(u, nm)] = Just nm
  findNearest _ [] = Nothing

visualizeTrace :: Bool -> String -> String -> [String] -> IO ()
visualizeTrace dedup inFile oFile smFiles = do
  traceFile <- readFile inFile
  sourceMaps <- mapM readSourceMap smFiles

  -- Create a map from phase name to source map (organized by procedure)
  -- e.g., "pp1.sm" -> ("PP1", Map ProcedureName [(Int, Label)])
  let phaseNames = map (map toUpper . takeBaseName) smFiles
  let sourceMapDict = M.fromList $ zip phaseNames sourceMaps

  -- Debug output
  putStrLn $ "Loaded source maps for phases: " ++ show phaseNames
  mapM_ (\(phase, subprogMap) ->
    putStrLn $ "  " ++ phase ++ " subprograms: " ++ show (M.keys subprogMap))
    (M.toList sourceMapDict)
  let sampleLines = take 5 $ lines traceFile
  putStrLn $ "Sample trace lines:"
  mapM_ putStrLn sampleLines
  putStrLn $ "Parsed phases: " ++ show (map getPhase sampleLines)

  putStrLn "Rendering trace graph..."
  let
    trace = map (sourceMapLookup sourceMapDict) $ lines traceFile
    nodeDict = zip (nub trace) [1 ..]
    edges = bool dedupEdges constEdges dedup $ map (toEdge nodeDict) (toEdgeList trace)
    graph = mkGraph (map swap nodeDict) edges
  runGraphvizCommand Dot (graphToDot params (graph :: Gr Label Int)) Png oFile

  mapM_ (\(p, o) -> putStrLn $ p ++ " " ++ o) trace
 where
  constEdges :: [(Node, Node, ())] -> [(Node, Node, Int)]

  dedupEdges :: [(Node, Node, ())] -> [(Node, Node, Int)]
  dedupEdges edges =
    let
      classes = eqClasses edges
     in
      map (\c@((s, t, _) : _) -> (s, t, length c)) classes

  toEdge dict (x, y) = (fromJust' $ x `lookup` dict, fromJust' $ y `lookup` dict, ())
  fromJust' (Just x) = x

  params :: GraphvizParams Node Label Int String Label
  params =
    defaultParams
      { fmtNode = \(_, l) -> [toLabel $ (\(p, o) -> p ++ " " ++ o) l, style rounded, shape BoxShape, FontSize 20.0]
      , fmtEdge = \(_, _, l) -> [edgeLabel l, FontSize 20.0]
      , clusterBy = \(node, lab) -> C (fst lab) (N (node, lab))
      , clusterID = Str . pack
      , fmtCluster = const [GraphAttrs [RankDir FromTop]]
      , globalAttributes =
          [ GraphAttrs
              [ RankDir FromTop
              ]
          ]
      }
   where
    edgeLabel 1 = toLabel ""
    edgeLabel l = toLabel (" " ++ show l)
  constEdges = map (\(a, b, _) -> (a, b, 1))
eqClasses :: Eq a => [a] -> [[a]]
eqClasses [] = []
eqClasses (x : xs) =
  let (cls, rest) = partition (== x) xs
   in (x : cls) : eqClasses rest

splitOn :: Char -> String -> [String]
splitOn c xs = go c xs []
 where
  go :: Char -> String -> String -> [String]
  go c (x : xs) [] | x == c = go c xs []
  go c (x : xs) acc | x == c = reverse acc : go c xs []
  go c (x : xs) acc = go c xs (x : acc)
  go c [] acc = [reverse acc]

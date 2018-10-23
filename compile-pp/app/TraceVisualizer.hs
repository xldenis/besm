module TraceVisualizer where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Commands
import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Attribute(FontSize))

import System.Environment
import Data.Tuple (swap)
import Data.List (nub, sortOn, partition)

import Data.String
import Data.Text.Lazy (pack)
import Data.Bifunctor

import Debug.Trace

toEdgeList :: [a] -> [(a, a)]
toEdgeList (x : y : xs) = (x, y) : toEdgeList (y : xs)
toEdgeList _ = []

type Label = (String, String)

readSourceMap :: String -> IO [(Int, Label)]
readSourceMap mapFile = do
  raw <- readFile mapFile
  let pairs = sortOn fst . map toPair $ lines raw

  return $ map (\(k, v) -> (read k, v)) pairs


  where
  toPair line = let
    k : val = splitOn ' ' line

    (proc, op) = (take 2 val, drop 2 val)

    in (k, (unwords proc, unwords op))


sourceMapLookup :: [(Int, Label)]  -> String -> Label
sourceMapLookup map line = do
  let addr = read $ (splitOn ' ' line) !! 2

  findNearest addr map
  where

  findNearest :: Int -> [(Int, Label)] -> Label
  findNearest i ((l, nm) : r@((u, _) : es))
    | i == l = nm
    | l < i && i < u = nm
    | otherwise = findNearest i r
  findNearest _ [(u, nm)] = nm
  findNearest _ [] = error $ "shouldn't be possible"

visualizeTrace :: String -> String -> String -> IO ()
visualizeTrace inFile oFile smFile = do
  traceFile <- readFile inFile
  sourceMap <- readSourceMap smFile

  putStrLn "Rendering trace graph..."
  let
    trace = map (sourceMapLookup sourceMap) $ lines traceFile
    nodeDict = zip (nub trace) [1..]
    edges = dedupEdges $ map (toEdge nodeDict) (toEdgeList trace)
    graph = mkGraph (map swap nodeDict) edges
  runGraphviz (graphToDot params $ (graph :: Gr Label Int)) Png oFile

  mapM_ (\(p, o) -> putStrLn $ p ++ " " ++ o) trace

  return ()
  where

  dedupEdges edges = let
    classes = eqClasses edges
    in map (\c@((s,t, _) : _) -> (s, t, length c)) classes

  toEdge dict (x, y) = (fromJust' $ x `lookup` dict, fromJust' $  y `lookup` dict, ())
  fromJust' (Just x) = x

  params :: GraphvizParams Node Label Int String Label
  params = defaultParams
    { fmtNode = \ (_,l) -> [toLabel $ (\(p, o) -> p ++ " " ++ o) l, style rounded, shape BoxShape, FontSize 20.0]
    , fmtEdge = \ (_, _, l) -> [toLabel (" " ++ show l), FontSize 20.0]
    , clusterBy = \(node, lab) -> C (fst lab) (N (node, lab))
    , clusterID = Str . pack
    }

eqClasses :: Eq a => [a] -> [[a]]
eqClasses [] = []
eqClasses (x : xs) = let (cls, rest) = partition (== x) xs
  in (x : cls) : eqClasses rest

splitOn :: Char -> String -> [String]
splitOn c xs = go c xs []
  where

  go :: Char -> String -> String -> [String]
  go c (x : xs) []  | x == c =       go c xs []
  go c (x : xs) acc | x == c = (reverse acc) : go c xs []
  go c (x : xs) acc          = go c xs (x : acc)
  go c []       acc          = [reverse acc]

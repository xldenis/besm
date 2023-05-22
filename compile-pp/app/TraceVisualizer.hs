module TraceVisualizer where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Attributes.Complete (Attribute (FontSize))
import Data.GraphViz.Commands

import Data.List (nub, partition, sortOn)
import Data.Tuple (swap)
import System.Environment

import Data.Bifunctor
import Data.String
import Data.Text.Lazy (pack)

import Data.Bool

toEdgeList :: [a] -> [(a, a)]
toEdgeList (x : y : xs) = (x, y) : toEdgeList (y : xs)
toEdgeList _ = []

type Label = (String, String)

readSourceMap :: String -> IO [(Int, Label)]
readSourceMap mapFile = do
  raw <- readFile mapFile
  let pairs = sortOn fst . map (\(k, v) -> (read k, v)) . map toPair $ lines raw

  return pairs
 where
  toPair line =
    let
      k : val = splitOn ' ' line

      (proc, op) = (take 2 val, drop 2 val)
     in
      (k, (unwords proc, unwords op))

sourceMapLookup :: [(Int, Label)] -> String -> Label
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

visualizeTrace :: Bool -> String -> String -> String -> IO ()
visualizeTrace dedup inFile oFile smFile = do
  traceFile <- readFile inFile
  sourceMap <- readSourceMap smFile

  putStrLn "Rendering trace graph..."
  let
    trace = map (sourceMapLookup sourceMap) $ lines traceFile
    nodeDict = zip (nub trace) [1 ..]
    edges = bool dedupEdges constEdges dedup $ map (toEdge nodeDict) (toEdgeList trace)
    graph = mkGraph (map swap nodeDict) edges
  runGraphviz (graphToDot params $ (graph :: Gr Label Int)) Png oFile

  mapM_ (\(p, o) -> putStrLn $ p ++ " " ++ o) trace

  return ()
 where
  constEdges :: [(Node, Node, ())] -> [(Node, Node, Int)]
  constEdges edges = map (\(a, b, _) -> (a, b, 1)) edges

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
      }
   where
    edgeLabel 1 = toLabel ""
    edgeLabel l = toLabel (" " ++ show l)
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
  go c (x : xs) acc | x == c = (reverse acc) : go c xs []
  go c (x : xs) acc = go c xs (x : acc)
  go c [] acc = [reverse acc]

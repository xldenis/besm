module TraceVisualizer where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Commands
import Data.GraphViz

import System.Environment
import Data.Tuple (swap)
import Data.List (nub, sortOn)

import Data.String
import Data.Text.Lazy (pack)
import Data.Bifunctor

import Debug.Trace

toEdgeList :: [a] -> [(a, a)]
toEdgeList (x : y : xs) = (x, y) : toEdgeList (y : xs)
toEdgeList _ = []

readSourceMap :: String -> IO [(Int, String)]
readSourceMap mapFile = do
  raw <- readFile mapFile
  let pairs = sortOn fst . map toPair $ lines raw

  return $ map (\(k, v) -> (read k, v)) pairs


  where
  toPair (' ': rest) = ([], rest)
  toPair (a : as) = first (a :) (toPair as)

sourceMapLookup :: [(Int, String)]  -> String -> String
sourceMapLookup map line = do
  let addr = read $ (splitOn ' ' line) !! 2

  findNearest addr map
  where

  findNearest :: Int -> [(Int, String)] -> String
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
    edges = map (toEdge nodeDict) (toEdgeList trace)
    graph = mkGraph (map swap nodeDict) edges
  runGraphviz (graphToDot params $ (graph :: Gr String ())) Png oFile

  return ()
  where

  toEdge dict (x, y) = (fromJust' $ x `lookup` dict, fromJust' $  y `lookup` dict, ())
  fromJust' (Just x) = x

  params :: GraphvizParams Node String el String String
  params = defaultParams
    { fmtNode = \ (_,l) -> [toLabel l]
    , fmtEdge = \ (_, _, l) -> []
    , clusterBy = \(node, lab) -> C (procedure lab) (N (node, lab))
    , clusterID = Str . pack
    }

  procedure :: String -> String
  procedure (' ' : _) = []
  procedure (a : as) = a : procedure as

splitOn :: Char -> String -> [String]
splitOn c xs = go c xs []
  where

  go :: Char -> String -> String -> [String]
  go c (x : xs) []  | x == c =       go c xs []
  go c (x : xs) acc | x == c = (reverse acc) : go c xs []
  go c (x : xs) acc          = go c xs (x : acc)
  go c []       acc          = [reverse acc]

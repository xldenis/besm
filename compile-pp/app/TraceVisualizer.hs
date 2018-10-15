module TraceVisualizer where

import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz.Commands
import Data.GraphViz

import System.Environment
import Data.Tuple (swap)
import Data.List (nub)

import Data.String
import Data.Text.Lazy (pack)

toEdgeList :: [String] -> [(String, String)]
toEdgeList (x : y : xs) = (x, y) : toEdgeList (y : xs)
toEdgeList _ = []

main = do
  (file : out : _) <- getArgs
  traceFile <- readFile file

  let
    trace = lines traceFile
    nodeDict = zip (nub trace) [1..]
    edges = map (toEdge nodeDict) (toEdgeList trace)
    graph = mkGraph (map swap nodeDict) edges
  runGraphviz (graphToDot params $ (graph :: Gr String ())) Png out

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

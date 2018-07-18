module Main where

import Lib

import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import CFG

import Monad

import Data.GraphViz.Commands

main :: IO ()
main = do
  let bbs = runBuilder (op 999) $ arithCoder
      cfg = programmeToGraph bbs

  runGraphviz (graphToDot params $ (nmap formatAddr cfg)) Png "cfg.png"

  return ()

params :: (Labellable nl) => GraphvizParams n nl el () nl
params = nonClusteredParams
  { fmtNode = \ (_,l) -> [toLabel l]
  , fmtEdge = \ (_, _, l) -> []
  }


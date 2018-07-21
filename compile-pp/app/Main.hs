module Main where

import Lib

import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import CFG

import Monad

import Data.GraphViz.Commands
import Syntax

main :: IO ()
main = do
  let bbs = runBuilder (op 999) $ arithCoder
      cfg = programmeToGraph bbs

  runGraphviz (graphToDot params $ (nmap formatAddr cfg)) Png "cfg.png"

  print $ bb0
  return ()

params :: (Labellable nl) => GraphvizParams n nl el () nl
params = nonClusteredParams
  { fmtNode = \ (_,l) -> [toLabel l]
  , fmtEdge = \ (_, _, l) -> []
  }

bb0 = asmToCell $ BB
  { baseAddress = 0
  , terminator  = CCCC 6
  , instrs =
    [ Add 5 5 6 Normalized
    , Sub 6 5 6 Normalized
    ]
  }

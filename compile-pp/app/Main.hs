module Main where

import Lib

import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import CFG

import Monad

import Data.GraphViz.Commands
import Syntax

import PP1
import Data.Function
import Besm.Put
import Assembler

main :: IO ()
main = do
  let cfg = programmeToGraph (runBuilder (op 999) $ arithCoder)
      cf2 = programmeToGraph (runBuilder (op 999) $ mp1)

  runGraphviz (graphToDot params $ (nmap formatAddr cfg)) Png "cfg.png"
  runGraphviz (graphToDot params $ (nmap formatAddr cf2)) Png "cfg-2.png"

  print $ bb0

  mapM_ putStrLn $ assemble constantMap AlignRight (runBuilder (op 999) mp1) & map toHexString
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

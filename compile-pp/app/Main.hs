module Main where


import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Function
import Data.GraphViz.Commands

import CFG
import Monad
import Syntax
import PP1
import PP1.Logical
import Lib
import Assembler

import Besm.Put

main :: IO ()
main = do
  let cfg = programmeToGraph (runBuilder (op 999) $ arithCoder)
      cf2 = programmeToGraph (runBuilder (op 999) $ mp1)
      lcfg = programmeToGraph (runBuilder (op 999) $ pp1_1)
  runGraphviz (graphToDot params $ (nmap formatAddr cfg)) Png "cfg.png"
  runGraphviz (graphToDot params $ (nmap formatAddr cf2)) Png "cfg-2.png"

  runGraphviz (graphToDot params $ (nmap formatAddr lcfg)) Png "cfg-logi.png"
  -- print $ bb0

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

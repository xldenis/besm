module Main where


import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Function
import Data.GraphViz.Commands

import CFG
import Monad
import Syntax
import PP1
import qualified PP1.Logical as Logical
import Lib
import Assembler

import Besm.Put

main :: IO ()
main = do
  let cfg = programmeToGraph (snd . unProc $ runProcedure "PP-1-2" $ arithCoder)
      cf2 = programmeToGraph (snd . unProc $ runProcedure "MP-1" $ mp1)
      lcfg = programmeToGraph (snd . unProc $ runProcedure "PP-1-1" $ Logical.pp1_1)

  runGraphviz (graphToDot params $ (nmap formatAddr cfg)) Png "cfg.png"
  runGraphviz (graphToDot params $ (nmap formatAddr cf2)) Png "cfg-2.png"
  runGraphviz (graphToDot params $ (nmap formatAddr lcfg)) Png "cfg-logi.png"

  mapM_ putStrLn $
    assemble (Logical.constantMap ++ constantMap) AlignRight (
      [runProcedure "MP-1" mp1
      , runProcedure "PP-1" Logical.pp1_1
      , pp2, mp2
      ]) & map toHexString

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

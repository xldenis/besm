module Main where


import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Function
import Data.GraphViz.Commands

import           Besm.Assembler.CFG
import           Besm.Assembler.Monad
import           Besm.Assembler.Syntax
import           Besm.PP1 as PP1
import qualified Besm.PP1.Logical as Logical
import qualified Besm.PP1.Arithmetic as Arith
import qualified Besm.PP1.Economy as Economy
import           Besm.Assembler
import           Control.Monad
import Besm.Put

main :: IO ()
main = do
  let cfg = programmeToGraph (blocks $ runProcedure "PP-1-2" $ Arith.arithCoder)
      cf2 = programmeToGraph (blocks $ runProcedure "MP-1" $ mp1)
      lcfg = programmeToGraph (blocks $ runProcedure "PP-1-1" $ Logical.pp1_1)
      ecfg = programmeToGraph (blocks $ runProcedure "PP-1-3" $ Economy.pp1_3)

  runGraphviz (graphToDot params $ (nmap formatAddr cfg)) Png "cfg.png"
  runGraphviz (graphToDot params $ (nmap formatAddr cf2)) Png "cfg-2.png"
  runGraphviz (graphToDot params $ (nmap formatAddr lcfg)) Png "cfg-logi.png"

  mapM_ putStrLn  . either id id $
    assemble (Logical.constantMap ++ PP1.constantMap ++ Arith.constantMap ++ Economy.constantMap) AlignRight (
      [ runProcedure "MP-1" mp1
      , runProcedure "PP-1-1" Logical.pp1_1
      , runProcedure "PP-1-2" Arith.arithCoder
      , runProcedure "PP-1-3" Economy.pp1_3
      , pp2, mp2
      ]) & liftM (map toHexString)

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

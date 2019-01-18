{-# LANGUAGE LambdaCase #-}
module Compiler where

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

import           Besm.PP2 as PP2
import qualified Besm.PP2.Loop as Loop
import qualified Besm.PP2.Control as Control
import qualified Besm.PP2.Distribute as Distribute

import           Besm.Assembler
import           Control.Monad
import Besm.Put

import Options.Applicative.Simple as S

import TraceVisualizer

fileArg :: S.Parser String
fileArg = strArgument (metavar "FILE" <> help "location of source file")

outputFileArg :: S.Parser String
outputFileArg = strOption $ long "output-file" <> metavar "OUT" <> short 'o'
  <> help "provide  a name for the final executable"

sourceMapArg :: S.Parser String
sourceMapArg = strOption $ long "source-map" <> metavar "SOURCEMAP"
  <> help "location of the sourcemap file"

phase :: ReadM [Procedure Address]
phase = do
  str >>= \case
    "pp1" -> return pp1Procedures
    "pp2" -> return pp2Procedures

options =
  simpleOptions "v0.1.0" "PP-BESM Compiler Source Code and Debugger" "" (pure ()) $ do
    addCommand
      "compile"
      "generate the machine code for the PP-BESM compiler"
      compileCommand
      ((,,) <$> argument phase mempty <*> optional outputFileArg <*> optional sourceMapArg)
    addCommand
      "trace"
      "visualize a trace file generated by the BESM VM"
      traceCommand
      ((,,,) <$> fileArg <*> outputFileArg <*> sourceMapArg <*> dedupFlag)
    addCommand
      "cfg"
      "print the CFG of all procedures to disk"
      cfgCommand
      (pure ())
  where
  dedupFlag :: S.Parser Bool
  dedupFlag = flag True False (long "dedup-edges")

compileCommand (procs, oFile, smFile) = do
  let compiledModule = compile (simpleModule AlignRight procs)

  case compiledModule of
    Left err -> mapM_ putStrLn err
    Right mod -> do
      case oFile of
        Just o -> do
          writeFile o . unlines $
            render mod & (map toHexString)
        Nothing -> putStrLn . unlines $ render mod & (map toHexString)

      case smFile of
        Just path -> writeFile path $ renderSourceMap mod
        Nothing -> return ()

  return ()

cfgCommand _ = do
  let cfg = programmeToGraph  (blocks $ runProcedure "PP-1-2" $ Arith.arithCoder)
      cf2 = programmeToGraph  (blocks $ runProcedure "MP-1" $ mp1)
      lcfg = programmeToGraph (blocks $ runProcedure "PP-1-1" $ Logical.pp1_1)
      ecfg = programmeToGraph (blocks $ runProcedure "PP-1-3" $ Economy.pp1_3)

  runGraphviz (graphToDot params $ (nmap formatAddr  cfg)) Png "cfg.png"
  runGraphviz (graphToDot params $ (nmap formatAddr  cf2)) Png "cfg-2.png"
  runGraphviz (graphToDot params $ (nmap formatAddr lcfg)) Png "cfg-logi.png"

  return ()

traceCommand :: (String, String, String, Bool) -> IO ()
traceCommand (inFile, oFile, smFile, dedup) = do
  visualizeTrace dedup inFile oFile smFile

pp1Procedures =
  [ runProcedure "MP-1" mp1
  , runProcedure "PP-1-1" Logical.pp1_1
  , runProcedure "PP-1-2" Arith.arithCoder
  , runProcedure "PP-1-3" Economy.pp1_3
  , end
  ]

pp2Procedures =
  [ runProcedure ".loader" loader
  , runProcedure "MP-2" mp2
  , runProcedure "I-PP-2" Loop.pp2_1
  -- , runProcedure "II-PP-2" Control.pp2_2
  -- , runProcedure "III-PP-3" Distribute.pp2_3
  ]

main :: IO ()
main = do
  (_, comm) <- options

  comm

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

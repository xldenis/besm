{-# LANGUAGE LambdaCase, DataKinds #-}
module Compiler where

import Data.GraphViz
import Data.Graph.Inductive.Graph (nmap, emap)
import Data.Function
import Data.GraphViz.Commands
import           System.FilePath.Posix

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

import qualified Data.Map as M
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

options =
  simpleOptions "v0.1.0" "PP-BESM Compiler Source Code and Debugger" "" (pure ()) $ do
    addCommand
      "compile"
      "generate the machine code for the PP-BESM compiler"
      compileCommand
      ((,) <$> optional outputFileArg <*> optional sourceMapArg)
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

    addCommand
      "debug"
      "print some debbug info"
      debugCommand
      (pure ())
  where
  dedupFlag :: S.Parser Bool
  dedupFlag = flag True False (long "dedup-edges")

compiledModules = do
  let pp1 = compile (simpleModule AlignRight pp1Procedures)
  let pp2Segs = [MkSeg ["MP-2"], MkSeg ["I-PP-2", "II-PP-2", "III-PP-2"]]
  let pp2 = compile ((simpleModule AlignRight pp2Procedures) {segments = pp2Segs })

  (,) <$> pp1 <*> pp2

compileCommand (oDir, smDir) = do
  case compiledModules of
    Left err -> mapM_ putStrLn err
    Right (pp1, pp2) -> do
      case oDir of
        Just o -> do
          writeFile (o </> "mp1.txt") . unlines $ render pp1 & (map toHexString)
          writeFile (o </> "mp2.txt") . unlines $ render pp2 & (map toHexString)
          writeFile (o </> "boot.txt") . unlines $ renderProc 0 (bootloader pp1 pp2) & (map toHexString)
        Nothing -> do
          putStrLn . unlines $ render pp1 & (map toHexString)
          putStrLn "\n\n PP-2 \n\n"
          putStrLn . unlines $ render pp2 & (map toHexString)

      case smDir of
        Just path -> do
          writeFile (path </> "pp1.sm") $ renderSourceMap pp1
          writeFile (path </> "pp2.sm") $ renderSourceMap pp2
        Nothing -> return ()

  return ()
  where
  bootloader :: ModuleAssembly 'Absolutized -> ModuleAssembly 'Absolutized -> Procedure Int
  bootloader pp1 pp2 = let
    Just destAddr = DefaultData `lookup` (offsets $ memoryLayout pp2)
    Just srcAddr = DefaultData `lookup` (offsets $ diskLayout pp2)
    Just pp1Start = Procedure "MP-1" (Operator 1) `M.lookup` (offsetMap pp1)
    Just pp2Start = Procedure "MP-2" (Operator 1) `M.lookup` (offsetMap pp2)
    in Proc
    { procName = "bootloader"
    , constDefs = []
    , blocks =
      [ BB
        { instrs =
          [ Ma (0x100) 0 1
          , Mb 1023
          ]
        , terminator = CCCC pp1Start
        }
      , BB
        { instrs =
          [ Ma (0x104) srcAddr destAddr
          , Mb 1023
          ]
        , terminator = CCCC pp2Start
        }
      ]
    }

debugCommand _ = do
  case compiledModules of
    Right (pp1, pp2) -> do
      modInfo pp1
      modInfo pp2
      pure ()
    Left err -> mapM_ putStrLn err
  pure ()
--     ma (0x100) 1 1
--     mb 1023
--     cccc (-1) -- how do i get this info?
-- t
--                                                   -- v this is based on the memory layout
--     ma (0x100 + 4) ( 1) ( loadAddr :: Int)
--     mb (-1) -- fuck!
--     cccc (-1) -- (Procedure "MP-2" (op 1))


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
  ]

pp2Procedures =
  [ runProcedure "MP-2" mp2
  , runProcedure "I-PP-2" Loop.pp2_1
  , runProcedure "II-PP-2" Control.pp2_2
  , runProcedure "III-PP-2" Distribute.pp2_3
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

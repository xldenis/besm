{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad

-- import           Data.BitVector.Sized.BitLayout
import Data.Semigroup
import Data.Text.IO as T

import Options.Applicative.Simple as S

import Besm
import Besm.Lower
import Besm.Parser
import Besm.Put
import Besm.Syntax

import System.Environment

import Text.PrettyPrint.HughesPJClass (pPrint, vcat, (<+>))

fileArg :: S.Parser String
fileArg = strArgument (metavar "FILE" <> help "location of source file")

options :: IO ((), IO ())
options =
  simpleOptions "v0.0.1" "BESM Prettyprinter and Coder" "" (pure ()) $ do
    addCommand "pretty" "prettyprint the programme" prettyCommand fileArg
    addCommand
      "code"
      "output the binary representation of the program (debug)"
      codeCommand
      fileArg
    addCommand
      "print"
      "output the binary representation of the program"
      printCommand
      fileArg

parseFromFile file = do
  f <- T.readFile file
  let res = parse pp file f
  case res of
    Left err -> error $ errorBundlePretty err
    Right pp -> return pp

prettyCommand :: String -> IO ()
prettyCommand f = do
  pp <- parseFromFile f
  print $ prettyProgramme pp

codeCommand :: String -> IO ()
codeCommand f = do
  pp <- parseFromFile f
  let qa = calculateQuantityAddresses lowered
      lowered = lowerProgramme pp
      prettyPrint = print . vcat . map (pPrint . toHexString)

  print "Block V"
  prettyPrint $ blockV qa (variableAddresses lowered)

  print "Block P"
  prettyPrint $ blockP qa (parameters lowered)

  print "Block C"
  prettyPrint $ blockC (constants lowered)

  print "Block K"
  prettyPrint $ blockK qa (programme lowered)

printCommand :: String -> IO ()
printCommand f = do
  pp <- parseFromFile f
  let lowered = lowerProgramme pp
      encoded = encodeProgramme lowered

  mapM_ (Prelude.putStrLn . toHexString) encoded

main :: IO ()
main = do
  (_, comm) <- options
  comm

-- prettyInstruction inst =
--   pPrint (extract opcodeLayout inst)
--     <+> pPrint (extract instAddr1 inst)
--     <+> pPrint (extract instAddr2 inst)
--     <+> pPrint (extract instAddr3 inst)

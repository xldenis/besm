module Main where

import Lib
import Syntax

import Data.Text.IO as T

import System.Environment
import Parser
import Lower
import Put

import Text.PrettyPrint.HughesPJClass



main :: IO ()
main = do
  (file : _) <- getArgs

  f <- T.readFile file

  let res = parse pp file f

  case res of
    Left err -> Prelude.putStrLn $ parseErrorPretty' f err
    Right pp -> do
      print $ prettyProgramme pp

      Prelude.putStrLn . prettyShow $ encodeProgramme (lowerProgramme pp)

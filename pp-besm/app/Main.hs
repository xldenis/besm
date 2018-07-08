module Main where

import Lib

import Data.Text.IO as T

import System.Environment
import Parser
main :: IO ()
main = do
  (file : _) <- getArgs

  f <- T.readFile file

  parseTest' pp f

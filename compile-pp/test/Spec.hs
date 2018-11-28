{-# LANGUAGE RecursiveDo #-}
import Besm.Assembler
import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import Test.Hspec

import Control.Monad.IO.Class

main :: IO ()
main = hspec $ do
  describe "assembler" $ do
    it "lays out local constants before code" $ do
      case compile AlignLeft [simpleAddition] of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod
          blocks compiled `shouldBe` [BB {instrs = [Add 1 1 1 UnNormalized], terminator = Stop, baseAddress = 2}]
          head rendered `shouldBe` 10

    it "lays out globals in common section" $ do
      case compile AlignLeft [simpleGlobal] of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod

          length rendered `shouldBe` 13

          rendered !! 10 `shouldBe` 11
    it "" $ do -- want to check that length proc == blockLens + constLens
      pending

    it "" $ do -- check for error if referring to unknown addresss
      pending

    it "properly absolutizes addresses AlignRight" $ do
      case compile AlignRight [omg, simpleGlobal] of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod

          blocks compiled `shouldBe` [BB {instrs = [AI 1019 1008 986,TN 997 987 UnNormalized], terminator = Stop, baseAddress = 1018}]

    it "properly absolutizes addresses AlignLeft" $ do
      case compile AlignLeft [omg, simpleGlobal] of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod

          blocks compiled `shouldBe` [BB {instrs = [AI 34 23 1,TN 12 2 UnNormalized], terminator = Stop, baseAddress = 33}]

  describe "monad builder" $ do
    it "" $ do -- check that helpers return the correct addresses
      pending

simpleGlobal = runProcedure "add" $ do
  local "not-buffer" (Raw 11)
  global "buffer" (Size 10)
  operator 1 $ do
    tN' (Unknown "not-buffer") (Unknown "buffer")
    stop

simpleAddition = runProcedure "add" $ do
  local "val" (Raw 10)
  operator 1 $ do
    add' (Unknown "val") (Unknown "val") (Unknown "val")
    stop

omg = runProcedure "omg" $ do
  global "c" (Raw 1)
  global "omg" (Size 10)
  global "B" Cell
  local "a" (Size 10)
  operator 1 $ mdo
    ai omg (Unknown "a") (Unknown "B")
    omg <- tN' (Unknown "c") (Unknown "omg")
    stop

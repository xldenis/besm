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
      case compile (simpleModule AlignLeft [simpleAddition]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod
          blocks compiled `shouldBe` [BB {instrs = [Add 1021 1021 1021 UnNormalized], terminator = Stop, baseAddress = 1022}]
          rendered !! 1020 `shouldBe` 10

    it "lays out globals in common section" $ do
      case compile (simpleModule AlignLeft [simpleGlobal]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod

          rendered !! 0 `shouldBe` 2
    it "" $ do -- want to check that length proc == blockLens + constLens
      pending

    it "" $ do -- check for error if referring to unknown addresss
      pending

    it "properly absolutizes addresses AlignRight" $ do
      case compile (simpleModule AlignRight [omg, simpleGlobal]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod

          blocks compiled `shouldBe` [BB {instrs = [AI 1019 1008 1,TN 12 2 UnNormalized], terminator = Stop, baseAddress = 1018}]
    -- good enough for now... still needs improving to verify that it tests useful properties
    it "check that ma / mb uses the disk mapping" $ do
      let loader = runProcedure "loader" $ do
                    operator 1 $ do
                      readMD 4 (ProcStart "sub") (ProcEnd "sub") (ProcStart "sub")
                      readMD 4 (ProcStart "add") (ProcEnd "add") (ProcStart "add")
                      stop
          add = runProcedure "add" $ do
                  local "val" (Raw 10)
                  operator 1 $ do
                    add' (Unknown "val") (Unknown "val") (Unknown "val")
                    stop
          sub = runProcedure "sub" $ do
                  local "val" (Raw 10)
                  operator 1 $ do
                    sub' (Unknown "val") (Unknown "val") (Unknown "val")
                    stop
          mod = Mod () () [] AlignRight [MkSeg ["loader"], MkSeg ["sub", "add"]] [loader, sub, add]

      case compile mod of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compile = head $ procs mod

          liftIO $  (debugRender mod)
          blocks compile `shouldBe` [BB {instrs = [Ma 260 1018 1021,Mb 1020,Ma 260 1021 1021,Mb 1023], terminator = Stop, baseAddress = 1016}]

  describe "monad builder" $ do
    it "" $ do -- check that helpers return the correct addresses
      pending

simpleGlobal = runProcedure "add" $ do
  local "not-buffer" (Raw 11)
  global "buffer" (Size 10)
  global "val" (Raw 2)
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

import Besm.Assembler
import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "assembler" $ do
    it "lays out local constants before code" $ do
      case compile AlignLeft [simpleAddition] of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled  = head $ procs mod
              rendered = render mod
          blocks compiled `shouldBe` [BB {instrs = [Add 0 0 0 UnNormalized], terminator = Stop, baseAddress = 1}]
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Besm.Assembler
import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import Test.Hspec

import Control.Monad.IO.Class
import qualified Data.Map as M

import Besm.Put
import Data.BitVector.Sized

main :: IO ()
main = hspec $ do
  describe "assembler" $ do
    it "lays out local constants before code" $ do
      case compile (simpleModule [simpleAddition]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled = head $ procs mod
              rendered = render mod
          rendered !! (1021 - 1) `shouldBe` 10

          blocks compiled `shouldBe` [BB{instrs = [Add 1021 1021 1021 UnNormalized], terminator = Stop, baseAddress = 1022}]

    it "lays out globals in common section" $ do
      case compile (simpleModule [simpleGlobal]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled = head $ procs mod
              rendered = render mod

          rendered !! 1019 `shouldBe` 2
    it "" $ do
      -- want to check that length proc == blockLens + constLens
      pending

    it "" $ do
      -- check for error if referring to unknown addresss
      pending

    it "generates proper mapping tables" $ do
      case compile (simpleModule [simpleGlobal]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled = head $ procs mod
              rendered = render mod
              Just start = Procedure "add" (Operator 1) `M.lookup` (offsetMap mod)
          -- liftIO $  (debugRender mod)
          -- liftIO $ print $ relativeMap mod

          let expected = (bitVector 1 :: BV 4) <:> bv 1 <:> (b0 :: BV 9) <:> (instToCell (TN 1021 1010 UnNormalized))
          -- liftIO $ print rendered
          rendered !! (start - 1) `shouldBe` expected

    it "properly absolutizes addresses" $ do
      case compile (simpleModule [omg, simpleGlobal]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let compiled = head $ procs mod
              rendered = render mod
          blocks compiled `shouldBe` [BB{instrs = [AI 1019 1008 1005, TN 1006 995 UnNormalized], terminator = Stop, baseAddress = 1018}]
    -- good enough for now... still needs improving to verify that it tests useful properties
    describe "segments" $ do
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
          mod = (simpleModule [loader, sub, add]){segments = [MkSeg ["loader"], MkSeg ["sub", "add"]], packSize = False}

      case compile mod of
        Left err -> it "module should be valid" $ expectationFailure (show err)
        Right mod -> do
          it "check that ma / mb uses the disk mapping" $ do
            let compile = head $ procs mod

            -- liftIO $  (debugRender mod)
            blocks compile `shouldBe` [BB{instrs = [Ma 260 1017 1021, Mb 1019, Ma 260 1020 1021, Mb 1022], terminator = Stop, baseAddress = 1016}]

          it "works with segments" $ do
            let compiled = head $ procs mod
                rendered = render mod
                Just start = Text "loader" `lookup` offsets (diskLayout mod)

            let expected = (bitVector 1 :: BV 4) <:> bv 1 <:> (b0 :: BV 9) <:> instToCell (Ma 260 1017 1021)
            -- liftIO $ print rendered
            -- liftIO $ debugRender mod
            -- liftIO $ print start
            rendered !! start `shouldBe` expected

  describe "memory layout" $ do
    it "calculates correct position of DefaultData" $ do
      let code = runProcedure "P1" $ do
            global "simple" (Raw 15)
            operator 1 stop

      case compile (simpleModule [code]) of
        Left err -> expectationFailure (show err)
        Right mod -> do
          let Just dataStart = DefaultData `lookup` (offsets $ memoryLayout mod)

          render mod !! (dataStart - 1) `shouldBe` 15

  describe "disk layout" $ do
    it "provides proper offsets to account for packed cells" $
      pending

  describe "monad builder" $ do
    it "" $ do
      -- check that helpers return the correct addresses
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

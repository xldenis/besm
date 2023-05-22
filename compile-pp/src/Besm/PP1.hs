{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}

module Besm.PP1 where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax
import qualified Data.Bits as B

informationBlock = Unknown "buffer" `offAddr` 0
completedInstructions = Unknown "buffer" `offAddr` 96

selectionCounter = Unknown "selection counter"
ninetysix = Unknown "96"
header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7
cellKlast = header `offAddr` 5
arrangementCounter = Unknown "arrangement counter"
hundredfourtyfour = Unknown "144"
maxSelected = Unknown "max-selected"
maxWritten = Unknown "max-written"

end = Proc "MP-2" [BB{baseAddress = Operator 1, instrs = [], terminator = Stop}] []

{-
  Known Bugs
  ==========

  1. Selection and Arrangement counters should be absolute. Not relative

-}

mp1 = do
  global "A + 1" Cell
  local "buffer" (Size 240)
  pinned "header" "programme header table" (Size 15)
  local "selection counter" (Val 0)
  local "arrangement counter" (Val 0)
  local "96" (Raw 96)
  local "144" (Raw 144)
  global "A" Cell
  global "B" Cell
  local "0x18" (Raw 0x18)
  local "snd and third addr mask" (Raw $ 0b11111111111 `B.shift` 11 B..|. 0b11111111111)
  local "start of 144 block" (Val 0)
  local "96 shifted" (Raw $ 96 `B.shift` 11)
  local "144 shifted 2nd addr" (Raw $ 144 `B.shift` 11)
  local "max-selected" (Raw 96)
  local "max-written" (Raw 144)
  local "-96-shifted" (Raw $ 0b11110100000 `B.shift` 22)
  local "-144" (Raw 0b11101110000)

  {-

    Op. 1 reads from MD-2 and writes the contents of blocks V and P on MD-1.

  -}
  operator 1 $ mdo
    cellC <- readMD 2 (Absolute 7) (Absolute 15) header
    sub' (header `offAddr` 2) one cellA

    -- Read blocks V and P from MD-2 and then write them to MD-1
    shift cellA (left 11) cellB

    ai addr cellB addr

    let buffer = Unknown "buffer" -- Address above the executable, at most we need 256 bytes. (less since we have no constants)
    ma (Absolute $ 0x0100 + 2) (Absolute 0x10) buffer
    addr <- mb (Absolute 0)

    -- write it back out

    ai addr' cellB addr'

    ma (Absolute $ 0x0300 + 1) (Absolute 0x10) buffer
    addr' <- mb (Absolute 0)

    {-
      During the execution of PP-1 we change the layout of the program in memory, from
      V, P, C, K to V, P, K. this means that the first cell of K is now what was the first
      cell of C. To this end we initialize the arrangementCounter with this value.
    -}

    tN' cellA arrangementCounter
    ai maxWritten cellA maxWritten

    tN' (header `offAddr` 4) selectionCounter
    tN' (header `offAddr` 4) maxSelected

    -- Absolutely way too brittle
    -- Initialize values inside of the selection and arrangement subroutines

    shift (header `offAddr` 4) (left 11) cellB

    ai (op 19) cellB (op 19)
    ai (op 19 `offAddr` 1) cellB (op 19 `offAddr` 1)

    shift (header `offAddr` 2) (left 11) cellB

    ai (op 22) cellB (op 22)
    ai (op 22 `offAddr` 1) cellB (op 22 `offAddr` 1)

    -- Update the start position of K in the header table
    tN' cellA (header `offAddr` 4)

    chain (op 2)

  {-
    Op. 2 selects the next line of information on the logical scheme and transfers it to cell A.
  -}
  operator 2 $ do
    callRtc (op 17) (op 20)
    chain (op 3)

  {-
    Op. 3 extracts the exponent x from (A).
  -}
  operator 3 $ do
    tExp' cellA cellB
    chain (op 4)

  {-
    Op. 4 transfers control to op. 5 if x = 0.
  -}
  operator 4 $ do
    compWord zero cellB (op 6) (op 5)
  {-
    Op. 5 in the case that (A) != 0, transfers control to the block of
    arithmetical operators since a "non-vacant" line of information with zero
    exponent is information on an arithmetical operator.
  -}
  operator 5 $ do
    let pp_2 = Procedure "PP-1-2" (op 1)
    compWord zero cellA pp_2 (op 6)
  {-
    Op. 6 transfers control to op. 11 if x != 018. In this case in cell A is
    found a line of information on a non-standard operator or the
    close-parentheses of a loop.
  -}
  operator 6 $ do
    let logOp = Unknown "0x18" -- 0x18
    compWord logOp cellB (op 11) (op 7)
  {-
    If x = 018, there may be in cell A the number of an operator,
      open-parentheses of a loop or initial information on a logical operator.
      In the later case, there is either zero or the number of this operator
      in the first address (A), to distinguish this information.

    Op. 7 places in cell A + 1 the extracted first address (A), and in cell A,
    the extracted second and third address of (A).
  -}
  operator 7 $ do
    let secondAndThirdAddr = Unknown "snd and third addr mask"

    bitAnd firstAddr cellA cellA1
    bitAnd secondAndThirdAddr cellA cellA

    chain (op 8)

  {-
    Op. 8 examines (A + 1). IF (A + 1) != 0, then in A + 1 is the number of the
    operator of the open-parentheses of the loop, which
  -}
  operator 8 $ do
    compWord zero cellA1 (op 9) (op 10)
  {-
    Op. 9 transfers to the block of completed instructions.
  -}
  operator 9 $ do
    callRtc (op 21) (op 23)
    chain (op 10)
  {-
    Op. 10 examines (A). if (A) != 0, then this first line of information on
    the logical operator and control is transferred to the block of logical
    operators. If (A) = 0 control is transferred to op. 2 for selection of the
    following line of information.

  -}
  operator 10 $ do
    compWord zero cellA (Procedure "PP-1-1" (op 1)) (op 2)
  {-
    Op. 11 sends this line to cell A + 1 and from there to the block of
    completed instructions.

  -}
  operator 11 $ do
    tN' cellA cellA1
    callRtc (op 21) (op 23)
    chain (op 2)
  {-
    Op. 12 transfers K_f to cell 000C, in which will be subsequently stored the
    address of the last cell of block K.
  -}
  operator 12 $ do
    tN' arrangementCounter cellKlast
    chain (op 13)

  {-
    Op. 13 compares K_f with K_cr, equal ɣ.

  -}
  operator 13 $ do
    comp arrangementCounter (header `offAddr` 6) (op 15) (op 14)

  {-
    Op. 14 is a "control stop", occuring for K_f >= K_cr. After termination of
    the functioning of PP-1 there need not be 144 instructions in the block of
    completed instructions.
  -}
  operator 14 $ do
    checkStop
  {-
    Op. 15 writes the last portion of instructions from the block of completed
    operators on MD-1 in "forced order" with the use of the writing
    instructions located in the arrangement block.

    ====

    Can probably be rewritten to use a jump instead! would save 3 instructions
  -}
  operator 15 $ mdo
    let _startMDKMinus144 = Unknown "start of 144 block"

    -- SUPER BRITTLE. NEED TO FIND A WAY AROUND THIS.

    tN' (op 22) a
    tN' (op 22 `offAddr` 1) b

    a <- ma (Absolute $ 0x0300 + 1) _startMDKMinus144 completedInstructions
    b <- mb (Absolute 0)

    chain (op 16)

  {-
    Op. 16 reads from MD-1 information on the problem prepared for the work of
    PP-2 into IS and located in standard position.
  -}
  operator 16 $ mdo
    shift arrangementCounter (left 11) cellB
    ai addr cellB addr

    let buffer = Unknown "buffer" -- Address above the executable, at most we need 256 bytes. (less since we have no constants)
    ma (Absolute $ 0x0100 + 1) (Absolute 0x10) (Absolute 0x10)
    addr <- mb (Absolute 0x10)

    let _mp2 = Absolute (1025 + 3)

    chain _mp2

  {-
    To determine the moment of completing selection of information on a
      logical scheme, a "selection counter" is in the selection block, in
      which is the address of the last line of information selected from block
      K in the initial position of information on the problem.

    Op. 17 transfers control to op. 12 as soon as the indication of the
    seleection counter becomes equal to the address of the last cell of block
    K.

    In the arrangement block is a similar counter, in which at the end of
    the functioning of PP-1 there is obtained the address K_f, the last of
    block K with transformed information on the programme with standard
    position of information on the problem.

  -}

  operator 17 $ do
    comp selectionCounter cellKlast (op 18) (op 12)

  {-
    Op. 18 tests that the contents of the selection block have been exhausted
  -}
  operator 18 $ do
    comp selectionCounter maxSelected (op 20) (op 19)

  mdo
    {-
      Op. 19 reads the next 96 cells of memory to the selection block.
    -}
    operator 19 $ do
      let startOfK = Absolute 1
      let kPlus96 = startOfK `offAddr` 95
      a <- ma (Absolute $ 0x0100 + 2) startOfK informationBlock
      b <- mb kPlus96

      let ninetysixSndAddr = Unknown "96 shifted"
      ai a ninetysixSndAddr a
      ai b ninetysixSndAddr b

      ai maxSelected ninetysix maxSelected -- reset selection counter comp
      let minus96 = Unknown "-96-shifted"
      ai iOp minus96 iOp -- reset index op
      chain (op 20)

    {-
      Op. 20 sends the contents of the first cell in the block to the standard cell A.
    -}
    iOp <- operator 20 $ mdo
      ta <- tN' (informationBlock `offAddr` 96) cellA -- use AI to modify
      ai ta oneFirstAddr ta
      ai selectionCounter one selectionCounter
      retRTC
      return ta

    return ()
  {-
    Op. 21 tests whether the arrangement block has been exhausted
  -}
  operator 21 $ do
    comp arrangementCounter maxWritten (op 23) (op 22)

  mdo
    {-
      Op. 22 writes the arrangement block to MD-1
    -}
    operator 22 $ do
      let startMDK = Absolute 0
      let endOfMDK = startMDK `offAddr` 143
      let hundredfourtyfourSndAddr = Unknown "144 shifted 2nd addr"

      a <- ma (Absolute $ 0x0300 + 1) startMDK completedInstructions
      b <- mb endOfMDK

      ai a hundredfourtyfourSndAddr a
      ai b hundredfourtyfourSndAddr b

      ai maxWritten hundredfourtyfour maxWritten -- reset selection counter comp
      ai ta (Unknown "-144") ta -- reset index op
      chain (op 23)

    {-
      Op. 23 transfers the contesnts of cell A + 1 to the next cell in the arrangement of block.
    -}
    ta <- operator 23 $ do
      ot <- tN' cellA1 completedInstructions -- use AI to modify
      ai arrangementCounter one arrangementCounter
      ai ot one ot
      retRTC
      return ot
    return ()

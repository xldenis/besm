{-# LANGUAGE RecursiveDo #-}
module PP1 where


import Monad
import Syntax

informationBlock      = Unknown "information block"
completedInstructions = Unknown "completed instructions"

cellB  = Unknown "cell b"
cellA  = Unknown "cell a"
cellA1 = Unknown "cell a+1" -- A + 1

cellKf  = Unknown "K_f"
cellKcr = Unknown "K_cr"

zero :: Address
zero = Unknown "zero"

one :: Address
one  = Unknown "one"

_selectionCounter   = Unknown "selection counter"
_ninetysix          = Unknown "96"
cellKlast           = Unknown "last K cell"
_arrangementCounter = Unknown "arrangement counter"
_hundredfourtyfour  = Unknown "144"

{-
  Known Bugs
  ==========

  1. Selection and Arrangement counters should be absolute. Not relative

-}

mp1 = do
  {-

    Op. 1 reads from MD-2 and writes the contents of blocks V and P on MD-1.

    can be done in one pass by using the upper memory
  -}
  operator 1 $ mdo
    cellC <- readMD 2 (Absolute 9) (Absolute 9) cellB
    sub cellB one cellB

    ai cellB addr addr

    let buffer = Unknown "buffer" -- Address above the executable, at most we need 256 bytes. (less since we have no constants)
    ma (Absolute $ 0x0100 + 2) (Absolute 0x10) buffer
    addr <- mb (Absolute 0)

    -- write it back out

    ai cellB addr' addr'

    ma (Absolute $ 0x0300 + 1) (Absolute 0x10) buffer
    addr' <- mb (Absolute 0)


    chain (op 2)

  {-
    Op. 2 selects the next line of information on the logical scheme and transfers it to cell A.
  -}
  operator 2 $ do
    callRtc (op 17)
    chain (op 3)

  {-
    Op. 3 extracts the exponent x from (A).
  -}
  operator 3 $ do
    tExp cellA cellB
    cccc (op 4)

  {-
    Op. 4 transfers control to op. 5 if x = 0.
  -}
  operator 4 $ do
    compWord zero cellB (op 5) (op 6)
  {-
    Op. 5 in the case that (A) != 0, transfers control to the block of
    arithmetical operators since a "non-vacant" line of information with zero
    exponent is information on an arithmetical operator.
  -}
  operator 5 $ do
    let pp_2 = Procedure "PP-2"
    compWord zero cellA (pp_2) (op 6)
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
    let _firstAddr  = Unknown "first addr mask"
        _secondAndThirdAddr = Unknown "snd and third addr mask"

    bitAnd _firstAddr cellA cellA1
    bitAnd _secondAndThirdAddr cellA cellA

    chain (op 8)

  {-
    Op. 8 examines (A + 1). IF (A + 1) != 0, then in A + 1 is the number of the
    operator of the open-parentheses of the loop, which
  -}
  operator 8 $ do
    compWord zero cellA1 (op 10) (op 9)
  {-
    Op. 9 transfers to the block of completed instructions.
  -}
  operator 9 $ do
    callRtc (op 21)
    chain (op 10)
  {-
    Op. 10 examines (A). if (A) != 0, then this first line of information on
    the logical operator and control is transferred to the block of logical
    operators. If (A) = 0 control is transferred to op. 2 for selection of the
    following line of information.


  -}
  operator 10 $ do
    compWord zero cellA (op 2) (Procedure "1-PP")
  {-
    Op. 11 sends this line to cell A + 1 and from there to the block of
    completed instructions.

  -}
  operator 11 $ do
    tN cellA cellA1
    callRtc (op 21)
    cccc (op 2)
  {-
    Op. 12 transfers K_f to cell 000C, in which will be subsequently stored the
    address of the last cell of block K.
  -}
  operator 12 $ do
    tN cellKf (Absolute 0x000C)
    chain (op 13)

  {-
    Op. 13 compares K_f with K_cr, equal ɣ.

  -}
  operator 13 $ do
    comp cellKf cellKcr (op 14) (op 15)

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
  -}
  operator 15 $ mdo
    let shift11 = Unknown "shift l 11"
    let _startMDKMinus144 = Unknown "start of 144 block"

    cellC <- shift _arrangementCounter shift11 cellC
    ai cellC b b

    a <- ma (Absolute $ 0x0300 + 1) _startMDKMinus144 completedInstructions
    b <- mb (Absolute 0)



    chain (op 16)

  {-
    Op. 16 reads from MD-1 information on the problem prepared for the work of
    PP-2 into IS and located in standard position.
  -}
  operator 16 $ mdo
    cellC <- readMD 2 (Absolute 9) (Absolute 9) cellB
    sub cellB one cellB

    ai cellB addr addr

    let buffer = Unknown "buffer"-- Address above the executable, at most we need 256 bytes. (less since we have no constants)
    ma (Absolute $ 0x0100 + 2) (Absolute 0x10) buffer
    addr <- mb (Absolute 0)

    let _mp2 = Procedure "MP-2"

    cccc _mp2

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
    comp _selectionCounter cellKlast (op 12) (op 18)

  {-
    Op. 18 tests that the contents of the selection block have been exhausted
  -}
  operator 18 $ do
    comp _selectionCounter _ninetysix (op 19) (op 20)

  {-
    Op. 19 reads the next 96 cells of memory to the selection block.
  -}
  operator 19 $ do
    let startOfK = Unknown "start of info block write"
    let kPlus96  = Unknown "end of info block write"
    a <- ma (Absolute $ 0x0100 + 2) startOfK informationBlock
    b <- mb kPlus96

    let _ninetysixSndAddr = Unknown "96 shifted"
    ai a _ninetysixSndAddr a
    ai b _ninetysixSndAddr b

    tN zero _selectionCounter

    chain (op 20)

  {-
    Op. 20 sends the contents of the first cell in the block to the standard cell A.
  -}
  operator 20 $ mdo
    ta <- tN informationBlock cellA -- use AI to modify
    ai one ta ta
    retRTC

  {-
    Op. 21 tests whether the arrangement block has been exhausted
  -}
  operator 21 $ do
    comp _arrangementCounter _hundredfourtyfour (op 22) (op 23)


  {-
    Op. 22 writes the arrangement block to MD-1
  -}
  operator 22 $ do
    let _startMDK = Unknown "end of md write"
    let _startMDKMinus144 = Unknown "start of md write"
    let _hundredfourtyfourSndAddr = Unknown "144 shifted 2nd addr"

    a <- ma (Absolute $ 0x0300 + 1) _startMDK completedInstructions
    b <- mb _startMDKMinus144

    ai a _hundredfourtyfourSndAddr a
    ai b _hundredfourtyfourSndAddr b

    tN zero _arrangementCounter

    chain (op 23)

  {-
    Op. 23 transfers the contesnts of cell A + 1 to the next cell in the arrangement of block.
  -}
  operator 23 $ do
    tN cellA1 completedInstructions -- use AI to modify

    retRTC




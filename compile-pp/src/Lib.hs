{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving, RecursiveDo #-}
module Lib where

import Monad
import Syntax

import Debug.Trace
{-
  PP-1

  """
  PP-1 contains 400 instructions and 70 constants and consists of a
  master programme (MP-1) and 3 blocks - I-PP-1, II-PP-1 and III-PP-1.
  During the period of functioning of PP-1, it is completely located
  in IS. The initial date of PP-1 is information on the problem in
  the initial position, located before the start of functioning
  of PP-1 on MD-2 and in IS.
  """ p. 79
-}

{-
  """
  Block II-PP-1 relaeizes programming of arithmetical
  operators without economy of working cells and beings its
  functioning when the first line of information on the next
  arithmetical operator is in standard cell A.
  """ p. 89

  Let us introduce a special notation for certain standard cells and counters.

  Cell A - The cell with the next line of information the girven arithmetical operator.
  Cell B - The cell in the third address of which is located the code of the next symbol
           of the formula.
  Cell C - The cell to which, in programming formulae, the codes of quantities are transferred to
           the third address. In the programming the operations of raising to a square or cube,
           the code of the argument of this operation is found in it.
  Cell D - the cell from which all transfers of formula symbols to the partial programme take place.
  Cell E - the cell to which is transferred information from the partial programme in "shifting back"
           over the partial programme.
  Counter K is used in transferring instructions from the block of the completed operator.
  Counter B1 is used in the sub-routine for transfer to the partial programme.
  Counter B2 is used in the first sub-routine of selection from the partial programme.
  Counter B3 plays the same role in the second sub-routine for selection from the partial programme as B2
  Symbol Counter is used in successive extraction of the codes of symbols from lines of information.
-}

cellC :: Address
cellC = Unknown "C"

cellD :: Address
cellD = Unknown "D"

cellE :: Address
cellE = Unknown "E"

-- Other standard cells (16 standard cells total)

cellF :: Address
cellF = Unknown "F"

cellG :: Address
cellG = Unknown "G"

cellH :: Address
cellH = Unknown "H"

cellI :: Address
cellI = Unknown "I"

cellJ :: Address
cellJ = Unknown "J"

-- Counters

counterK :: Address
counterK = undefined

{-
  Based off contextual information, the counters B_1, B_2, and B_3 each store
  addresses that refer to locations of the partial programme.

  Ref: Op. 42
-}

counterB1 :: Address
counterB1 = undefined

counterB2 :: Address
counterB2 = undefined

counterB3 :: Address
counterB3 = undefined

symbolCounter :: Address
symbolCounter = undefined

partialProgramme :: Address
partialProgramme = undefined


completedOperator = Unknown "completed operators"
-- Apparently the first addresses of the DS store some constants
zero :: Address
zero = Unknown "0"

-- this is the address of unity which I assume is a denomarlized 1 aka lowest bit set.

one :: Address
one = Absolute 0x1081

four = undefined

x1c = undefined

arithCoder :: Builder Address
arithCoder = do
  let two = Unknown "2"
  let six = Unknown "6"

  {-
    """
    Op. 1 sets counter K, counter B1 and the symbol counter to the initial
    positions, clears cell C and the last cell before the partial programme. The
    latter is necessary, if the formula of the arithmetical operator begins with
    the symbol of a quantity, since in this case in verifying the presence of a
    single-place operation the contents of the last cell could be taken as the
    symbol a single-place operation.
    """
  -}

  operator 1 $ do
    tN zero counterK
    tN partialProgramme counterB1
    tN one symbolCounter
    tN zero cellC
    tN zero (partialProgramme `offAddr` (negate 1))

    chain (op 2)
  {-
    Op 2 - 3

    Op. 2 extracts the exponent of the line of information located in cell A.

    Op. 3 in the case of a non-zero exponent, denoting that the selected line is no
    longer information on an arithmetical opterator, transfers control to the
    block of working-cell economy (III-PP-1).

  -}

  let pp_3_1 = (Procedure "PP-3" (op 1))
  operator 2 $ tExp cellA cellF
  operator 3 $ compWord cellF zero pp_3_1 (op 4)

  {-
    Op 4. extracts the code of the next symbol of the formula and adds 1 to
    the symbol counter.
  -}
  operator 4 $ do
    let left8   = undefined

    shift cellF (right 22) cellA
    shift cellF left8 cellF

    add one symbolCounter symbolCounter

    chain (op 5)
  {-
    Op. 5 compares the indication of the symbol counter  with the number 4 and
    in the case of extraction of the last code from the line transfers control
    to Op. 6

    Op. 6 which selects the next line of information from the block and
    transfers it to cell A.

    Op. 7 clears the symbol counter

  -}

  mdo
    let mp_1_17 = Procedure "MP-1" (op 17)

    operator 5 $ comp symbolCounter four (op 6) joinP
    operator 6 $ do
      callRtc mp_1_17 (Procedure "MP-1" (op 21))

    operator 7 $ do
       tN zero symbolCounter
       chain joinP

    joinP <- block $ do
      jcc
      chain (op 8)

    return ()


  {-
    Op. 8 transfers the code of the extracted symbol to cell B.

    Op. 9 determines the case of zero symbol code, denoting a gap in the
    formula  coding, transferring control to Op. 2
  -}

  operator 8 $ tN' cellF cellB >> chain (op 9)
  operator 9 $ compWord zero cellB (op 2) (op 10)

  {-
    Op 10 transfers control to Op. 11, if in the cell is the code of a
    quantity.
  -}

  operator 10 $ mdo
    let xf0 = Unknown "xf0"
        xb  = Unknown "xb"

    _   <- comp cellB xb  (op 12) alt
    alt <- comp cellB xf0 (op 11) (op 12)
    return ()
  {-
    Op. 11 transfers the code of the quantity to cellC C
  -}

  operator 11 $ do
    tN cellB cellC
    cccc (op 2)

  {-
    Op. 12 transfers control to op. 13, if in the cell there is
    located the code of an operation symbol for raising to a square or cube.
  -}

  operator 12 $ mdo
    let x1b = undefined

    comp cellB x1b (op 13) alt
    alt <- comp cellB x1c (op 13) (op 19)

    return alt

  {-
    Op. 13 forms the instruction for raising to a square of a quantity, the
    code of which is in cell C.
    Op. 14 transfers the formed instruction to the block of the completed
    operator.
    Op. 15 determines the case of raising to a cube, and transfers to Op. 16

  -}
  let multTemplate = undefined

  operator 13 $ tN multTemplate cellC -- WRONG
  -- insert the quantity from c into both args
  operator 14 $ clcc (op 106)

  operator 15 $ comp cellB x1c (op 16) (op 18)

  {-
    Op. 16 additionally forms the instruction for obtaining the cube, which
    Op. 17 transfers to the block of the completed operator.

  -}

  operator 16 $ tN multTemplate cellC
  -- insert previous cell reference and quantity as args
  operator 17 $ clcc (op 106) >> chain (op 18)
  {-
    Op. 18 transfers the additional code to the working cell with the result
    of the programmed operation to cell c.
  -}

  operator 18 $ do
    tN cellD cellC
    cccc (op 2)

  {-
    Op. 19 transfers control to the sub-routine for testing the presence of a
    single-place operation.
    Op. 20 transfers control to Op. 21 if in cell B is the code of a symbol of
    a single-place operation.
  -}
  let xf0 = undefined

  operator 19 $ callRtc (op 73) (op 78)
  operator 20 $ comp xf0 cellB (op 21) (op 25)

  {-

  Op. 21 determines the case of a single-place operation with parameter (CE_n
  and <-_n), transferring control to op. 22

  Op. 22 selects rom cell A the parameter n and shifts it to the cell B.

  Op. 23 combines the symbol code and the parameter n in a single cell B.

  -}
  mdo
    let xff = Unknown "xff"
        xfd = Unknown "xfd"

    operator 21 $ mdo
      _   <- comp     cellB xfd (op 24) alt
      alt <- compWord cellB xff (op 24) op22
      return ()

    op22 <- operator 22 $ do
      shift cellB (left 11) cellB
      clcc (op 2) -- woo subroutines!

      cccc (op 24)

    return ()
  {-

  Op. 24 transfers (B) to cell D for the last transfer to the partial programme.

  -}

  operator 24 $ do
    tN cellB cellD
    cccc (op 69)

  {-

  Op. 25 transfers control to op. 30, if in cell B is the code of a single
  open-parentheses

  Op. 26 transfers control to op. 27, if in the cell B is the code a multiple
  open-parentheses.

  -}

  mdo
    let singOParenCode = undefined
        multOParenCode = undefined

    _    <- operator 25 $ compWord cellB singOParenCode (op 30) op26
    op26 <- operator 26 $ compWord cellB multOParenCode (op 27) (op 28)

    return ()

  {-
    Op. 27 shifts this code to the first address of cell B.
  -}

  operator 27 $ do
    shift cellB (left 22) cellB
    cccc (op 22)

  {-

  Op. 28 transfers control to op. 29, if in cell B is the code of a symbol for
  the operations multiplication or division.

  -}
  operator 28 $ mdo
    let divCode = undefined
        multCode = undefined

    compWord cellB multCode (op 29) alt
    alt <- compWord cellB divCode (op 29) (op 31)

    return ()
  {-

  Op. 31 determines the case of multiple close-parentheses or the sign of
  correspondence, for which

  -}
  operator 31 $ mdo
    let correspond = undefined
        multCParen = undefined

    comp cellB multCParen (op 32) alt
    alt <- comp cellB correspond (op 32) (op 34)

    return ()
  {-

  Op. 32 determines from cell A the code of the following symbol (the number of
  the close-parentheses or the symbol of the result).

  Op. 33 shifts this code to the second address.

  -}

  operator 32 $ do
    tN cellB cellF

    clcc (op 2)
    chain (op 33)

  operator 33 $ do
    shift cellB (left 11) cellB

    cccc (op 34)

  {-

  Op. 29 reduces the quantity of the code of an operation symbol by 4. The
  necessity of this will be evident from the description of the functioning of
  the sub-routine for programming two-place operations.

  -}

  operator 29 $ do
    sub cellB four cellB
    cccc (op 30)

  {-

  Op. 30 shifts the code of the symbol in the first address of cell D for the
  subsequent trsansfer to the partial programme.

  -}

  operator 30 $ do
    tN cellB cellD
    cccc (op 69)

  {-

  Op. 34 sends to counter B_2 the contents of counter B_1, preparing "shifting
  backwards" over the partial programme.

  -}

  operator 34 $ do
    tN counterB1 counterB2
    chain (op 35)

  {-

  Op. 35 sends the contents of the next cell in the partial programme to cell E.

  -}

  operator 35 $ do
    clcc (op 71)


  {-

  Op. 36 verifies if the contents of cell E constitute the code of a sign
  open-parentheses, addition or subtraction (YEs -- op. 38 functions, NO --
  op.37)

  -}

  operator 36 $ do
    comp four cellE (op 38) (op 37)


  {-

  Op. 37 transfers control to op. 38 , if in "shifting backwards" we have
  arrived at the start of the partial programme.

  NOTE
  Want to compare counterB2 with the _address_ of the start of the partial programme...
  -}

  operator 37 $ do
    compWord partialProgramme counterB2 (op 38) (op 35)

  {-

  Op. 38 programmes the operations of mutlplication and division, the codes of
  signs and components of which are found in the part of the partial programme
  "already examined", using the sub-routine of programming two-place operations.

  -}

  operator 38 $ do
    callRtc (op 80) (op 90)
    chain (op 39)

  {-

  Op. 39 transfers to the partial programme the conditional code of the working
  cell with the result of the programmed operations.

  -}

  operator 39 $ do
    clcc (op 69) -- is this enough? op69 expects the conditional code in cellD
    chain (op 40)


  {-

  Op. 40 carries out further testing of the symbol, the code of which is in cell
  B, determines the cases of operations of addition or subtraction, transferring
  control to op. 30.

  Operators 25 and 26 have eliminated parentheses from consideration (01, 02)

  If 2 < cellB < 5, then op 30, else op 41

  -}

  operator 40 $ mdo
    comp two cellB (op 30) (op 41) -- cellB > 2

  {-

  Op. 41, in the case of the correspondence sign, transfers control to op. 42,
  and in the case of close-parentheses -- to op. 55.

  Because of 40 we only need to check that < 7 and < 9
  28 has eliminated multiplication and division
  -}

  operator 41 $ do
    compWord six cellB (op 42) (op 55)
  {-
  Op. 42 sets in counter B_2 the address of the start of the partial programme.

  TODO: Decide, is partialProgramme the address of the partial programme
    or the address of the address of the partial programme.

  -}

  operator 42 $ do
    tN partialProgramme counterB2
  {-

  Op. 43 with the use of the subroutine for programming two-place operations
  programmes the operations of addition and subtraction, the codes of signs and
  components of which are located in the partial programme.
  -}

  operator 43 $ do
    callRtc (op 80) (op 90)

    chain (op 44)


  {-

  Op. 44 verifies if anything has been transferred to the block of the completed
  operator in programming a given arithmetical operation (YES -- op. 45
  unctions, NO -- op. 47). This test determines the case when the arithmetical
  operator begins with the formula a => y.

  Notes
  =====

  Counter K seems to store the address of the last completed operator.
  This means that if a constant stores the address of the _start_ of the completed
  operators then we can easily check whether any have been stored.

  Confirmed: the original code stored the start address of the block and then compared K to it.
  -}

  operator 44 $ do
    let pointerToCompletedOps = Unknown ("&completed operator")
    compWord counterK pointerToCompletedOps (op 45) (op 47)

  {-

  Op. 45 selects the last instruction from the block of the completed operator
  and extracts its third address.

  -}

  operator 45 $ mdo
    let addr3bitmask = Unknown "0x7FF" -- 0x7FF

    shift counterK (left 22) cellF
    ai cellF addr addr
    addr <- bitAnd completedOperator addr3bitmask cellF

    chain (op 46)
  {-

  Op. 46  compares the contents of the selected address with zero. If it is
  different from zero, this denotes that in programming the formula to which the
  selected sign => relates, no instruction has been transferred to the block of
  the completed operator, which is possible if the formula has the form a => y
  or if the right part of the formula is completely included in the already
  programmed expression. In these cases either the code of the quantity a or the
  contitional code r of the working cell with the result of the programmed
  expression is in cell D, and then control is transferred to op. 47.

  -}
  operator 46 $ do
    comp cellF zero (op 48) (op 47)

  {-

  Op. 47 forms the instruction
    ┌───┬───┬───┬───┐
    │ T │ x │   │.  │
    └───┴───┴───┴───┘,

    where x is equal to "a" or r.

  -}
  let completedInstr = Unknown "transfer cell" -- cell taht's used to transfer to block 106

  operator 47 $ do
    let tnTemplate = Unknown "tn template"
    shift cellD (right 22) completedInstr
    ai tnTemplate completedInstr completedInstr

    chain (op 48)
  {-
  Op. 48 shifts the code with the result of the formula to the third address.

  NOTES
  =====

  According to the operator diagram, no routine is called to select the next code
  segment from the input... This means it's farily unclear how we're supposed to
  get the code of the result which could be on separate line of information!

  -}

  let resultCode = Unknown "how the fuck do i get this"
  operator 48 $ do
    shift cellB (left 11) resultCode

  {-
  Op. 49 verifies if this code is equal to zero (YES -- op. 50 functions, NO --
  op. 51).
  -}
  operator 49 $ do
    compWord resultCode zero (op 51) (op 50)

  {-
  Op. 50, in accordance with the coding rules (sec 8), forms the instruction for
  printing the result of the formula.
  -}
  operator 50 $ do
    let pnTemplate = Unknown "pn template"
    ai completedInstr pnTemplate completedInstr
    chain (op 51)
  {-
  Op. 51 places the code with the result in the third address of the last
  instruction of the programme of the formula.
  -}

  operator 51 $ do
    ai completedInstr resultCode completedInstr

    chain (op 52)
  {-
  Op. 52 determines the case of the symbol =>, for which
  -}

  operator 52 $ do
    let eight = Unknown "8"
    compWord cellB eight (op 53) (op 54)

  {-
  Op. 53 places inhibition of normalization in the operation code of the last
  isntruction.
  -}
  operator 53 $ do
    let inhibitFlag = Unknown "inhibition flag"
    ai completedInstr inhibitFlag completedInstr

    chain (op 54)
  {-

  Op. 54 sends the last instruction, in the third address of which is placed the
  code of the result, to the block of the completed operator and transfers
  control to selection of the next symbol of the arithmetical operator.

  -}

  operator 54 $ do
    clcc (op 106) -- CLCC or callRtc?

    chain (op 2)

  {-
  Operaotrs 55-68 function if in cell B there is locatred the code of a single
  or multiple close-parentheses.

  Op. 55 sends to counter B_2 the ocntents of coutner B_1 preparing "shifting
  backwards" over the partial programme.

  -}

  operator 55 $ do
    tN counterB2 counterB1

    chain (op 56)

  {-

  Op. 56 sends the contents of the next cell of the partial programme to cell E.

  -}

  operator 56 $ do
    clcc (op 71)
    chain (op 57)

  {-
  Op. 57 tests the code fo the next symbol from the partial programme and refers
  to op. 56 as logn as the code of a single or multiple open-parentheses does
  not appear in cell E.
  -}

  operator 57 $ do
    let two = Unknown "2"

    tN cellE cellF
    shift cellF (right 22) cellF

    comp two cellF (op 56) (op 58)

  {-

  Op. 58 programmes the operatio of addition and subtraction standing in
  parentheses with the aid of the sub-routine for programming single-place
  operations.

  -}

  operator 58 $ do
    callRtc (op 80) (op 90)
    chain (op 59)

  {-

  Op. 59 subtracts 1 from coutner B_1, "erasing" by this the open-parenthses.

  -}

  operator 59 $ do
    sub counterB1 one counterB1
    chain (op 60)

  {-
  Op. 60 determines the case of m-multiple open-parentheses, transferring
  conrtol to op. 61.

  Notes

  By Op. 57, cell e is either a multiple or single open paren.

  -}
  operator 60 $ do
    compWord two cellE (op 64) (op 61)

  {-
  Op. 61 reduces m by 1.

  The value in cell e will have the form:
    ┌───┬────┬───┬───┐
    │ 0 │ 02 │ m │ 0 │
    └───┴────┴───┴───┘

    where 0 < m < 2^11

    The idea is to use "negative address modification constants" to modify the value of the second address, directly!

    0000 00 | 000 0000 0010 000 0000 0001 000 0000 0000 = 0x800800

    0000 00 | 111 1111 1111 111 1111 1111 000 0000 0000  = 0x1FFFFF800

  AI =
    0000 00 | 000 0000 0010 000 0000 0000 000 0000 0000  = 0x800000
  -}

  operator 61 $ do
    let aiConstant = Unknown "op 61-1" -- address of cell holding 0x1FFFFF800

    ai cellE aiConstant cellE
    chain (op 62)

  {-
  Op. 62 transfers control to op. 64 if m = 0.

  If we m = 0 then we have the value

    ┌───┬────┬───┬───┐
    │ 0 │ 02 │ 0 │ 0 │ => hex: 0x800000
    └───┴────┴───┴───┘

  -}

  operator 62 $ do
    let compVal = Unknown "0x800000"

    compWord cellE compVal (op 64) (op 63)

  {-
  Op. 63 sends to the partial programme the code of the multiple
  open-parentheses with m reduced by 1.

  -}
  operator 63 $ do
    tN cellE cellD
    clcc (op 69)
    chain (op 64)


  {-
  Op. 64 determines the case of n-multiple close-parentheses, transferring
  control to op. 65

  -}

  operator 64 $ do
    compWord cellE six (op 67) (op 65)
  {-

  Op. 65 reduces n by 1.

  -}

  operator 65 $ do
    let aiConstant = Unknown "ai constant"-- address of cell holding 0x1FFFFF800
    ai (Unknown "op 65-1") aiConstant (Unknown "op 65-2")
    chain (op 66)

  {-
  Op. 66 transfers control to op. 67, if n = 0.

    ┌───┬────┬───┬───┐
    │ 0 │ 06 │ 0 │ 0 │ => hex: 0x1800000
    └───┴────┴───┴───┘

  -}

  operator 66 $ do
    let compVal = Unknown "comparison value" --  0x1800000

    compWord cellE compVal (op 68) (op 67)


  {-

  Op. 67 sends the code of the result of the programmed operation within
  parentheses to cell C.
  -}

  operator 67 $ do
    tN' cellD cellC
    chain (op 2)
  {-
  For n != 0

  Op. 68 transfers the code of the result of the programmed operation in
  parentheses to cell C and transfers control to the sub-routine testing the
  presence of a single-place operation, which may be located before the
  open-parentheses. After this control is again transferred to op. 34.

  Let us now consider the functioning of the sub-routine of the block of
  arithmetical operators.
  -}

  operator 68 $ do
    tN' cellD cellC
    callRtc (op 73) (op 79)

  {-

  Op. 69 transfers (D) to the partial programme and adds 1 to counter B_1.

  rofl this operator is bonkers... the idea is to use the AI operation to modify the
  _next_ instruction and change the address we want to set based off counterB1!
  -}

  operator 69 $ mdo
    add' one counterB1 counterB1
    ai counterB1 addr addr
    addr <- tN cellD (Absolute 0)

    chain (op 70)
  {-
  Op 70. forestalls "overflow" of the partial programme. If there are more than 32
  symbols in the partial programme a check stop takes place.

  -}

  operator 70 $ mdo
    let thirtyTwo = Unknown "32"

    comp counterB1 thirtyTwo stop jcc'

    stop <- block (checkStop)

    jcc' <- block (jcc >> cccc (op 2))

    return ()
  {-
  Op 71. is the first-sub-routine of selection from the partial programme,
  transferring to cell E the contents of the next cell of the partial programme
  and subtracticing 1 from counter B_2.

  -}
  operator 71 $ mdo
    shift counterB2 (left 22) cellE
    ai trans cellE trans
    trans <- tN (Absolute 0) cellE
    sub' counterB2 one counterB2
    jcc

  {-
  Op 72. is the second sub-routine of selection from the partial programme,
  transferring to cell D the contents of the next cell of the partial programme
  and adding 1 to the counter B_3.
  -}
  operator 72 $ mdo
    shift counterB3 (left 22) cellD
    ai trans cellD trans
    trans <- tN (Absolute 0) cellD
    add' counterB3 one counterB3
    jcc

  {-
  Operators 73 - 78 constitute the sub-routine for testing the presence of a
  single-place operation. This sub-routine realizes operator 6 in the scheme of
  the algorithm for programming formulae (section 9).

  Op. 73 compares (C) with zero. IF (C) = 0, exixt from the subroutine takes
  place.
  -}
  operator 73 $ mdo
    compWord cellC zero (op 74) (RTC (op 79))
    return ()
  {-
  Op. 74 sends (C) to cell D and the contents of counter B_1 to the counter B_2.
  -}

  operator 74 $ do
    tN' cellC cellD
    tN' counterB1 counterB2
  {-
  Op. 75 transfers the contents of the next cell of the partial programme to cell
  E.
  -}

  operator 75 $ do
    clcc (op 71)
    chain (op 76)

  {-
  Op. 76 transfers control to op. 77 if in cell E is the code of the symbol of a
  single-place operation and to op. 77 in the contrary case.
  -}

  operator 76 $ do
    let xf0 = Unknown "xf0"
    comp xf0 cellE (op 77) (op 79)
  {-
  Op. 77 programmes a single-place operation according to the codes of the
  argument and the sign of the operation located simultaneously in cells D and E.
  The conditional code of the working cell with the result of the programmed
  operation is sent to cell D.
  -}
  operator 77 $ do
    callRtc (op 91) (op 122)
    chain (op 78)

  {-
  Op 78. subtracts  1 from counter B_1, "erasing" the programme operation.
  -}

  operator 78 $ do
    sub' counterB1 one counterB1
    chain (op 75)

  {-
  Op 79. transfers (D) to the partial programme and clears counter C.
  -}

  operator 79 $ do
    clcc (op 79)
    tN' zero cellC

    retRTC

  {-
  The sub-routine for programming two-palce operations (operators 80 - 90)
  programmes expressions existing in coded form in the partial programme. The
  boundaries of this expression are the addressses of the cells in counters B_2
  and B_1. At the start of functioning of the sub-routinethe programmed
  expression represents a sequence of quantities separated by the signs +, - or
  x, :.

  Op. 80 transfers the contents of counter B_2 to counter B_3.
  -}

  operator 80 $ do
    tN' counterB2 counterB3
    chain (op 81)
  {-
  Op. 81 selects from cell D the code of the first quantity of the programmed
  expression.
  -}

  operator 81 $ do
    clcc (op 72)
    chain (op 82)

  {-
  Op. 82 sets (D) in the first address of the formed instruction (this will be
  the first component of the first two-place operation) and transfers control to
  op. 89, determining the end of functioning of the sub-routine. This also takes
  into account the case where the programmed expression consists only of a single
  quantity.
  -}
  let formedInstruction = Unknown "formed instruction"
  operator 82 $ do
    shift cellD (right 22) formedInstruction
    chain (op 89)
  {-
  Op. 83 selects from cell D the code of the sign of the two-place operation.
  -}

  operator 83 $ do
    clcc (op 72)
    chain (op 84)

  {-
  Op. 84 forms the code of the operation of the instruction being constructed,
  according to the sign of the operation. Since the codes of multiplication and
  division signs were previously reduced by four, the codes of symbols of
  two-place operations differ from the codes of operations of the corresponding
  instruction by the same quantity, equal to 2.

  NOTES
  =====

  In the partial programme, the codes of two-place operations are stored in the
  first address of the cell. This means that
  -}

  operator 84 $ do
    let twoShifted = Unknown "2 << 22"
    sub' cellD twoShifted cellD
    shift' cellD (left 11) cellD

    ai cellD formedInstruction formedInstruction

    chain (op 85)

  {-
  Op. 85 selects from cell D the code of the second componenet of the two-place
  operation.
  -}

  operator 85 $ do
    clcc (op 72)

    chain (op 86)

  {-
  Op. 86 sets it in the second address of the formed instruction.
  -}

  operator 86 $ do
    shift cellD (left 11) cellD
    ai formedInstruction cellD formedInstruction

    chain (op 87)
  {-
  Op. 87 transfers the formed instruction to the block of the completed operator.
  -}
  operator 87 $ do
    callRtc (op 106) (op 122)

    chain (op 88)

  {-
  Op. 88 sets the conditional code of the working cell with the result of the
  programmed operation in the first address of the cell in which the instruction
  is formed, as the first component of the following two-place operation.
  -}

  operator 88 $ do
    shift cellD (right 22) formedInstruction

  {-
  Op. 89 compares the indications of counters B_3 and B_1 and in the case of
  their agreement transfers control to op. 90.
  -}

  operator 89 $ do
    compWord counterB3 counterB1 (op 83) (op 90)

  {-
  Op. 90 transfers the contents of counter B_2 to counter B_1, "erasing" the
  programmed part of the formula.
  -}
  operator 90 $ do
    tN' counterB2 counterB1
    retRTC

  {-
  The sub-routine for programming single-place operations functions on the
  following principle. In a definite order in the store, correspondng to the
  distribution of symbols of single-place operations in the coding table, are
  distributed the preparations for instructions for carrying out single-place
  operations in such form, that they only lack indications of the addresses of
  the argument (and the quantuty n for instructiosn CEn and <-n). The sub-routine
  locates the necessary intermediate results according to the magnitude of the
  code of the single-place operation and, utilizing the code of the argument
  located in cell D, forms the necessary instruction. For single-place operations
  carried out by means of standard sub-routines in DS, only the intermediate
  result of the instructions of reference to the sub-routine is retained. The
  instructions for dispatching the argument to cell 0001 and producing the result
  from 0002 (from 0003 for the cosine) are formed additionally.

  Op. 91 installs the code of the argument in the first address and the parameter
  (if there is one) in the second address of cell D.

  -}
  let secondAddr = Unknown "2nd-addr"
  operator 91 $ mdo
    shift cellD (left 22) cellD
    bitAnd cellE secondAddr cellF
    ai cellD cellF cellD

    let lowerFour = Unknown "15"
    bitAnd cellE lowerFour cellE

    chain (op 92)

  {-
  Op. 92 determines the case of the operation "sign", for which
  -}

  operator 92 $ do
    let fifteen = Unknown "15"
    comp cellE fifteen (op 94) (op 93)

  {-
  Op. 93 shifts the code of the argument to the second address (s 1).
  -}

  operator 93 $ do
    shift cellD (right 11) cellD
    chain (op 94)
  {-
  Op. 94 forms the instruction for carrying out the single-place operation by
  combining the intermediate results of the instruction with the contents of cell
  D.
  -}
  let outCell = completedInstr
  let builder = Unknown "scratch cell"
  let templateDispatch = Unknown "templateDispatch" -- holds AI template-table cellD builder

  operator 94 $ mdo
    shift cellE (left 22) jumpCell

    ai templateDispatch jumpCell jumpCell
    jumpCell <- empty -- this instruction gets replaced with a tN of the correct instruction into the builder cell

    tN' builder outCell
    chain (op 95)

  {-
  Op. 95 transfers control to op. 106 in the sub-routine for transferring the
  instructions to the block of completed operators and economy of instructions,
  if the single-place operation fulfilled does not require reference to a
  sub-routine in DS. In the contrary case control is transferred to op. 96
  -}
  let nine = Unknown "9"

  operator 95 $ do
    comp nine cellE (op 106) (op 96)

  {-
  Op. 96 forms the instruction for sending the argument to cell 0001.
  -}

  operator 96 $ do
    let temp = Unknown "AI _ _ 0001"

    ai temp cellD outCell
    chain (op 97)

  {-
  Op. 97 determines the sign of the operation "cot", for which:
  -}

  operator 97 $ do
    compWord zero cellD (op 98) (op 99)

  {-
  Op. 98 forms the instruction or dispatching the argument in the form

    ┌───┬──────┬─────┬──────┐
    │ - │ 1101 │ "x" │ 0001 │
    └───┴──────┴─────┴──────┘

    where 1101 is the address of the constant pi / 2 (cot is calcuated according
    to the formula cot x = tan (pi / 2 - x)).
  -}

  operator 98 $ do
    let temp = Unknown "- 1101 _ 0001"

    shift cellD (right 11) cellD
    ai temp cellD outCell

    chain (op 99)


  {-
  Op 99. transfers the completed instruction for dispatch of the argument to the
  block of the completed operator.
  -}

  operator 99 $ do
    clcc (op 106)
    chain (op 100)

  {-
  Op 100 and op 101. transfers the instruction for reference to the sub-routine
  in DS to the block of the completed operator.
  -}

  operator 100 $ do
    tN' builder outCell -- we haven't used the builder again so the original instruction is still there
    clcc (op 107)

    chain (op 102)
  {-
  Op. 102 sets, in the standard cell for the succeeding transfer to the block of
  the completed operator, the instruction

    ┌───┬──────┬─────┬──────┐
    │ T │ 0002 │     │      │
    └───┴──────┴─────┴──────┘
  -}

  operator 102 $ do
    let temp = Unknown "T 0002 _ _"

    tN' temp outCell

    chain (op 103)
  {-
  Op. 103 determines the sign of the operation "Cos", for which control is
  transferred to op. 104
  -}

  operator 103 $ do
    comp cellE nine (op 105) (op 104)

  {-
  Op. 104 sets in the standard cell the instruction

    ┌───┬──────┬─────┬──────┐
    │ T │ 0003 │     │      │
    └───┴──────┴─────┴──────┘
  -}

  operator 104 $ do
    ai outCell (Unknown "1-first-addr") outCell

    chain (op 105)
  {-
  Op. 105 transfers to the block of the completed operator the instruction for
  obtaining the result from 0002 or 0003, and then refers to op. 111 for economy
  of instruction.

  -}

  operator 105 $ do
    clcc (op 107)

    chain (op 111)

  {-
  The sub-routine for transferring instructions to the block of competed operator
  and economy of instructions (op 106 - 122) functions according to the following
  principle.

  In transferring the instructions the indication of counter K is fixed in a
  certain cell and then increased by unity. The transferred instruction is
  compared with instructions standing in the block of the completed operator. If
  it agrees with one of them (with regard to the possibility of interchange of
  addresses in instructions carrying out commutative operations), while the codes
  in its first and second addresses do not figure as the codes of the resutls of
  formulae of the given arithemtical operator, the transferred instruction is
  economize. Then the stored indiction is placed in counter K while for the
  conditional code of the result of the constructed instruction the address is
  taken agreeing with that of the instruction in the block of the completed
  operator. In the contrary case the new indication is left in the counter, which
  is transferred as the conditional code of the result of cell D.

  In transferring the instructions for carrying out single-place operations
  employing a sub-routine from DS the indication of the counter is fixed only in
  transferring the fist instruction. The transfer of all three instructions to
  the block of the completed operator takes place under local control. As is
  evident from the scheme, such transfer is not accompanied by economy of
  instructions. Economy is carried out at one time for the three instructions
  after transfer of the last instruction.

  Op 106. fixed the indication of counter K.
  -}

  let fixedCounter = Unknown "fixed K"

  operator 106 $ do
    tN' counterK fixedCounter

    chain (op 107)

  {-
  Op 107. adds unity to counter K and forms the instruction for transfer to the
  block of the completed operator.
  -}

  operator 107 $ do
    ai counterK one counterK

    chain (op 108)

  {-
  Op. 108 tests for "overflow" of the block of the completed operator (it
  occupies 160 cells of IS). In the case of "overflow" a check stop takes place.
  -}

  operator 108 $ mdo
    let maxK = Unknown "&K + 160"

    compWord counterK maxK (op 109) addr

    addr <- block checkStop

    chain (op 109)

  {-
  Op. 109 transfers the instruction to the block fo the completed operator.
  -}

  let transferTemplate = Unknown "transfer template" -- holds ,TN outCell _
  operator 109 $ mdo
    ai transferTemplate counterK transferI

    transferI <- empty

    jcc
    chain (op 110)

  {-
  Op. 110 stores the transferred instruction in the standard cell. This operator
  does not function for instructions of single-place operations utilizing the
  sub-routine DS since there is already in the indicated standard cell the
  instruction for reference to the sub-routine, which is compared with the
  instructions in the block of the completed operator.
  -}

  operator 110 $ do
    tN' outCell builder

    chain (op 111)

  {-
  Op. 111 forms the initial form of the instruction for selection from the block
  of the completed operator.
  -}
  mdo
    operator 111 $ do
      let kComp = Unknown "k comp" -- ,< 0001 builder 112

      shift fixedCounter (left 22) cmp

      ai kComp cmp cmp

      tExp builder (Unknown "trans-opcode")
      chain (op 112)

    {-
    Op. 112 selects the next instruction from the block of the completed operator,
    beginning with the next to the last instruction.
    -}

    operator 112 $ mdo
      ai _ negativeOne _

      chain (op 113)
    {-
    Op. 113 verifies if all instructions have been taken from the block. After
    termination of selection control is transferred to the instruction JCC located
    before op. 120.
    -}
    operator 113 $ do
      let firstK = Unknown "first-k-cell"
      compMod cmp firstK (op _) (op 114)

    {-
    Op. 114 verifies if the third address of the next instruction from the block of
    the completed operator coincides with the first or second address of the tested
    instruction (YES -- op. 122 functions, NO -- op. 115).
    -}

    operator 114 $ mdo
      let lastTested = Unknown "3rd-addr-of-tested"
      let scratch = Unknown "scratch-cell"
      let testedCell = Unknown "tested-cell"

      shift (op 115) (right 22) testedCell

      bitAnd builder thirdAddr lastTested
      bitAnd testedCell secondAddr scratch

      shift scratch (right 11) scratch

      compWord scratch lastTested secondTest (op 122)

      secondTest <- block $ do
        b <- shift testedCell (right 22) scratch
        compWord scratch lastTested (op 115) (op 122)
        return b

    {-
    Op. 115 compares the tested instruction with the next instruction from the
    block of the completed operator, transferring control to op. 116 in case of
    agreement.
    -}

    cmp <- operator 115 $ do
      _ <- empty
      return _

  {-
  Op. 116 transfers to the third address of cell D the address of the instruction
  in the block of the completed operator coinciding with the tested instruction,
  which constitues the conditional code of the working cell with the rsult of the
  tested instruction.
  -}

  operator 116 $ do
    shift (op 115) (right 22) cellD

    chain (op 117)

  {-
  Op. 117 determines the case when the coindiding instructions constitute CLCC to
  the sub-routine in DS, transferring conrtol to op. 118.
  -}

  operator 117 $ do
    let clccCode = Unknown "CLCC"
    let opCode   = Unknown "trans-opcode"
    compWord opCode clccCode (op 119) (op 118)

  {-
  Op. 118 increases the third address in cell D by 1, since the code of the
  result in this case is found in the following instruction calling up exit from
  the sub-routine.
  -}

  operator 118 $ do
    ai cellD one cellD

    chain (op 119)
  {-
  Op. 119 transfers to counter K the stored indication, then calling up exit from
  the sub-routine.
  -}

  operator 119 $ do
    tN' fixedCounter counterK

    chain _exit

  {-
  Op. 120 determines the case of commutative operations of multiplication and
  addition, in which case control is transferred to op. 121
  -}

  operation 120 $ do
    comp two opCode (op 121) (op 122)

  {-
  Op. 121 interchange the first and second addresses of the tested instruction
  and transfers control to op. 111 with transfer to the local system of control,
  since after repeating the test of the instruction JCC gives control to op. 122.
  -}

  operation 121 $ do
    bitAnd builder firstAddr _
    shift builder (left 11) _b

    shift _ (right 11) _

    ai _b _ _b

    tSign _b builder builder

    clcc (op 111)
  {-
  Op. 122 transfers the indication of counter K to the third address of cell D
  and realizes exit from the sub-routine.
  -}

  operator 122 $ do
    tN' counterK cellD
    retRTC


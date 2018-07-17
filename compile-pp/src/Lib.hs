{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving, RecursiveDo #-}
module Lib
    ( someFunc
    ) where

import Monad

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

cellA :: Address
cellA = undefined

cellB :: Address
cellB = undefined

cellC :: Address
cellC = undefined

cellD :: Address
cellD = undefined

cellE :: Address
cellE = undefined

-- Other standard cells (16 standard cells total)

cellF :: Address
cellF = undefined

cellG :: Address
cellG = undefined

cellH :: Address
cellH = undefined

cellI :: Address
cellI = undefined

cellJ :: Address
cellJ = undefined

-- Counters

counterK :: Address
counterK = undefined

counterB1 :: Address
counterB1 = undefined

counterB2 :: Address
counterB2 = undefined

symbolCounter :: Address
symbolCounter = undefined

partialProgramme :: Address
partialProgramme = undefined

left11 = undefined
left22 = undefined

-- Apparently the first addresses of the DS store some constants
zero :: Address
zero = undefined

one :: Address
one = Absolute 0x1081

four = undefined

x1c = undefined

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

op1 = do
  tN zero counterK
  tN partialProgramme counterB1
  tN one symbolCounter
  tN zero cellC
  tN zero (partialProgramme `offAddr` (negate 1))

  chain (op 2)
{-
  Op 2 - 3

  Op. 2 extracts the exponeent of the line of information located in cell A.

  Op. 3 in the case of a non-zero exponent, denoting that the selected line is no
  longer information on an arithmetical opterator, transfers control to the
  block of working-cell economy (III-PP-1).

-}

op23 = do
  tExp cellA cellF
  compWord cellF zero pp_3_1 (op 4)
  where pp_3_1 = undefined
{-
  Op 4. extracts the code of the next symbol of the formula and adds 1 to
  the symbol counter.
-}
op4 = do
  shift cellA left8 cellA
  tMod cellA cellF
  shift cellF right24 cellF

  add one symbolCounter symbolCounter

  cccc (op 5)
  where left8   = undefined
        right24 = undefined
{-
  Op. 5 compares the indication of the symbol counter  with the number 4 and
  in the case of extraction of the last code from the line transfers control
  to Op. 6

  Op. 6 which selects the next line of information from the block and
  transfers it to cell A.

  Op. 7 clears the symbol counter

-}
op5 = mdo
  comp symbolCounter four (op 6) joinP
  joinP <- block $ do
    jcc
    chain (op 8)

  operator 6 $ do
    tN zero symbolCounter

    callRtc mp_1_17
    cccc joinP

  where mp_1_17 = undefined


{-
  Op. 8 transfers the code of the extracted symbol to cell B.

  Op. 9 determines the case of zero symbol code, denoting a gap in the
  formula  coding, transferring control to Op. 2
-}
op89 = do
  add cellF cellB cellB
  compWord zero cellB (op 2) (op 10)

{-
  Op 10 transfers control to Op. 11, if in the cell is the code of a
  quantity.
-}

op10 = mdo
  _   <- comp cellB xb  (op 12) alt
  alt <- comp cellB xf0 (op 11) (op 12)

  cccc (op 12)
  where xf0 = undefined
        xb  = undefined
{-
  Op. 11 transfers the code of the quantity to cellC C
-}

op11 = do
  tN cellB cellC
  cccc (op 2)

{-
  Op. 12 transfers control to op. 13, if in the cell there is
  located the code of an operation symbol for raising to a square or cube.
-}

op12 = mdo
  comp cellB x1b (op 13) alt
  alt <- comp cellB x1c (op 13) (op 19)

  return alt
  where x1b = undefined

{-
  Op. 13 forms the instruction for raising to a square of a quantity, the
  code of which is in cell C.
  Op. 14 transfers the formed instruction to the block of the completed
  operator.
  Op. 15 determines the case of raising to a cube, and transfers to Op. 16

-}

op131415 = do
  tN multTemplate cellC
  -- insert the quantity from c into both args
  clcc (op 106)

  comp cellB x1c (op 16) (op 18)

  where multTemplate = undefined
{-
  Op. 16 additionally forms the instruction for obtaining the cube, which
  Op. 17 transfers to the block of the completed operator.

-}

op1617 = do
  tN multTemplate cellC
  -- insert previous cell reference and quantity as args
  cccc (op 106)
  where multTemplate = undefined
{-
  Op. 18 transfers the additional code to the working cell with the result
  of the programmed operation to cell c.
-}

op18 = do
  tN cellD cellC
  cccc (op 2)

{-
  Op. 19 transfers control to the sub-routine for testing the presence of a
  single-place operation.
  Op. 20 transfers control to Op. 21 if in cell B is the code of a symbol of
  a single-place operation.
-}

op1920 = do
  callRtc (op 73)
  comp xf0 cellB (op 21) (op 25)

  where xf0 = undefined
{-

Op. 21 determines the case of a single-place operation with parameter (CE_n
and <-_n), transferring control to op. 22

Op. 22 selects rom cell A the parameter n and shifts it to the cell B.

Op. 23 combines the symbol code and the parameter n in a single cell B.

-}
op212223 = mdo
  _   <- comp     cellB xfd (op 24) alt
  alt <- compWord cellB xff (op 24) op22

  op22 <- operator 22 $ do
    shift cellB left11 cellB
    clcc (op 2) -- woo subroutines!

    cccc (op 24)

  return ()
  where xff = undefined
        xfd = undefined
{-

Op. 24 transfers (B) to cell D for the last transfer to the partial programme.

-}

op24 = operator 24 $ do
  tN cellB cellD
  cccc (op 69)

{-

Op. 25 transfers control to op. 30, if in cell B is the code of a single
open-parentheses

Op. 26 transfers control to op. 27, if in the cell B is the code a multiple
open-parentheses.

-}

op2526 = mdo
  _    <- operator 25 $ compWord cellB singOParenCode (op 30) op26
  op26 <- operator 26 $ compWord cellB multOParenCode (op 27) (op 28)

  return ()

  where singOParenCode = undefined
        multOParenCode = undefined

{-
  Op. 27 shifts this code to the first address of cell B.
-}

op27 = operator 27 $ do
  shift cellB left22 cellB
  cccc (op 22)

{-

Op. 28 transfers control to op. 29, if in cell B is the code of a symbol for
the operations multiplication or division.

-}
op28 = operator 28 $ mdo
  compWord cellB multCode (op 29) alt
  alt <- compWord cellB divCode (op 29) (op 31)

  return ()
  where divCode = undefined
        multCode = undefined
{-

Op. 31 determines the case of multiple close-parentehses or the sign of
correspondence, for which

-}
op31 = mdo
  comp cellB multCParen (op 32) alt
  alt <- comp cellB correspond (op 32) (op 34)

  return ()
  where correspond = undefined
        multCParen = undefined
{-

Op. 32 determines from cell A the code of the following symbol (the number of
the close-parentheses or the symbol of the result).

Op. 33 shifts this code to the second address.

-}

op32 = operator 32 $ do
  tN cellB cellF

  clcc (op 2)

  shift cellB left11 cellB
  add cellF cellB cellB

  cccc (op 32)

{-

Op. 29 reduces the quantity of the code of an operation symbol by 4. The
necessity of this will be evident from the description of the functioning of
the sub-routine for programming two-place operations.

-}

op29 = do
  sub cellB four cellB
  cccc (op 30)

{-

Op. 30 shifts the code of the symbol in the first address of cell D for the
subsequent trsansfer to the partial programme.

-}

op30 = do
  tN cellB cellD
  cccc (op 69)

{-

Op. 34 sends to counter B_2 the contents of counter B_1, preparing "shifting
backwards" over the partial programme.

-}

op34 = operator 34 $ do
  tN counterB1 counterB2
  chain (op 35)

{-

Op. 35 sends the contents of the next cell in the partial programme to cell E.

-}

op35 = operator 35 $ do
  clcc (op 71)


{-

Op. 36 verifies if the contents of cell E constitute the code of a sign
open-parenteheses, addition or subtraction (YEs -- op. 38 functions, NO --
op.37)

-}

op36 = operator 36 $ do
  comp four cellE (op 38) (op 37)


{-

Op. 37 transfers control to op. 38 , if in "shifting backwards" we have
arrived at the start of the partial programme.

-}

op37 = operator 37 $ do
  compWord partialProgramme counterB2 (op 38) (op 35)

{-

Op. 38 programmes the operations of mutlplication and division, the codes of
signs and components of which are found in the part of the partial programme
"already examined", using the sub-routine of programming two-place operations.

-}

op38 = operator 38 $ do
  callRtc (op 80)
  chain (op 39)

{-

Op. 39 transfers to the partial programme the conditional code of the working
cell with the result of the programmed operations.

-}

op39 = operator 39 $ do
  clcc (op 69) -- is this enough? op69 expects the conditional code in cellD
  chain (op 40)


{-

Op. 40 carries out further testing of the symbol, the code of which is in cell
B, determines the cases of operations of addition or subtraction, transferring
control to op. 30.

Operators 25 and 26 have eliminated parentheses from consideration (01, 02)

If 2 < cellB < 5, then op 30, else op 41

-}

op40 = operator 40 $ mdo
  comp two cellB (op 30) (op 41) -- cellB > 2
  where two = undefined

{-

Op. 41, in the case of the correspondence sign, transfers control to op. 42,
and in the case of close-parentheses -- to op. 55.

Because of 40 we only need to check that < 7 and < 9
28 has eliminated multiplication and division
-}

op41 = operator 41 $ do
  compWord six cellB (op 42) (op 55)
  where six = undefined
{-
Op. 42 sets in counter B_2 the address of the start o the partial programme.

TODO: Decide, is partialProgramme the address of the partial programme
  or the address of the address of the partial programme.

-}

op42 = operator 42 $ do
  tN partialProgramme counterB2
{-

Op. 43 with the use of the subroutine for programming two-place operations
programmes the operations of addition and subtraction, the codes of signs and
components of which are located in the partial programme.
-}

op43 = operator 43 $ do
  callRtc (op 80)

  chain (op 44)


{-

Op. 44 verifies if anything has been transferred to the block of the completed
operator in programming a given arithmetical operation (YES -- op. 45
unctions, NO -- op. 47). This test determines the case when the arithmetical
oeprator begins with the formula a => y.

-}

op44 = operator 44 $ do
  undefined -- not sure how yet...

{-

Op. 45 selects the last instructioon from the block of the completed operator
and extracts its third address.

-}

op45 = operator 45 $ do
  undefined -- i dont want to do this yet

{-

Op. 46  compares the contents of teh selected address with zero. If it is
different from zero, this dnotes that in programming the formula to which the
selected sign => relates, no instruction has been transferred to the block of
the completed operator, which is possible if the formula has the form a => y
or if the right part of the formula is comppletely included in the already
programmed expression. In these cases either the code of the quantity a or the
contitional code r of the working cell with the result of the programmed
expression is in cell D, and then control is transferred to op. 47.

-}
op46 = operator 46 $ undefined

{-

Op. 47 forms the instruction
  ┌───┬───┬───┬───┐
  │ T │ x │   │.  │
  └───┴───┴───┴───┘,

  where x is equal to "a" or r.

-}
op47 = operator 47 $ do
  undefined

{-
Op. 48 shifts the code with the result of the formula to the third address.

Op. 49 verifies if this code is equal to zero (YES -- op. 50 functions, NO --
op. 51).

Op. 50, in accordance with the coding rules (sec 8), forms the instruction for
printing the result of the formula.

Op. 51 palces the code with the result in the third address of the last
instruction of the programme of the formula.

Op. 52 determines the case of the symbol =>, for which

Op. 53 places inhibition of normalization in the operatio ncode of hte last
isntruction.

Op. 54 sends the last instruction, in the third address of which is placed the
code of the result, to the block of the completed operator and transfers
control to selection of the next symbol of the arithmetical operator.

Operaotrs 55-68 function if in cell B there is locatred the code of a single
or multiple close-parentheses.

Op. 55 sends to counter B_2 the ocntents of coutner B_1 preparing "shifting
backwards" over the partial programme.

Op. 56 sends the contents of the next cell of the partial programme to cell E.

Op. 57 tests the code fo the next symbol from the partial programme and refers
to op. 56 as logn as the code of a single or multiple open-parentheses does
not appear in cell E.

Op. 58 programmes the operatio of addition and subtraction standing in
parentheses with the aid of the sub-routine for programming single-place
operations.

Op. 59 subtracts 1 from coutner B_1, "erasing" by this the open-parenthses.

Op. 60 determines the case of m-multiple open-parentheses, transferring
conrtol to op. 61.

Op. 61 reduces m by 1.

Op. 62 transfers control to op. 64 if m = 0.

Op. 63 sends to the partial programme the code of the multiple
open-parentheses with m reduced by 1.

Op. 64 determines the case of n-multiple close-parentheses, transferring
control to op. 65

Op. 65 reduces n by 1.

Op. 66 transfers control to op. 67, if n.= 0.

Op. 67 sends the code of the result of the programmed operation within
parentheses to cell
-}

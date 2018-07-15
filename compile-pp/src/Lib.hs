{-# LANGUAGE TemplateHaskell, FlexibleContexts, DeriveFunctor, GeneralizedNewtypeDeriving #-}
module Lib
    ( someFunc
    ) where

import Control.Monad.Free
import Control.Monad.Free.TH

someFunc :: IO ()
someFunc = putStrLn "someFunc"

newtype Address = A { unAddr :: Int }
  deriving (Show, Num)

data BesmAsm param
  = Add       Address Address Address param
  | Sub       Address Address Address param
  | Mult      Address Address Address param
  | Div       Address Address Address param
  | AddE      Address Address Address param
  | SubE      Address Address Address param
  | Ce        Address Address Address param
  | Xa        Address Address Address param
  | Xb                        Address param
  | DivA      Address Address Address param
  | DivB                      Address param
  | TN        Address         Address param
  | PN        Address                 param
  | TMin      Address         Address param
  | TMod      Address         Address param -- I think the 'modulus' actually means magnitude of mantissa?
  | TSign     Address Address Address param
  | TExp      Address         Address param
  | Shift     Address         Address param
  | ShiftAll  Address Address Address param
  | AI        Address Address Address param
  | AICarry   Address Address Address param
  | I         Address Address Address param
  | Comp      Address Address Address param
  | CompWord  Address Address Address param
  | CompMod   Address Address Address param
  | Ma        Address Address Address param
  | Mb        Address Address Address param
  | JCC                               param
  | CLCC                      Address param
  | CCCC              Address Address param
  | LogMult   Address Address Address param
  | Stop                              param
  | SwitchStop                        param
  -- | End
  deriving (Functor)

$(makeFree ''BesmAsm)

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

-- Apparently the first addresses of the DS store some constants
zero :: Address
zero = undefined

one :: Address
one = 0x1081

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
  tN _1 counterK
  tN _2 counterB1
  tN one symbolCounter
  tN 0 cellC
  tN 0 (partialProgramme - 1)
{-
  Op 2 - 3

  Op. 2 extracts the exponeent of the line of information located in cell A.

  Op. 3 in the case of a non-zero exponent, denoting that the selected line is no
  longer information on an arithmetical opterator, transfers control to the
  block of working-cell economy (III-PP-1).

-}

op23 = do
  tExp cellA _1
  compWord _1 zero _3pp1

{-
  Op 4. extracts the code of the next symbol of the formula and adds 1 to
  the symbol counter.
-}
op4 = do
  shift cellA _8left cellA
  tMod cellA cellF
  shift cellF _24right cellF

  add one symbolCounter symbolCounter

{-
  Op. 5 compares the indication of the symbol counter  with the number 4 and
  in the case of extraction of the last code from the line transfers control
  to Op. 6

  Op. 6 which selects the next line of information from the block and
  transfers it to cell A.

  Op. 7 clears the symbol counter

-}
op567 = do
  comp symbolCounter _four _mp_1_17
  tN zero symbolCounter

{-
  Op. 8 transfers the code of the extracted symbol to cell B.

  Op. 9 determines the case of zero symbol code, denoting a gap in the
  formula  coding, transferring control to Op. 2
-}
op89 = do
  tN cellF cellB
  compWord zero cellB _op2

{-
  Op 10 transfers control to Op. 11, if in the cell is the code of a
  quantity.
-}

op10 = do
  comp cellB _0xb  _op12
  comp cellB _0xf0 _op11

{-
  Op. 11 transfers control to Op. 13, if in the cell there is located the
  code of an operation symbol for raising to a square or a cube.
-}

op11 = do
  tN cellB cellC
  cCCC _op2

{-
  Op. 12 transfers control to op. 13, if in the cell there is
  located the code of an operation symbol for raising to a square or cube.
-}

op12 = do
  comp cellB _0x1B _op13
  comp cellB _0x1C _op13


{-
  Op. 13 forms the instruction for raising to a square of a quantity, the
  code of which is in cell C.
  Op. 14 transfers the formed instruction to the block of the completed
  operator.
  Op. 15 determines the case of raising to a cube, and transfers to Op. 16

-}

op131415 = do
  tN _multTemplate cellC
  -- insert the quantity from c into both args
  cCCC _op106

  comp cellB _0x1C _op16
  cCCC _op18

{-
  Op. 16 additionally forms the instruction for obtaining the cube, which
  Op. 17 transfers to the block of the completed operator.

-}

op1617 = do
  tN _multTemplate cellC
  -- insert previous cell reference and quantity as args
  cCCC _op106

{-
  Op. 18 transfers the additional code to the working cell with the result
  of the programmed operation to cell c.
-}

op18 = do
  tN cellD cellC

{-
  Op. 19 transfers control to the sub-routine for testing the presence of a
  single-place operation.
  Op. 20 transfers control to Op. 21 if in cell B is the code of a symbol of
  a single-place operation.
-}

op1920 = do
  cCCC _op73

  comp _0xf0 cellB _op21


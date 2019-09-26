{-# LANGUAGE RecursiveDo, BinaryLiterals #-}
module Besm.PP2 where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

{-
  The second part of PP contains 350 instructiosn and 50 constants and
  consists of the following parts: master programe (MP-2), the block for
  processing information on the parameter, or the block for loop formation
  (I-PP-2), the block for constructing control instructions (II-PP-2) and the
  block for distributing control instructions (III-PP-2).

  During the time that PP-2 operates the store contains MP-2 and one of the
  three of its blocks, read as necessary from MD-4 by the master programme, so
  that not more than 256 cells in IS are engaged by PP. Information on the
  problem is located entirely in IS in standard position.

  The master programme carries out the direct analysis of instructions from
  the block of information on the programme (block K) and organizes the
  functioning of PP-2 as a whole. The selection of instructions from block K
  and the filling of blocks α, β and ɣ id carried out by a special sub-routine
  of mP-2, functioning in local control.

  For each of these blocks there is a counter in which is stored the address
  of the last engaged cell of the block or the last selected instruction (for
  block K).

  Let us consider the functioning of the introductory programme.

-}
gammaBuilder   = Unknown "builder"
gammaCounter   = Unknown "ɣ-counter"
gammaTransfer  = Procedure "MP-2" (op 39)

header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7

gammaInitial   = header `offAddr` 6
gammaTransInit = Unknown "ɣ-trans-initial"

initialI = header `offAddr` 1
counterI = Unknown "i"

mp2 = do
  pinned "header" "programme header table" (Size 15)
  pinned "prog" "programme" (Size 750)

  wm <- global "working-cells" (Size 7)

  twentytwo <- local "22" (Val 22)
  twentyfour <- local "24" (Val 24)

  seven' <- global "7'" (Raw 7)

  -- We need a local table of these selectors because the ones in DS are not adjacent, meaning we can't
  -- simply increment between them in a loop. Why? IDK!
  local "addr-selectors" $ Table
    [ Raw $ 0b111_1111_1111 `B.shift` 22
    , Raw $ 0b111_1111_1111 `B.shift` 11
    , Raw $ 0b111_1111_1111 `B.shift` 00
    ]

  let cellJ = Absolute 0x0001

  local "shift-template" (Template (Shift cellJ (right 22) cellJ)) -- shift ? ? ?
  local "select-template" (Template (LogMult (var "selected") (var "addr-selectors") (cellJ))) -- ^ ? ? ?

  shiftIncr <- local "shift-incr" (Raw 0)
  finalSelect <- local "final-select" (Raw 0)

  -- local "p" Cell
  -- local "q" Cell
  -- local "z" Cell
  global "i" Cell

  global "V" Cell
  global "U" Cell

  -- these should all be merged together
  global "ɣ-trans-initial" (Template (TN (var "builder") (Absolute 1) UnNormalized))
  global "β-trans-initial" (Template (TN (var "builder") (Absolute 1) UnNormalized))
  global "α-trans-initial" (Template (TN (var "builder") (Absolute 1) UnNormalized))

  -- same
  global "builder" (Raw 0)

  gammaCounter <- global "ɣ-counter" (Raw 0)
  alphaCounter <- global "α-counter" (Raw 0)
  betaCounter  <- global "β-counter" (Raw 0)

  k <- global "k" Cell

  global "selected" Cell

  global "k-trans-initial" (Template (TN (Absolute 1) (var "selected") UnNormalized))

  {-
    Op. 1 sets counter gamma, the instruction for transfer to block gamma, and
    counter i in the initial positions.
  -}
  operator 1 $ do
    tN' gammaTransInit gammaTransfer
    tN' gammaInitial  gammaCounter

    ai initialI one counterI

    chain (op 31)

  {-

    !! IMPORTANT !!

    DEVIATION FROM BOOK

    In the book, PP-2 only checks for exhaustion of loop parameters at the _end_ of the loop. This means
    that if there are no loop parameters, the program will return incorrect results! While in practice
    no useful programs can be written without using a loop, for correctness' sake I rotated the check to
    the head of the loop so that we properly abort if there is nothing to check!

    ===========

    Op. 31 ensures construction of loops over all parameters of the block P.

    At the completion of operation of PP-2 there remains in IS information on
    the problem in standard position, prepared for funcioning of PP-3. The
      ndication of counter gamma is stored in cell 0006.

  -}

  operator 31 $ mdo
    compMod counterI (header `offAddr` 2) (op 2) exit

    exit <- block $ do
      tN' gammaCounter (header `offAddr` (-1)) -- TO BE FIXED: The header block should probably occupy the first 16 cells like in the book

      cccc (Absolute $ 1025 + 6)

    return ()

  {-
  Op. 2 reads block I-PP-2 from MD-4 (block for loop formation).
  -}
  operator 2 $ do
    readMD 4 (ProcStart "I-PP-2") (ProcEnd "I-PP-2") (ProcStart "I-PP-2")
    chain (op 3)

  {-
  Op. 3 transfers control to block I-PP-2, which processes information on
  the next parameter i and locates the start of the bloop over this parameter
  in block K.
  -}
  operator 3 $ do
    cccc (Procedure "I-PP-2" (op 1))

    chain (op 4)
  {-
  Op. 4 selects the next instruction from the working part of the loop over i.
  -}
  operator 4 $ do
    clcc (op 32)

    chain (op 5)
  {-
  Op. 5 extracts the operation code x from the selected instruction.
  -}
  let nextInstr = Unknown "selected"
  let instrExp = Absolute 0x0001
  operator 5 $ do
    tExp nextInstr instrExp

    chain (op 6)
  {-
  Op. 6 constructs the initial form of the instruction for extracting the
  addresses from the selected instruction, clears cells p and q in which will
  be stored the positive and negative parts of the address-modification
  constant, and cell z, in which will be stored the sign of dependence of the
  instruction on higher-order parameters.
  -}

  let cellP = wm `offAddr` 2
  let cellQ = wm `offAddr` 3
  let cellZ = wm `offAddr` 4

  let cellL = Absolute 0x0002

  let selectTemplate = Unknown "select-template"
  let shiftTemplate = Unknown "shift-template"

  local "select-loop-end" (Template (LogMult (var "selected") (var "addr-selectors" `offAddr` 2) cellJ))

  let outerloopEnd = Unknown "select-loop-end"

  let shiftI = op 9 `offAddr` 1
  let selectI = op 9

  local "shift-back" (Template (Shift cellL (left 22) cellL))

  operator 6 $ do
    tN' zero cellP
    tN' zero cellQ
    tN' zero cellZ

    tN' selectTemplate selectI
    tN' shiftTemplate shiftI

    tN' (var "shift-back") (op 16 `offAddr` 2)

    chain (op 7)
  {-
  Op. 7 transfers control to op. 8 in the case of selection of instruction Ma
  or Mb (x = b:016, dec:22, b:017, dec:23).
  -}

  operator 7 $ mdo
    comp instrExp twentytwo (op 9) o
    o <- comp instrExp twentyfour (op 8) (op 9)

    chain (op 7)
  {-
  In these instructions only the third address may be variable and
  consequently, it is necessary to examine only this, while the contents of
  the first two addresses cannot be tested.

  Op. 8 changes the instruction for extraction of addresses in a corresponding
  manner.
  -}
  operator 8 $ do
    tN' finalSelect selectI
    sub' shiftI shiftIncr shiftI
    -- sub' shiftI shiftIncr shiftI
    chain (op 9)
  {-
  Op. 9 extracts the next address from the selected instruction, beginning
  with the first, and sets it in the third address of a certain standard cell.
  -}
  operator 9 $ do
    empty
    empty

    chain (op 10)

  {-
  Op. 10 comparing the magnitude of the extracted address with the boundaries
  of block V, verifies if this is a variable address ( YES -- op 11, NO -- op
  22).
  -}
  let nextAddr = cellJ
  operator 10 $ mdo
    comp nextAddr seven' (op 22) omgg  -- check if standard cell < 7
    omgg <- comp (addr 8) nextAddr (op 11) (op 22) -- check if va addr < Vmax
    return ()

  {-
  Op. 11 selects information on the variable address according to the
  magnitude of the code in the extracted address.

      6b   8b    8b    8b    8b
    ┌───┬─────┬─────┬─────┬─────┐
    │   │ "a" │ "b" │ "c" │ "o" │
    └───┴─────┴─────┴─────┴─────┘
  -}

  local "va-selectors" $ Table
    [ Raw $ 0b1111_1111 `B.shift` 24
    , Raw $ 0b1111_1111 `B.shift` 16
    , Raw $ 0b1111_1111 `B.shift` 08
    ]

  let cellHead = Absolute 0x0001
  let va = wm
  vaSelect <- local "va-select" (Template (LogMult va (var "va-selectors") cellL))
  vaShift  <- local "va-shift" (Template (Shift cellL (left 24) cellL))

  local "ugh-1" (Template (TN zero va UnNormalized))
  local "ugh-2" (Template (LogMult zero (var "addr-selectors") cellHead))

  local "neg-bit" (Raw $ 1 `B.shift` 32)


  local "end-loop" (Template (LogMult va (var "va-selectors" `offAddr` 2) cellL))

  operator 11 $ mdo
    shift nextAddr (left 22) nextAddr
    ai (var "ugh-1") nextAddr select
    select <- empty

    tN' vaSelect (op 12)
    tN' vaShift (op 12 `offAddr` 1)

    tExp' va (cellL)
    shift (cellL) (left 22) (cellL)

    sub' nextAddr (cellL) nextAddr

    ai (var "ugh-2") nextAddr (op 16)
    tN' shiftTemplate (op 16 `offAddr` 1)

    chain (op 12)

  {-
  Op. 12 selects the next code "l" of the parameter on which this address
  depends from the lines of information on the variable address.

  this is operator 17 in the notes!
  -}
  operator 12 $ mdo
    empty -- bitAnd _ firstAddr l selector shifts each iter...
    empty -- shift it all the way to the right

    chain (op 13)

  {-
  Op. 13 and Op. 15 comparethe code of the parameter with the code of the
  processed parameter i. If "l" = "i", control is transferred to op. 16 while
  in the contrary case to op. 20. Op . 13 determines the case where "i" < "l".
  According to the rules for coding parameters the parameter l will be of
  higher order with regard to the parameter i in this case, about which
  -}
  operator 13 $ do
    comp counterI cellL (op 14) (op 15)

  {-
  Op. 14 makes a corresponding marker in cell z, transferring to it a number
  different from zero.
  -}
  operator 14 $ do
    tN' one cellZ
    chain (op 20)

  operator 15 $ do
    comp cellL counterI (op 20) (op 16)

  {-
  Op. 16 extracts from the head the step over the parameter i.
  -}
  local "lower-10bits" (Raw 0b11_1111_1111)

  operator 16 $ do
    empty -- select the value
    empty -- shift it to the rightmost position
    -- select the right most 10 bits of the word, to take magnitutde!
    bitAnd cellHead (var "lower-10bits") (cellL)
    empty
  {-
  Op. 17 determines the sign of the step.
  -}
  operator 17 $ do
    comp (var "neg-bit") cellHead (op 19) (op 18)

  {-
  If the step is positive, op. 18 functions while if it is negative, op. 19.
  These operators set the absolute magnitude of the step in the
  corresponding address of cell p (op. 18) or cell q (op. 19).
  -}

  operator 18 $ do
    ai cellP cellL cellP
    chain (op 20)

  operator 19 $ do
    ai cellQ cellL cellQ

    chain (op 20)

  {-
  Op. 20 prepares selection of the next parameter code from the information on
  the variable address.
  -}
  local "shift-dec8" (Raw $ 0x8 `B.shift` 11)
  local "shift-dec11" (Raw $ 11 `B.shift` 11)

  operator 20 $ do
    ai (op 12) oneSndAddr (op 12)
    sub' (op 12 `offAddr` 1) (var "shift-dec8") (op 12 `offAddr` 1)

    ai (op 16) oneSndAddr (op 16)
    sub' (op 6 `offAddr` 1) (var "shift-dec11") (op 16 `offAddr` 1)

    chain (op 21)

  {-
  Op. 21 ensures repitition of operators 12 - 20 three times (according to the
  number of parameter codes in the information on the variable address).
  -}
  operator 21 $ do
    comp (var "end-loop") (op 12) (op 12) (op 22)

  {-
  Op. 22 prepares testing of the following address of the instruction.
  -}

  local "shift-back-change" (Template (ShiftAll zero (left 11) zero))

  operator 22 $ do
    ai (op 12) oneSndAddr (op 12)
    sub' (op 16 `offAddr` 2) (var "shift-back-change") (op 16 `offAddr` 2)
    -- Changed from the page 8-419
    sub' (op 12 `offAddr` 1) (var "shift-back-change") (op 12 `offAddr` 1)

    chain (op 23)

  {-
  Op. 23 ensures repitition of operators 9 - 22 three times (according to the
  number of addresses in the instruction).
  -}

  operator 23 $ do
    comp (op 12) (var "select-loop-end") (op 9) (op 24)

  {-
  Op. 24 verifies if the given instruction depends on parameter i (YES -- op.
  25, NO -- op. 27). If the contents of at least one of cells p or q is
  distinct from zero, this constitutes the sign of dependence of the
  instruction on i.
  -}
  operator 24 $ mdo
    compWord cellP zero (op 25) q
    q <- compWord cellQ zero (op 25) (op 27)

    return ()

  {-
  Op. 25 reads blocks II-PP-2 from MD-4.
  -}
  operator 25 $ do
    readMD 4 (ProcStart "II-PP-2") (ProcEnd "II-PP-2") (ProcStart "II-PP-2")

    chain (op 26)

  {-
  Op. 26 transfers control to block II-PP-2, constructing for the variable
  instruction all control instructions relating to it and forming the
  address-modification constant.
  -}
  operator 26 $ do
    cccc (Procedure "II-PP-2" (op 1))

    chain (op 27)

  {-
  Op. 27 verifies if all instructions of the working part of the loop have
  been examined (YES -- control is transferreds to op. 28, NO -- top op. 4).
  The sign of termination of the working part is the selection of the
  close-parentheses of the loop.

    ┌─────┬──────┬──────┬──────┐
    │ 01F │ 13FF │ 13FF │ 13FF │
    └─────┴──────┴──────┴──────┘
  -}
  operator 27 $ do
    stop
  {-
  Op. 28 reads the block III-PP-2 from MD-4.
  -}
  operator 28 $ do
    readMD 4 (ProcStart "III-PP-2") (ProcEnd "III-PP-2") (ProcStart "III-PP-2")
  {-
  Op. 29 transfers control to block III-PP-2. This block transfers all
  constructed control instructions from the standard cells and from blocks
  alpha and beta to the programme, changing the addresses of variable
  instructions to their relative addresses.
-}
  operator 29 $ do
    cccc (Procedure "III-PP-2" (op 1))

    chain (op 30)
{-
  Op. 30 adding unity to counter i, obtains the code of the following
  parameter.
-}
  operator 30 $ do
    ai counterI one counterI

    chain (op 31)

  {-
    Op. 32 transfers cells from K
  -}
  operator 32 $ do
    kTransfer <- empty
    ai kTransfer oneFirstAddr kTransfer
    ai k unity k
    jcc
  {-
  Operators 33-35 form the sub-routine for transferring to block alpha.

  Op. 33 transfers the contents of some standard cell to the next cell of
  block alpha and adds one to counter alpha.
  -}
  operator 33 $ do
    aTransfer <- empty
    ai aTransfer one aTransfer
    ai one alphaCounter alphaCounter
    chain (op 34)
  {-
    Op. 34, comparing alpha_c and alpha_cr, tests block alpha for "overflow".
  -}
  operator 34 $ do
    compWord alphaCounter (Absolute 0xF) (op 32 `offAddr` 3) (op 35)

  {-
    Op. 35 is a check stop for overflow.
  -}
  operator 35 $ do
    checkStop
  {-
    The sub-routine for transferring to block beta (operators 36 -38) and to
    block gamma (operators 39-41) functions identically. As follows from the
    positions of blocks alpha, beta and gamme in IS, it may be remarked that
    gamma_cr = alpha_0, alpha_cr = beta_0, beta_cr = 0255.
  -}

  {-
  Operators 36-38 form the sub-routine for transferring to block beta.

  Op. 36 transfers the contents of some standard cell to the next cell of
  block beta and adds one to counter beta.
  -}
  operator 36 $ do
    bTransfer <- empty
    ai bTransfer one bTransfer
    ai one betaCounter betaCounter
    chain (op 37)
  {-
    Op. 34, comparing beta_c and beta_cr, tests block beta for "overflow".
  -}
  operator 37 $ do
    compWord betaCounter (Absolute 0x000) (op 32 `offAddr` 3) (op 38)

  {-
    Op. 38 is a check stop for overflow.
  -}
  operator 38 $ do
    checkStop
  {-
  Operators 39-41 form the sub-routine for transferring to block gamma.

  Op. 39 transfers the contents of some standard cell to the next cell of
  block gamma and adds one to counter gamma.
  -}
  operator 39 $ do
    gTransfer <- empty
    ai gTransfer one gTransfer
    ai one gammaCounter gammaCounter
    chain (op 40)
  {-
    Op. 34, comparing gamma_c and gamma_cr, tests block gamma for "overflow".
  -}
  operator 40 $ do
    compWord gammaCounter (Absolute 0xE) (op 32 `offAddr` 3) (op 41)

  {-
    Op. 35 is a check stop for overflow.
  -}
  operator 41 $ do
    checkStop

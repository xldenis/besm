{-# LANGUAGE RecursiveDo #-}
module Besm.PP2 where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

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
gammaBuilder   = Unknown "ɣ-builder"
gammaCounter   = Unknown "ɣ-counter"
gammaTransfer  = Procedure "MP-2" (op 39)

header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7

gammaInitial   = header `offAddr` 6
gammaTransInit = Unknown "ɣ-trans-initial"

initialI = header `offAddr` 1
counterI = Unknown "i"

sixteen = Unknown "16"

loader = do
  readMD 4 (ProcStart "MP-2") (ProcEnd "MP-2") (ProcStart "MP-2")
  chain (Procedure "MP-2" (op 1))

mp2 = do
  pinned "header" "programme header table" (Size 15)
  pinned "prog" "programme" (Size 750)
  sixteen <- local "16" (Raw 16)
  seven' <- local "7'" (Raw 7)

  local "shift-template" (Raw 0) -- shift ? ? ?
  shiftIncr <- local "shift-incr" (Raw 0)
  local "select-template" (Raw 0) -- ^ ? ? ?
  finalSelect <- local "final-select" (Raw 0)

  local "p" Cell
  local "q" Cell
  local "z" Cell
  global "i" Cell

  global "V" Cell
  global "U" Cell

  local "next-instr" Cell
  local "instr-exp" Cell

  -- these should all be merged together
  global "ɣ-trans-initial" (Template (TN (var "ɣ-builder") (Absolute 1) UnNormalized))
  global "β-trans-initial" (Template (TN (var "β-builder") (Absolute 1) UnNormalized))
  global "α-trans-initial" (Template (TN (var "α-builder") (Absolute 1) UnNormalized))

  -- same
  global "ɣ-builder" (Raw 0)
  global "α-builder" (Raw 0)
  global "β-builder" (Raw 0)

  gammaCounter <- global "ɣ-counter" (Raw 0)
  alphaCounter <- global "α-counter" (Raw 0)
  betaCounter  <- global "β-counter" (Raw 0)

  k <- global "k" Cell
  global "k-trans-initial" (Template (TN (Absolute 1) (var "k") UnNormalized))

  nextAddr <- local "next-addr" Cell

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
  let nextInstr = Unknown "next-instr"
  let instrExp = Unknown "instr-exp"
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

  let cellP = Unknown "p"
  let cellQ = Unknown "q"
  let cellZ = Unknown "z"

  let selectTemplate = Unknown "select-template"
  let shiftTemplate = Unknown "shift-template"

  let shift = op 9 `offAddr` 1
  let select = op 9

  operator 6 $ do
    tN' zero cellP
    tN' zero cellQ
    tN' zero cellZ

    tN' selectTemplate select
    tN' shiftTemplate shift

    chain (op 7)
  {-
  Op. 7 transfers control to op. 8 in the case of selection of instruction Ma
  or Mb (x = b:016, dec:22, b:017, dec:23).
  -}

  operator 7 $ do
                --- v-- this is wrong number it's actually 22
    comp instrExp sixteen (op 8) (op 9)

  {-
  In these instructions only the third address may be variable and
  consequently, it is necessary to examine only this, while the contents of
  the first two addresses cannot be tested.

  Op. 8 changes the instruction for extraction of addresses in a corresponding
  manner.
  -}
  operator 8 $ do
    tN' finalSelect select
    sub' shift shiftIncr shift
    sub' shift shiftIncr shift
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
  operator 10 $ mdo
    comp nextAddr seven' (op 22) omgg  -- check if standard cell < 7
    omgg <- comp (addr 8) nextAddr (op 11) (op 11) -- check if va addr < Vmax
    return ()

  {-
  Op. 11 selects information on the variable address according to the
  magnitude of the code in the extracted address.
  -}
  operator 11 $ do
    stop

  {-
  Op. 12 selects the next code "1" of the parameter on which this address
  depends from teh lines of information on the variablee address.

  Op. 13 and Op. 15 comparethe code of the parameter with the code of the
  processed parameter i. If "l" = "i", control is transferred to op. 16 while
  in the contrary case to op. 20. Op . 13 determines the case where "i" < "l".
  According to the rules for coding parameters the parameter l will be of
  higher order with regard to the parameter i in this case, about which

  Op. 14 makes a corresponding marker in cell z, transferring to it a number
  different from zero.

  Op. 16 extracts from the head the step over the parameter i.

  Op. 17 determines the sign of the step.

    If the step is positive, op. 18 functions while if it is negative, op. 19.
    These operators set the absolute magnitude of the step in the
    corresponding address of cell p (op. 18) or cell q (op. 19).

  Op. 20 prepares selection of the next parameter code from the information on
  the variable address.

  Op. 21 ensures repitition of operators 12 - 20 three times (according to the
  number of parameter codes in the information on the variable address).
  -}

  {-
  Op. 22 prepares testing of the following address of the instruction.
  -}
  operator 22 $ do
    stop
  {-
  Op. 23 ensures repitition of operators 9 - 22 three times (according to the
  number of addresses in the instruction).

  Op. 24 verifies if the given instruction depends on parameter i (YES -- op.
  25, NO -- op. 27). If the contents of at least one of cells p or q is
  distinct from zero, this constitutes the sign of dependence of the
  instruction on i.

  Op. 25 reads blocks II-PP-3 from MD-4.

  Op. 26 transfers control to block II-PP-3, constructing for the variable
  instruciton all control instructions relating to it and forming the
  address-modification constant.

  Op. 27 verifies if all instructions of the working part of the loop have
  been examined (YES -- control is transferreds to op. 28, NO -- top op. 4).
  The sign of termination of the working part is the selection of the
  close-parentheses of the loop.

    ┌─────┬──────┬──────┬──────┐
    │ 01F │ 13FF │ 13FF │ 13FF │
    └─────┴──────┴──────┴──────┘

  Op. 28 reads the block III-PP-2 from MD-4.

  Op. 29 transfers control to block III-PP-2. This block transfers all
  constructed control instructions from the standard cells and from blocks
  alpha and beta to teh programme, changing the addresses of variable
  instructions to their relative addresses.
-}
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
    ai aTransfer oneFirstAddr aTransfer
    ai unity alphaCounter alphaCounter
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
    ai bTransfer oneFirstAddr bTransfer
    ai unity betaCounter betaCounter
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
  Operators 39-35 form the sub-routine for transferring to block gamma.

  Op. 39 transfers the contents of some standard cell to the next cell of
  block gamma and adds one to counter gamma.
  -}
  operator 39 $ do
    gTransfer <- empty
    ai gTransfer oneFirstAddr gTransfer
    ai unity gammaCounter gammaCounter
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

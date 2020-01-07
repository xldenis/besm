module Besm.PP2.Distribute where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

{-

At the start of functioning of block III-PP-2 there are located in blocks
alpha and beta all control instructions not including loop comparisons and
instructions for changing parameters located in standard cells. The block
III-PP-2 distributes the constructed control instructions, changing the
addresses of variable instructions to their relative addresses. Since transfer
of control instructions from block beta is carries out twice -- the first time
the restoration instructions located "beyond the loop" are transferred, the
second time the address-modification instructions located at the end of the
repetitive part of the loop, the transfer instructions from block beta are
placed in a separate sub-routine (operators 29-38). At the fierst reference to
the sub-routine there takes place transfer from block beta only of the
restoration instructions and in the second only the address-modification
instructions.

In addition, there are two further sub-routines in the block: the sub-routine
for transfer from the programme (op. 39 and op. 40), which transfers.
instructions from the block K, beginning with the last instruction, to the
standard cell and the sub-routine for transfer to the programme (op. 41),
which realizes all transfers of control instructions and "shifted" programme
instructions to block K, also beginning with the last instruction.
-}


pp2_3 = do
  alphaCounter <- extern "α-counter"
  betaCounter  <- extern "β-counter"

  wm <- extern "working-cells"
  pht <- extern "programme header table"
  counterK <- extern "k"

  builtComp <- extern "built-comp"
  incrInstr <- extern "incr-instruction"
  parenCode <- extern "paren-code"

  let
    header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7

    betaInitial  = header `offAddr` 8
    alphaInitial = header `offAddr` 7
    kLast        = header `offAddr` 5


  {-
  Op. 1 determines by how many instructions the programme is increased in
  transferring to it the constructed control instructions, obtaining by this the
  new value of K_f.
  -}

  let temp  = Absolute 0x001
      temp' = Absolute 0x002
      alphaDiff = wm `offAddr` 5
      betaDiff  = wm `offAddr` 6
      kTrans    = wm `offAddr` 2

  local "current" Cell
  local "k-trans-template-1" (Template (TN zero (var "current") UnNormalized))
  local "k-trans-template-2" (Template (TN (var "current") zero UnNormalized))
  operator 1 $ do
    sub' alphaCounter alphaInitial alphaDiff
    sub' betaCounter  betaInitial  betaDiff

    add' alphaDiff betaDiff temp

    ai temp one temp

    shift kLast (left 22) temp'

    -- wtf is happening here?

    tN' kLast kTrans

    ai (var "k-trans-template-1") temp' (op 39) -- stored in op 39???

    ai kLast temp kLast

    ai (var "k-trans-template-2") kLast (op 41)

    tN' kLast (wm `offAddr` 3)


  {-
  Op. 2 compares the new value of K_f with K_cr, equal to gamma_0. When K_f >=
  K_cr a check stop (op. 42) takes place.
  -}
  let kCr = header `offAddr` 6
  operator 2 $ do
    compMod kCr kLast (Procedure "MP-2" (op 35)) (op 3)

  {-
  Op. 3 transfers the next insturction of the programme, beginning with the
  last, to the standard cell.
  -}
  operator 3 $ do
    clcc (op 39)

    chain (op 4)
  {-
  Op. 4 transfers this instruction from the standard cell to its new place in
  correspondence with the new value of K_f.
  -}
  operator 4 $ do
    clcc (op 41)

    chain (op 5)
  {-
  Op. 5 ensures "shift" of all instructions located beyond the close-parentheses
  of the programmed loop.
  -}
  operator 5 $ do
    compMod counterK kTrans (op 3) (op 6)
  {-
  Op. 6 prepares the sub-routine for transfer of restoration instructions.
  -}

  operator 6 $ do
    shift betaCounter (left 22) (wm `offAddr` 4)
    ------- v op 42 is a dummy operator
    tN' (op 42) (op 32)

    chain (op 7)
  {-
  Op. 7 transferring control to the sub-routine, sets the restoration
  instruction before the shifted part of the programme.
  -}
  operator 7 $ do
    callRtc (op 29) (op 38)
  {-
  Op. 8 forms the relative address which should stand in the third address of
  the comparison. It is equal to (K_co - (K_1 + alpha)), where K_co is the new
  address of the comparison in the programme, K_1 is the address of the
  open-parentheses of the loop and alpha is the number of teh control
  instruction in block. alpha.
  -}

  let unnamed = wm `offAddr` 3

  parenCode <- extern "paren-code" -- 0x3EC in source

  local "transfer-cell" Cell -- Addr 0x3EE in source

  operator 8 $ do
    sub' unnamed parenCode temp
    sub' temp alphaDiff temp

           -- v ---
    local "temp-value" (Raw 0x3FF)
    ai   temp (var "temp value") temp -- what does this do??????
    ai  builtComp temp (var "transfer-cell")

    chain (op 9)
  {-
  Op. 9 transfers the loop comparison to the programme.
  -}
  operator 9 $ do
    clcc (op 41)

    chain (op 10)
  {-
  Op. 10 and Op. 11 transfer the instruction for parameter change to the
  programme.
  -}
  operator 10 $ do
    tN' incrInstr (var "transfer-cell")
    chain (op 11)

  operator 11 $ do
    clcc (op 41)
    chain (op 10)
  {-
  Op. 12 prepares the sub-routine for transferring control instructions from the
  block beta for transfer to address-modification instructions.
  -}
  operator 12 $ do
    sub' betaCounter  betaInitial  betaDiff
    tN' (op 43) (op 32)

  {-
  Op. 13 transfers address-modification instructions to the programme, setting
  them in front of the parameter change instruction.
  -}
  operator 13 $ do
    callRtc (op 29) (op 38)
  {-
  Op. 14 carries out address-modification in the sub-routine for transfer from
  the programme, ensuring "erasing" of the close-parentheses of the loop.
  -}
  operator 14 $ do
    clcc (op 40)

  {-
  Op. 15 and op. 16 carry out "shift" of the next instruction of the working
  part of the loop.
  -}
  operator 15 $ do
    clcc (op 39)

  operator 16 $ do
    clcc (op 41)


  {-
  Op. 17 ensures shift of all instructions of the working part of the loop.
  -}
  operator 17 $ do
    comp parenCode kTrans (op 15) (op 18)

  {-
  Op. 18 forms the initial form of the instruction for selection from block
  alpha.

  Op. 19 transfers the next instruction from block alpha to the standard cell.

  Op. 20 determines the case of instruction AI (in the contrary case op. 26
  functions).

  Op. 21 obtains the relative address of the variable instruction. This address
  delta = K - K_AI + alpha, where K is the old address of the variable
  instruction, K_AI is the new address of the instruction AI and alpha is the
  number of the instruction in the block alpha.

  Op. 22 verifies if the first the third addresses in the instruction AI are
  equal (YES -- op. 23, NO -- op. 24).

  Op. 23 substitutes for the address of the variable instruction, located in the
  first and third addresses, its relative address.

  Op. 24 substitutes the third address of instruction AI by the relative address
  of the variable instruction.

  Op. 25 sets teh changed instruction AI in the standard cell.

  Op. 26 transfers teh tested instruction to the programme.

  Op. 27 carries out modification of the address of the transfer instruction in
  op. 19

  Op. 28 ensures transfer to the programme of all instructions from block alpha.

  Operators 29 - 38 formed the sub-routine for transfers from block beta.

  Op. 29 verifies if there is anything in block beta, transferring control to
  exit from the sub-routine if beta_c = beta_0,

  Op. 30 forms the initial form of the instruction for transfer from block beta.

  Op. 31 transfers the next instruction from block betato the standard cell.
  -}

  {-
  Op. 32 "by-passes" transfer of the instruction to the programme in the first
  reference to the sub-routine if an address-modification instruction is in the
  standard cell, while in the second reference, if a restoration instruction is
  in the standard cell.

  =====

  NOTES this is instantiated by a template!
  -}
  operator 32 $ do
    comp zero zero (op 37) (op 32)
  {-
  The address-modification instruction is found as an instruction AI, having 1
  in the eleventh place of the first address (the sign place).

  Op. 33 tests for an instruction AI (in the contrary case op. 36 functions).

  Op. 34 tests for a particular instruction AI

      ┌─────┬──────┬──────┬──────┐
      │ AI  │ 0001 │ 10B9 │ 0001 │
      └─────┴──────┴──────┴──────┘

  (it is not necessary to transform this instruction), transferring control to
  op. 36.

  Op. 35 substitutes for the address of the variable instruction in instruction
  AI its relative address. The relative address Delta = - (K_AI - (K + alpha)),
  where the same notation is employed as in op. 21.

  Op. 36 transfers the instruction from the standard cell to the programme.

  Op. 37 carries out modification of the transfer instruction address in op. 31.

  Op. 38 ensures reptition of operators 31 - 37 the necessary number of times.

  Op. 39 and op. 40 constitute the sub-routine for transferring instructions
  from block K.

  Op. 41 is the sub-routien for transferring instrucitons to block K.

  -}

  operator 42 $ do
    comp (var "current") firstAddr (op 37) (op 42)

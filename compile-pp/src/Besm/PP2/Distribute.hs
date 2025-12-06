{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}

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

Notes:

For such a division of control instructions two storage blcoks are utilized in programming each loop in the PP -- the blocks alpha and beta already mentioned in the first part. All control instructions formed which should be placed before the worrking part of the loop are stored in the block alpha in the same order in which tehy will be located in the programme. Those control instructions which shoudl be set after the workign part of the loop  are stored in blcok beta, with the address-modification instructions which should be located in hte repeititive part of the loop amrked by a specifal sign distinguishing them from other instructions in the block beta. The instructions of comparison and parameter change are stored in standard cells up to their introduction into the programme.

0 | "i"| 0 | 0
dispatch of calculation of i_in
calculation of i_fin*
dispatch of initial values of variable instructions*
working part of loop
address-modification instructions*
+ "i" 1081 "i"
comparison instruction
restoration instructions*

* only when needed


17.

After formation of control instructions they are to be arranged in the programme which is carried out in the following manner.

Let n_1 be the number of control instructions wihcih should be located before the working part of a loop and n_2 the nubmer of control instructions which should be after the working part. Then all instructions in the programme located after the close parentheses of the programmed loop are shifted "down" by n_1 + n_2 - 1 cells.

Directly before them are placed those those instructions from the block beta which should be "after the loop". Before them is placed the comparsion instruction, then the parameter modification instruction before which is placed the address-modification instructions transferred from block beta. After this the working part of the loop is shifted "down" by n_1 cells,  so that it is found directly before the addres-modification instructions. The control instructions transferred from block alpha are placed in the n_1 cells released, which complete teh arrangement of the control instructions. In transferring control instructions to the programme all addresses of variable instructions are made relative. It is easy to see that in transferring control instructions the clsoe parentheses of the programmed loop is eliminated.
-}

pp2_3 = do
  alphaCounter <- extern "α-counter"
  betaCounter <- extern "β-counter"

  wm <- extern "working-cells"
  pht <- extern "programme header table"
  counterK <- extern "k"

  builtComp <- extern "built-comp"
  incrInstr <- extern "incr-instruction"
  parenCode <- extern "paren-code"

  let
    header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7
    betaInitial = header `offAddr` 8
    alphaInitial = header `offAddr` 7
    kLast = header `offAddr` 5

  {-
  Op. 1 determines by how many instructions the programme is increased in
  transferring to it the constructed control instructions, obtaining by this the
  new value of K_f.
  -}

  let temp = Absolute 0x001
      temp2 = Absolute 0x002
      temp3 = Absolute 0x003
      alphaDiff = wm `offAddr` 5
      betaDiff = wm `offAddr` 6
      kTrans = wm `offAddr` 2

  local "current" Cell
  local "k-trans-template-1" (Template (TN zero (var "current") UnNormalized))
  local "k-trans-template-2" (Template (TN (var "current") zero UnNormalized))
  local "first and second mask" (Raw $ 0b11111111111 `B.shift` 22 B..|. 0b11111111111 `B.shift` 11)
  -- local "< current firstaddr op 37" (Template (Comp (var "current") firstAddr (op 37) zero))
  -- current appears to be global because it is used in the Loop module op 14.
  -- local ",TN _ current" (Template (TN zero (var "current") UnNormalized))
  operator 1 $ do
    sub' alphaCounter alphaInitial alphaDiff
    sub' betaCounter betaInitial betaDiff

    add' alphaDiff betaDiff temp

    ai temp one temp

    shift kLast (left 22) temp2

    -- wtf is happening here?

    tN' kLast kTrans

    ai (var "k-trans-template-1") temp2 (op 39) -- stored in op 39???
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
    -- tN' (var "< current firstaddr op 37") (op 32)
    tN' (op 42) (op 32)
    chain (op 7)
  {-
  Op. 7 transferring control to the sub-routine, sets the restoration
  instruction before the shifted part of the programme.
  -}
  operator 7 $ do
    callRtc (op 29) (op 43)
  {-
  Op. 8 forms the relative address which should stand in the third address of
  the comparison. It is equal to (K_co - (K_1 + alpha)), where K_co is the new
  address of the comparison in the programme, K_1 is the address of the
  open-parentheses of the loop and alpha is the number of teh control
  instruction in block. alpha.
  -}

  let unnamed = wm `offAddr` 3

  parenCode <- extern "paren-code" -- 0x3EC in source
  operator 8 $ do
    sub' unnamed parenCode temp
    sub' temp alphaDiff temp

    -- v ---
    local "temp-value" (Raw 0x03FF) -- 10 bits set... why?
    ai temp (var "temp-value") temp -- what does this do??????
    ai builtComp temp (var "current")

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
    tN' incrInstr (var "current")
    chain (op 11)

  operator 11 $ do
    clcc (op 41)
    chain (op 12)
  {-
  Op. 12 prepares the sub-routine for transferring control instructions from the
  block beta for transfer to address-modification instructions.
  -}
  operator 12 $ do
    sub' betaCounter betaInitial betaDiff
    tN' (op 42) (op 32)

  {-
  Op. 13 transfers address-modification instructions to the programme, setting
  them in front of the parameter change instruction.
  -}
  operator 13 $ do
    callRtc (op 29) (op 43)
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
  -}
  operator 18 $ do
    shift alphaCounter (left 22) temp
    ai (var "k-trans-template-1") temp (op 19)

  {-
  Op. 19 transfers the next instruction from block alpha to the standard cell.
  -}

  operator 19 $ do
    empty
  {-
  Op. 20 determines the case of instruction AI (in the contrary case op. 26
  functions).
  -}
  operator 20 $ do
    compMod (var "current") (op 6) (op 26) (op 21) -- what matters is that we compare against a <- shift instruction since the opcode for ai is the following one
    {-
    Op. 21 obtains the relative address of the variable instruction. This address
    delta = K - K_AI + alpha, where K is the old address of the variable
    instruction, K_AI is the new address of the instruction AI and alpha is the
    number of the instruction in the block alpha.
    -}
  operator 21 $ do
    bitAnd (var "current") thirdAddr temp3
    sub' temp3 (wm `offAddr` 3) temp
    add' temp alphaDiff temp
    shift (var "current") (right 22) temp2

  {-
  Op. 22 verifies if the first the third addresses in the instruction AI are
  equal (YES -- op. 23, NO -- op. 24).
  -}
  operator 22 $ do
    compWord temp2 temp3 (op 24) (op 23)
  {-
  Op. 23 substitutes for the address of the variable instruction, located in the
  first and third addresses, its relative address.
  -}
  operator 23 $ do
    shift temp (left 22) temp2
    ai temp temp2 temp -- should form _  Δ _ Δ
    bitAnd (var "current") secondAddr temp3 -- ai _ A_2 _ printout uses addr 307 why?
    chain (op 25)

  {-
  Op. 24 substitutes the third address of instruction AI by the relative address
  of the variable instruction.
  -}
  operator 24 $ do
    bitAnd (var "current") (var "first and second mask") temp3

    chain (op 25)
  {-
  Op. 25 sets teh changed instruction AI in the standard cell.
  -}
  operator 25 $ do
    ai temp3 temp (var "current")

    chain (op 26)
  {-
  Op. 26 transfers teh tested instruction to the programme.
  -}
  operator 26 $ do
    clcc (op 41)
  {-
  Op. 27 carries out modification of the address of the transfer instruction in
  op. 19
  -}
  operator 27 $ do
    ai (op 19) firstAddr (op 19)
    sub' alphaCounter one alphaCounter
    chain (op 28)

  {-
  Op. 28 ensures transfer to the programme of all instructions from block alpha.
  -}
  operator 28 $ mdo
    compMod alphaInitial alphaCounter (op 19) p2

    p2 <- block $ do
      cccc (Procedure "MP-2" (op 30))
    return ()
  {-
  Operators 29 - 38 formed the sub-routine for transfers from block beta.

  Op. 29 verifies if there is anything in block beta, transferring control to
  exit from the sub-routine if beta_c = beta_0,
  -}
  operator 29 $ do
    compMod betaCounter one (op 43) (op 30)
  {-
  Op. 30 forms the initial form of the instruction for transfer from block beta.
  -}
  operator 30 $ do
    ai (var "k-trans-template-1") (wm `offAddr` 4) (op 31)

  {-
  Op. 31 transfers the next instruction from block betato the standard cell.
  -}
  operator 31 $ do
    empty
    chain (op 32)

  {-
  Op. 32 "by-passes" transfer of the instruction to the programme in the first
  reference to the sub-routine if an address-modification instruction is in the
  standard cell, while in the second reference, if a restoration instruction is
  in the standard cell.

  =====

  NOTES this is instantiated by a template!
  -}
  operator 32 $ do
    comp zero zero (op 37) (op 33)
  {-
  The address-modification instruction is found as an instruction AI, having 1
  in the eleventh place of the first address (the sign place).

  Op. 33 tests for an instruction AI (in the contrary case op. 36 functions).
  -}
  operator 33 $ do
    compMod (var "current") (op 6) (op 36) (op 34) -- only thing that should matter is that we compare against a shift instruction
    {-
    Op. 34 tests for a particular instruction AI

        ┌─────┬──────┬──────┬──────┐
        │ AI  │ 0001 │ 10B9 │ 0001 │
        └─────┴──────┴──────┴──────┘

    (it is not necessary to transform this instruction), transferring control to
    op. 36.

    ???????????
    -}
  local "lkadjf" (Template (AI (Absolute 0x001) one (Absolute 0x01))) -- the real code uses (op 27) to do this, why?
  operator 34 $ do
    compWord (var "lkadjf") (var "current") (op 36) (op 35)
  -- compMod _ (var "current") (op 36) (op 35)
  {-
  Op. 35 substitutes for the address of the variable instruction in instruction
  AI its relative address. The relative address Delta = - (K_AI - (K + alpha)),
  where the same notation is employed as in op. 21.
  -}
  operator 35 $ do
    bitAnd (var "current") thirdAddr temp
    sub' (wm `offAddr` 3) temp temp
    sub' temp betaDiff temp
    shift temp (left 22) temp2
    ai temp temp2 temp
    bitAnd (var "current") secondAddr temp2 -- this uses cell 3A7 why?
    {-
    Op. 36 transfers the instruction from the standard cell to the programme.
    -}
  operator 36 $ do
    clcc (op 41)
  {-
  Op. 37 carries out modification of the transfer instruction address in op. 31.
  -}
  operator 37 $ do
    ai (op 31) firstAddr (op 31)
    chain (op 38)
  {-
  Op. 38 ensures reptition of operators 31 - 37 the necessary number of times.
  -}
  operator 38 $ mdo
    compMod betaInitial betaCounter (op 31) (op 43)

  -- 42 is a dummy operator number
  operator 43 $ do
    retRTC
  {-
  Op. 39 and op. 40 constitute the sub-routine for transferring instructions
  from block K.
  -}
  operator 39 $ do
    empty -- filled by template tN' kTrans current
    chain (op 40)

  operator 40 $ do
    ai (op 39) firstAddr (op 39)
    sub' kTrans one kTrans
    jcc
  {-
  Op. 41 is the sub-routine for transferring instructions to block K.

  -}
  operator 41 $ do
    empty
    ai (op 41) negAddrModifConstant (op 41)
    sub' (wm `offAddr` 3) one (wm `offAddr` 3)
    jcc

  -- dummy operator to get a terminator template variable
  operator 42 $ do
    comp (var "current") firstAddr (op 37) zero

-- Comp (var "current") firstAddr (op 37) zero)

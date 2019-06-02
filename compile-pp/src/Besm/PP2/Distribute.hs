module Besm.PP2.Distribute where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

pp2_3 = do
  operator 1 $ do
    stop

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

Op. 1 determines by how many instructions the programme is increased in
transferring to it the constructed control instructions, obtaining by this the
new value of K_f.

Op. 2 compares the new value of K_f with K_cr, equal to gamma_0. When K_f >=
K_cr a check stop (op. 42) takes place.

Op. 3 transfers the next insturction of the programme, beginning with the
last, to the standard cell.

Op. 4 transfers this instruction from the standard cell to its new place in
correspondence with the new value of K_f.

Op. 5 ensures "shift" of all instructions located beyond the close-parentheses
of the programmed loop.

Op. 6 prepares the sub-routine for transfer of restoration instructions.

Op. 7 transferring control to the sub-routine, sets the restoration
instruction before the shifted part of the programme.

Op. 8 forms the relative address which should stand in the third address of
the comparison. It is equal to (K_co - (K_1 + alpha)), where K_co is the new
address of the comparison in the programme, K_1 is the address of the
open-parentheses of the loop and alpha is the number of teh control
instruction in block. alpha.

Op. 9 transfers the loop comparison to the programme.

Op. 10 and Op. 11 transfer the instruction for parameter change to the
programme.

Op. 12 prepares the sub-routine for transferring cotnrol instrucitons from the
block beta for transfer to address-modification instructions.

Op. 13 transfers address-modification isntructions to the programme, setting
them in front of the parameter change instruction.

Op. 14 carries out address-modification in the sub-routine for transfer from
the programme, ensuring "erasing" of the close-parentheses of the loop.

Op. 15 and op. 16 carry out "shift" of the next instruction of the working
part of the loop.

Op. 17 ensures shift of all instructions of the working part of the loop.

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

Op. 32 "by-passes" transfer of the instruction to the programme in the first
reference to the sub-routine if an address-modification instruction is in the
standard cell, while in the second reference, if a restoration instruction is
in the standard cell.

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

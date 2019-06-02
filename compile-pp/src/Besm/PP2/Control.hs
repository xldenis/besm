module Besm.PP2.Control where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

pp2_2 = do
  operator 1 $ do
    stop

{-

The block II-PP-2 constructs all control instrucitons relating to variable
instructions. The initial data for this constitute: the address of the
variable instruction in block K locate in counter K; the contents of cells p
and q, from which will be constructed teh address-modification constant; the
contents of cell z, determining if the given instruction depends on higher
order parametrs; the quantitiy "i_in", from which it may be determined if i_in
is equal to zero. From the contentss of cells U and V for markers on transfer
of the denormalization instructions for i_in and i_Fin to the programme it is
possible to determine if it is necesssary, in constructing the instructions
for formation and restoration for the given variable instruction, to transfer
the instruction for denormalization of the parameter to the programme.

As a result of the functioning of block II-PP-2 the address-modification and
restoration instructions will be transferred to block beta, while to block
alpha the instruction for dispatching or forming variable instructions.

Op. 1 forms the address-modification constant.

Op. 2 transfers control to the sub-routine verifying if tehre is such a
constant among the positive address-modification constants of IS.

  If there is such a constant then the sub-routine places it in the standard
  cell the address of the address-modification constant, transferring control
  to op. 9 In the contrary case the sub-routine realizes return transfer of
  control in the ordinary manner.

Op. 3 prepares the sub-routine for verifying the presence of the constructed
address-modification constant among the negative constants of IS.

Op. 4 transfers control to the sub-routine.

Op. 5 prepares the sub-routine for verifying the presence of the constructed
address-modification constant among the constants of block gamma.

Op. 6 transfers control to the sub-routine.

Op. 7 transfers the address-modification constant to block gamma.

Op. 8 sets the code of the cell of block gamma with the address-modification
constant in the standard cell.

Op. 9 forms the address-modification constant.

Op. 10 transfers this instruction to block beta.

Op. 11 comparing the contents of cell z with zero verifies if the instruction
depends on a higher-order parameter (YES -- op. 31, NO -- op. 12)

Op. 12 and op. 13 clear the next cell of block gamma in which was last stored
the initial value of the given variable instruction.

Op. 14 fixes the code of this cell.

Op. 15 verifies if i_in is equal to zero (YES -- op. 16, NO -- op. 18)

Op. 16 forms the instruction for dispatch of the intial value of the variable
instruction.

Op. 17 transfers this instruction to block alpha and refers to op. 27 of MP-2.

Op. 18 comparing (U) with zero, verifies if it is necessary to transfer the
instructio for denormalization of the parameter to the programme in forming
the initail form of the given variable instruction (if (U) = 0, op. 19
functions, if (U) != 0, op. 21).

Op. 19 and Op. 20 transfer the denormalization instructijon for i_in to block
alpha and form a marker about this, directing to cell U a number different
from zero.

Operators 21-24 construct and transfer to block alpha the instruction for
multiplying the address-modification constant by the quantity i_in.

Op. 25 constructs the preparation for the formation instruction

    ┌─────┬──────┬──────┬──────┐
    │ AI  │      │ 0002 │    k │
    └─────┴──────┴──────┴──────┘

Op. 26 verifies if the variable instruction depends on higher-order parameters
(YES -- op. 29, NO -- op. 27).

Op. 27 and op. 28 construct and transfers. to block alpha the instruction for
formation of a variable instruction not depending on higher-order parameters.

    ┌─────┬──────┬──────┬──────┐
    │ AI  │    ɣ │ 0002 │    k │
    └─────┴──────┴──────┴──────┘

Op. 29 and op. 30 construct and transfer to block alpha the instruction for formation of a variable instruction dependent on higher-order parameters,

    ┌─────┬──────┬──────┬──────┐
    │ AI  │    k │ 0002 │    k │
    └─────┴──────┴──────┴──────┘

Op. 31, comparing (V) with zero, verifies if it is necessaary to transfer the
instruction for denormalization of the parameter to the programme in
restoration of the given variable instruction (if (V) = 0, op. 23 functions,
if (V) != 0 -- op. 38).

Operators 23 -37 transfer the instructions for denormalization of i_fin and
for obtaining i_Fin in the complementary code to block beta and form a marker
on this, transferring to cell V a number different from zero.

Operators 38 - 43 form and transfer to block beta the instructions for
restoration of the variable instruction.

Op. 44 verifies if i_in = 0 (YES -- exit from block II-PP-2, NO -- op. 18).

Operators 45-50 form the sub-routine which verifies if the constructed
address-modification constant is among the constants of some block. In
dependence on the choice of the initial value of the instruction for selection
from the block, this block will be either the group of positive
address-modification constants of IS or the group of negative
address-modification constants of IS or the block gamma.

Op. 45 clears the counter for selection from the block.

Op. 46 selects the next constant from the block.

Op. 47 verifies if the selected constant agrees with the constructed
address-modification constant (YES -- op. 48, NO -- op. 49).

Op. 48 fixes the address of the address-modification constant in the block.

Op. 49 ensures transfer to selection of the following constant.

Op. 50 according to the contents of the selection counter, determines if all
constants of the block have been tested.

-}

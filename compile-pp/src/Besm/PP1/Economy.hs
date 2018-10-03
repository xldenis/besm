module Besm.PP1.Economy where

import Besm.Monad
import Besm.Syntax

{-

The block III-PP-1 exactly realizes the above described (S 11) algorithm for
the economy of working cells. The block beings to function after completion of
the work of the block of arithmetical operators, when the programme of an
arithmetical operator with conditional codes of working cells has been
constructed in the block of the completed operator.

Block III-PP-1 economizes working cells and transfers the instructions of the
constructed arithmetical operator to the block of completed instructions. For
cells β₁, β₂, .., β₁₆, 16 cells of IM are assigned in sequence.
-}

pp1_3 = do

  {-
  Op. 1 clears cells β₁, .., β₁₆ and forms the initial form of hte instructiosn
  for selection from the block of the completed operator.
  -}
  {-
  Op. 2 selects instructiosn from the block of the completed operator beginning
  with the last instruction.
  -}
  {-

  Op. 3 verifies if all instructions have been taken from the block,
  transferring to op. 19 on completion of selection.
  -}
  {-
  Op. 4 extracts the third address of the selected instruction. If in the
  selected address is the "true" (not conditional) code of working cell r+ i.
  -}
  {-
  Op. 5 transfers control to op. 6.
  -}
  {-
  Op. 6 clears the corresponding cell βᵢ
  -}
  {-
  Op. 7 compares r + i with the contents of the standard cell 0005, where the
  code of the last engaged working cell is stored. If r + i exeedss the contents
  of cell 0005, op. 8 functions.
  -}
  {-
  Op. 8 transfers r + i to cell 0005.
  -}
  {-
  Op. 9 extracts the first or second address of the instructions (in the first
  passage, the second address, iun the second passage, the first address) and
  shifts it to the third address of standard cell S.
  -}
  {-
  Op. 10 examines the contents of cell S. if it containts the conditional code
  of a working cell k', op. 11 functions; in the contrary case control is
  transferred to the instruction JCC, located before op. 19.
  -}
  {-
  Op. 11 selects from the block of the completed operators the instruction with
  addresss k', extracting its third address and sending it to cell S.
  -}
  {-
  Op. 12 compares the extracted address with zero. If it is equal to zero. this
  signifies that the working cell with the result of this instruction has not
  yet been assigned its "true" code. In this case op.13 functions.
  -}
  {-
  Op.13 forms the initial form of the instruction for selection from cells β₁,
  .., β₁₆.
  -}
  {-
  Op. 14 selects the contents of the next cell βᵢ.
  -}
  {-
  Op. 15 verifies if the cell βᵢ is "marked" YEs -- op. 14 functions, NO -- op.
  16.
  -}
  {-
  Op. 16 places the code r + u of the working cell in the third addrss of thei
  instruction with address k' and cell S if Bᵤ is the first "unmarked" cell from
  among β₁, .., β₁₆, and then "marks" cell Bᵤ.
  -}
  {-
  Op. 17 places the contents of the third address of cell X in the second
  address of the instruction under examination and transfers control to op. 9
  for examination of the first address of the instruction, similarily to the
  investigation of its second addresss. Since during this the local control
  system functions, after testing of the first address of the instruction, JCC
  standing before op. 17 transfers control to op. 18.
  -}
  {-
  Op. 18 placed the contents of the third address of cellS in the first addresss
  of ths instruction under ivnestigation, sends the investigated instruction
  back to the completed operator and then transfers control to op. 2, selecting
  the next instruction.
  -}
  {-
  In addition, in the block of working cell economy is a loop for transerring
  instructions of the constructed arithmetical operator to the block of the
  completed instruction.

  Op. 19 transfers the next instruction from the block of the completed operator
  to the standard cell A + 1.
  -}
  {-
  Op. 20, after transfer of all instructions of the given arithmetical operator
  transfers to MP-1(3).
  -}
  {-
  Op. 21 modifies the addresses of the transfer instruction in op. 19.
  -}
  {-
  Op. 22 sends the instruction from cell A + 1 to the block of completed
  instructions and transfers control to op. 19.
  -}

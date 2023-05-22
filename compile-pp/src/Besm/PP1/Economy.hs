{-# LANGUAGE RecursiveDo #-}

module Besm.PP1.Economy where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

{-

The block III-PP-1 exactly realizes the above described (S 11) algorithm for
the economy of working cells. The block beings to function after completion of
the work of the block of arithmetical operators, when the programme of an
arithmetical operator with conditional codes of working cells has been
constructed in the block of the completed operator.

Block III-PP-1 economizes working cells and transfers the instructions of the
constructed arithmetical operator to the block of completed instructions. For
cells β₁, β₂, .., β₁₆, 16 cells of IM are assigned in sequence.

NOTES

The objective of this module is to assign one of 16 working cells to the output
of each arithmetic instruction. Those working cells are referred to as
r + 1, .., r + i, (i = 16). The β cells act as a bitmap of which cells are occupied
at any point within the operator.
-}

pp1_3 = do
  completedOperator <- extern "arith-buffer"
  let partialProgramme = Unknown "arith-buffer" `offAddr` 208
  let beta0 = partialProgramme `offAddr` (negate 1) -- β₀
  let beta = partialProgramme -- Unknown "β₁, .., β₁₆"
  extern "K"

  cellA1 <- extern "A + 1"
  let working = cellA1 -- Unknown "econ-current-cell"
  cellS <- local "S" Cell
  lowerBound <- extern "&completedOperator"
  upperBound <- local "upperBound" (Raw 0x3FF)

  local ",TN 116F _" (Template (TN thirdAddr zero UnNormalized))
  local ",TN buffer _" (Template (TN completedOperator zero UnNormalized))
  local ",TN β₀ βᵢ" (Template (TN beta0 (Unknown "βᵢ") UnNormalized))
  local "tn" (Template (TN zero working UnNormalized)) -- ,TN _  working
  local "ai _ cellS _" (Template (AI zero cellS zero))
  local "and-template" (Template (LogMult zero thirdAddr cellS)) -- '^ _ thirdAddr cellS'
  local "final-store" (Template (AddE cellS working zero UnNormalized)) -- ,+Exp cellS currInst _
  local "copy of working" (Raw 0)
  local "end-of-arith-op" (Raw 0)
  let endOfOperator = Unknown "end-of-arith-op"

  local "0005" (Raw 0)
  local "k'" (Raw 0)
  local "recall-arg" (Raw 0)
  local "working-code" (Raw 0)
  local "βᵢ" (Raw 0)

  {-
  Op. 1 clears cells β₁, .., β₁₆ and forms the initial form of the instructiosn
  for selection from the block of the completed operator.
  -}
  selectNextInstr <- mdo
    operator 1 $ do
      let tnTemplate = Unknown "tn" -- ,TN _  working
      let counterK = Unknown "K"
      readMD 4 (Absolute 1) (Absolute 15) beta

      shift counterK (left 22) select
      ai tnTemplate select endOfOperator
      ai endOfOperator oneFirstAddr select

      chain (op 2)
    {-
    Op. 2 selects instructions from the block of the completed operator beginning
    with the last instruction.
    -}

    select <- operator 2 $ mdo
      let minusOne = firstAddr
      ai select minusOne select
      select <- empty
      chain (op 3)

      return select

    {-
    Op. 3 verifies if all instructions have been taken from the block,
    transferring to op. 19 on completion of selection.
    -}
    operator 3 $ do
      let startOfBlock = Unknown ",TN buffer _" -- have we reached the start of the buffer
      compMod select startOfBlock (op 19) (op 4)

    return select
  {-
  Op. 4 extracts the third address of the selected instruction.
  -}
  let workingCode = Unknown "working-code"
  let workingCopy = Unknown "copy of working"
  operator 4 $ do
    tN' working workingCopy
    bitAnd workingCopy thirdAddr workingCode
    chain (op 5)

  {-
  If in the selected address is the "true" (not conditional) code of working cell r + i.
  Op. 5 transfers control to op. 6.

  NOTES

  Objective is to check if its a 'working cell' or a constant value
  so we need it to be in the range 0x100 - 0x3f0

  This means betas _need_ to be laid out after the completed operator block.

  Conditional codes are values in the range of the "buffer" used by PP-1.
  -}
  operator 5 $ mdo
    compMod workingCode lowerBound (op 9) c -- this actually tests if we have a quantity
    c <- block $ compMod upperBound workingCode (op 9) (op 6) -- tests if we are in the range of betas
    return ()
  {-
  Op. 6 clears the corresponding cell βᵢ
  -}

  operator 6 $ mdo
    ce' workingCode (Absolute 0xC) clear
    clear <- empty
    return ()
  {-
  Op. 7 compares r + i with the contents of the standard cell 0005, where the
  code of the last engaged working cell is stored. If r + i exceeds the contents
  of cell 0005, op. 8 functions.
  -}
  let u0005 = Unknown "0005"
  operator 7 $ do
    comp workingCode u0005 (op 9) (op 8)
  {-
  Op. 8 transfers r + i to cell 0005.
  -}
  operator 8 $ do
    tN' workingCode u0005
  {-
  Op. 9 extracts the first or second address of the instructions (in the first
  passage, the second address, in the second passage, the first address) and
  shifts it to the third address of standard cell S.
  -}
  operator 9 $ do
    shift' workingCopy (right 11) workingCopy
    bitAnd workingCopy thirdAddr cellS

    chain (op 10)

  {-
  Op. 10 examines the contents of cell S. if it contains the conditional code
  of a working cell k', op. 11 functions; in the contrary case control is
  transferred to the instruction JCC, located before op. 17.
  -}

  operator 10 $ mdo
    comp cellS lowerBound (op 17) upperComp
    upperComp <- comp upperBound cellS (op 17) (op 11)
    return ()
  {-
  Op. 11 selects from the block of the completed operators the instruction with
  addresss k', extracting its third address and sending it to cell S.
  -}

  let k' = Unknown "k'" -- cell holds the address k'
  operator 11 $ mdo
    let andTemplate = Unknown "and-template" -- '^ _ thirdAddr cellS'
    shift cellS (left 22) k'
    ai andTemplate k' and
    ai k' cellS k'
    and <- empty

    chain (op 12)
  {-
  Op. 12 compares the extracted address with zero. If it is equal to zero, this
  signifies that the working cell with the result of this instruction has not
  yet been assigned its "true" code. In this case op.13 functions.
  -}

  operator 12 $ do
    compWord cellS zero (op 17) (op 13)

  {-
  Op.13 forms the initial form of the instruction for selection from cells β₁,
  .., β₁₆.
  -}
  mdo
    operator 13 $ do
      let baseInstruction = Unknown ",TN β₀ βᵢ"
      tN' baseInstruction selector
      chain (op 14)
    {-
    Op. 14 selects the contents of the next cell βᵢ.
    -}
    selector <- operator 14 $ do
      ai selector oneFirstAddr selector -- modifies the instruction at selector: ,TN βᵣ βᵢ => ,TN βᵣ₊₁ βᵢ
      selector <- empty

      chain (op 15)
      return selector
    {-
    Op. 15 verifies if the cell βᵢ is "marked" Yes -- op. 14 functions, NO -- op.
    16.
    -}
    operator 15 $ do
      let selected = Unknown "βᵢ"

      compMod zero selected (op 14) (op 16)
    {-
    Op. 16 places the code r + u of the working cell in the third addrss of the
    instruction with address k' and cell S if Bᵤ is the first "unmarked" cell from
    among β₁, .., β₁₆, and then "marks" cell Bᵤ.
    -}
    operator 16 $ mdo
      let storeTemplate = Unknown "ai _ cellS _" -- this is used to store βᵢ in the third (blank addr) of k'
      let markTemplate = Unknown ",TN 116F _" -- used to mark βᵢ (116F is a builtin cell full of 1).
      shift selector (right 22) cellS
      ai storeTemplate k' storeK'
      storeK' <- empty
      ai markTemplate cellS markBeta
      markBeta <- empty

      chain (op 17)
  {-
  Op. 17 places the contents of the third address of cell S in the second
  address of the instruction under examination and transfers control to op. 9
  for examination of the first address of the instruction, similarily to the
  investigation of its second addresss. Since during this the local control
  system functions, after testing of the first address of the instruction, JCC
  standing before op. 17 transfers control to op. 18.
  -}

  let recallArg = Unknown "recall-arg"
  operator 17 $ do
    jcc

    shift cellS (left 11) recallArg
    clcc (op 9)

    chain (op 18)
  {-
  Op. 18 placed the contents of the third address of cell S in the first addresss
  of the instruction under investigation, sends the investigated instruction
  back to the completed operator and then transfers control to op. 2, selecting
  the next instruction.
  -}

  operator 18 $ mdo
    shift' cellS (left 22) cellS
    ai cellS recallArg cellS
    ai cellS workingCode cellS
    shift selectNextInstr (right 22) finalStore

    let template = Unknown "final-store" -- ,+Exp cellS working _
    ai template finalStore finalStore
    finalStore <- empty

    chain (op 2)

  {-
  In addition, in the block of working cell economy is a loop for transerring
  instructions of the constructed arithmetical operator to the block of the
  completed instruction.

  Op. 19 transfers the next instruction from the block of the completed operator
  to the standard cell A + 1.

  NOTE
  ====

  -}

  mdo
    transfer <- operator 19 $ do
      ai selectNextInstr oneFirstAddr transfer
      transfer <- empty
      chain (op 20)

      return transfer
    {-
    Op. 20, after transfer of all instructions of the given arithmetical operator
    transfers to MP-1(3).
    -}
    operator 20 $ do
      comp endOfOperator transfer (Procedure "MP-1" (op 3)) (op 21)
    {-
    Op. 21 modifies the addresses of the transfer instruction in op. 19.
    -}
    operator 21 $ do
      ai transfer oneFirstAddr transfer
    {-
    Op. 22 sends the instruction from cell A + 1 to the block of completed
    instructions and transfers control to op. 19.
    -}
    operator 22 $ do
      callRtc (Procedure "MP-1" (op 21)) (Procedure "MP-1" (op 23))

      cccc (op 19 `offAddr` 1)

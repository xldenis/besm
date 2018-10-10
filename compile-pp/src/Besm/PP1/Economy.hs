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

constantMap =
  [ ("β₁, .., β₁₆", Size 16)
  , ("tn", Template (TN (Absolute 0) (Unknown "working") UnNormalized)) -- ,TN _  working
  , ("end-of-arith-op", Raw 0)
  , (",TN buffer _", Template (TN (Unknown "buffer") (Absolute 0) UnNormalized))
  , ("econ-current-cell", Raw 0)
  , ("copy of working", Raw 0)
  , ("third-addr-mask", Raw 0)
  , ("working-code", Raw 0)
  , ("&completedOperator", Addr completedOperator)
  , ("&completedOperator + 144", Addr $ completedOperator `offAddr` 144)
  , ("0005", Raw 0)
  , ("S", Cell)
  , ("k'", Raw 0)
  , ("and-template", Raw 0)
  , ("1 << 22", Raw 0)
  , ("ai _ cellS _", Raw 0)
  , (",TN 116F _", Raw 0)
  , ("recall-arg", Raw 0)
  , ("final-store", Raw 0)
  , (",TN β₀ βᵢ", Template (TN beta0 (Unknown "βᵢ")  UnNormalized))
  , ("βᵢ", Raw 0)
  ]
  where
  completedOperator = Unknown "buffer" `offAddr` 96
  beta0 = Unknown "β₁, .., β₁₆" `offAddr` (negate 1)

pp1_3 = do
  let one           = Unknown "1"
  let beta          = Unknown "β₁, .., β₁₆"
  let working       = Unknown "econ-current-cell"
  let endOfOperator = Unknown "end-of-arith-op"
  let thirdAddr     = Unknown "third-addr-mask"
  let zero          = Unknown "0"
  let minusOne      = Unknown "-1"
  let oneInFirst    = Unknown "1 << 22"
  let lowerBound    = Unknown "&completedOperator"
  let upperBound    = Unknown "&completedOperator + 144"

  {-
  Op. 1 clears cells β₁, .., β₁₆ and forms the initial form of the instructiosn
  for selection from the block of the completed operator.
  -}
  selectNextInstr <- mdo
    operator 1 $ do
      let tnTemplate = Unknown "tn" -- ,TN _  working
      let counterK = Unknown "K"
      readMD 7 (Absolute 1) (Absolute 15) beta

      ai tnTemplate counterK endOfOperator
      ai endOfOperator one select

      chain (op 2)
    {-
    Op. 2 selects instructions from the block of the completed operator beginning
    with the last instruction.
    -}

    select <- operator 2 $ mdo
      ai select minusOne select
      select <- empty
      chain (op 3)

      return select

    return ()
    {-
    Op. 3 verifies if all instructions have been taken from the block,
    transferring to op. 19 on completion of selection.
    -}
    operator 3 $ do
      let startOfBlock = Unknown ",TN buffer _" -- have we reached the start of the buffer
      compMod select startOfBlock (op 4) (op 19)

    return select
  {-
  Op. 4 extracts the third address of the selected instruction.
  -}
  let workingCode = Unknown "working-code"
  let workingCopy = Unknown "copy of working"
  operator 4 $ do
    tN' working workingCopy
    bitAnd working thirdAddr workingCode
    chain (op 5)

  {-
  If in the selected address is the "true" (not conditional) code of working cell r + i.
  Op. 5 transfers control to op. 6.

  Conditional codes are values in the range of the "buffer" used by PP-1.
  -}
  operator 5 $ mdo
    compMod workingCode lowerBound c (op 6)
    c <- block $ compMod upperBound workingCode (op 9) (op 6)

    chain (op 6)
  {-
  Op. 6 clears the corresponding cell βᵢ
  -}

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
  let cellS = Unknown "S"
  operator 9 $ do
    shift working (right 11) working
    bitAnd working thirdAddr cellS

    chain (op 10)

  {-
  Op. 10 examines the contents of cell S. if it contains the conditional code
  of a working cell k', op. 11 functions; in the contrary case control is
  transferred to the instruction JCC, located before op. 19.
  -}

  operator 10 $ mdo
    comp cellS lowerBound (op 19) upperComp
    upperComp <- comp upperBound cellS (op 11) (op 19)
    chain (op 11)
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
      let oneInFirst = Unknown "1 << 22"
      ai selector oneInFirst selector -- modifies the instruction at selector: ,TN βᵣ βᵢ => ,TN βᵣ₊₁ βᵢ
      selector <- empty

      chain (op 14)
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
      let markTemplate  = Unknown ",TN 116F _" -- used to mark βᵢ (116F is a builtin cell full of 1).
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
  Op. 18 placed the contents of the third address of cellS in the first addresss
  of ths instruction under ivnestigation, sends the investigated instruction
  back to the completed operator and then transfers control to op. 2, selecting
  the next instruction.
  -}

  operator 18 $ mdo
    shift cellS (left 22) cellS
    ai recallArg cellS cellS
    ai workingCode cellS cellS
    shift selectNextInstr (right 22) finalStore

    let template = Unknown "final-store" -- ,+Exp cellS currInst _
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

  This relies on the idea that
  -}

  mdo
    let cellA1 = Unknown "A + 1"
    transfer <- operator 19 $ do
      ai selectNextInstr oneInFirst transfer
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
      ai transfer oneInFirst transfer
    {-
    Op. 22 sends the instruction from cell A + 1 to the block of completed
    instructions and transfers control to op. 19.
    -}
    operator 22 $ do
      callRtc (Procedure "MP-1" (op 21)) (Procedure "MP-1" (op 23))

      chain (op 19)

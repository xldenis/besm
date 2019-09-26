{-# LANGUAGE RecursiveDo #-}
module Besm.PP2.Control where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

{-

The block II-PP-2 constructs all control instructions relating to variable
instructions. The initial data for this constitute: the address of the
variable instruction in block K locate in counter K; the contents of cells p
and q, from which will be constructed the address-modification constant; the
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
-}

pp2_2 = mdo
  let gammaTransfer = Procedure "MP-2" (op 39)
      betaTransfer  = Procedure "MP-2" (op 36)
      header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7
      gammaInitial  = header `offAddr` 6
      alphaTransfer = Procedure "MP-2" (op 33)

  pht <- extern "programme header table"
  gammaBuilder <- extern "builder"
  gammaCounter <- extern "ɣ-counter"

  wm <- extern "working-cells"
  extern "selected"


  normInstr <- extern "norm-instruction"
  let cellP = wm `offAddr` 2
  let cellQ = wm `offAddr` 3

  let counter = Absolute 0x003
  let loopBound = Absolute 0x001

  local "gamma param" (Template (TN zero wm UnNormalized))
  local "posi param" (Template (TN oneFirstAddr wm UnNormalized))
  local "neg param" (Template (TN firstAddr wm UnNormalized))

  local "xa-template" (Template (Xa (Absolute 0x001) zero zero UnNormalized))
  local "xb-template" (Template (Xb (Absolute 0x002) UnNormalized))


  seven' <- extern "7'"

  counterK <- extern "k"

  cellU <- extern "U"
  cellV <- extern "V"

  let cellZ = wm `offAddr` 4
  {- Op. 1 forms the address-modification constant.
  -}
  operator 1 $ do
    -- these two instructions are taken from Appendix 3.2 Negative Address-Modification Constants (p 314)
    add' (Absolute 0x1169) cellQ cellQ
    ai cellQ one cellQ

    ai cellP cellQ gammaBuilder

    tN' (var "posi param") (op 46)
    tN' seven' loopBound
    chain (op 2)
  {-
  Op. 2 transfers control to the sub-routine verifying if there is such a
  constant among the positive address-modification constants of IS.
  -}
  operator 2 $ do
    callRtc (op 45) subRet

    chain (op 3)
  {-
  If there is such a constant then the sub-routine places it in the standard
  cell the address of the address-modification constant, transferring control
  to op. 9. In the contrary case the sub-routine realizes return transfer of
  control in the ordinary manner.

  Op. 3 prepares the sub-routine for verifying the presence of the constructed
  address-modification constant among the negative constants of IS.
  -}

  operator 3 $ do
    tN' (var "neg param") (op 46)
    tN' seven' loopBound

  {-
  Op. 4 transfers control to the sub-routine.
  -}
  operator 4 $ do
    callRtc (op 45) subRet
  {-
  Op. 5 prepares the sub-routine for verifying the presence of the constructed
  address-modification constant among the constants of block gamma.
  -}

  let cellB = Absolute 0x002
  operator 5 $ do
    shift gammaInitial (left 22) cellB
    ai (var "gamma param") cellB (op 46)
    sub' gammaCounter gammaInitial loopBound

    chain (op 6)
  {-
  Op. 6 transfers control to the sub-routine.
  -}
  operator 6 $ do
    callRtc (op 45) subRet
  {-
  Op. 7 transfers the address-modification constant to block gamma.
  -}
  operator 7 $ do
    clcc gammaTransfer
    chain (op 8)
  {-
  Op. 8 sets the code of the cell of block gamma with the address-modification
  constant in the standard cell.
  -}
  let shiftedS = wm `offAddr` 1
  operator 8 $ do
    -- what is the significance of gammaCounter here? Is it the location
    -- that the loop parameter will be stored at?
    shift gammaCounter (left 11) shiftedS

    chain (op 9)
  {-
  Op. 9 forms the address-modification constant.
  -}
  let shiftedK = wm `offAddr` 2
  operator 9 $ do
    ai counterK shiftedS cellB
    shift counterK (left 22) shiftedK
    ai cellB shiftedK cellB
    tSign' cellB  (Absolute 0) cellB -- change the sign bit to make it an unnormalized instr

    ce' cellB (Absolute 0x12) gammaBuilder

  {-
  Op. 10 transfers this instruction to block beta.
  -}

  operator 10 $ do
    clcc betaTransfer
  {-
  Op. 11 comparing the contents of cell z with zero verifies if the instruction
  depends on a higher-order parameter (YES -- op. 31, NO -- op. 12)
  -}
  operator 11 $ do
    compWord zero cellZ (op 31) (op 12)
  {-
  Op. 12 and op. 13 clear the next cell of block gamma in which was last stored
  the initial value of the given variable instruction.

  unclear why the code uses the selected-instruction counter?
  -}
  operator 12 $ do
    tN' (var "selected") gammaBuilder

  operator 13 $ do
    clcc gammaTransfer
  {-
  Op. 14 fixes the code of this cell.
  -}
  operator 14 $ do
    shift gammaCounter (left 22) cellB

  {-
  Op. 15 verifies if i_in is equal to zero (YES -- op. 16, NO -- op. 18)
  -}
  operator 15 $ do
    compMod zero wm (op 18) (op 16)
  {-
  Op. 16 forms the instruction for dispatch of the intial value of the variable
  instruction.
  -}
  operator 16 $ do
    ai cellB gammaCounter counter
    ce' counter (Absolute 0x12) gammaBuilder

    chain (op 17)
  {-
  Op. 17 transfers this instruction to block alpha and refers to op. 27 of MP-2.
  -}
  operator 17 $ do
    clcc alphaTransfer

    cccc (Procedure "MP-2" (op 27))

  {-
  Op. 18 comparing (U) with zero, verifies if it is necessary to transfer the
  instruction for denormalization of the parameter to the programme in forming
  the initial form of the given variable instruction (if (U) = 0, op. 19
  functions, if (U) != 0, op. 21).
  -}
  operator 18 $ do
    compWord zero cellU (op 21) (op 19)
  {-
  Op. 19 and Op. 20 transfer the denormalization instructijon for i_in to block
  alpha and form a marker about this, directing to cell U a number different
  from zero.
  -}
  operator 19 $ do
    tN' one cellU
    tN' normInstr gammaBuilder

  operator 20 $ do
    clcc alphaTransfer

  {-
  Operators 21-24 construct and transfer to block alpha the instruction for
  multiplying the address-modification constant by the quantity i_in.

  Note
  ====

  The source documents disagree with the book on the ordering of the parameters

  does this matter? is this a reflection in changes between the source and book?


  -}
  operator 21 $ do
    ai (var "xa-template") shiftedS gammaBuilder

  operator 22 $ do
    clcc alphaTransfer

  operator 23 $ do
    tN' (var "xb-template") gammaBuilder

  operator 24 $ do
    clcc alphaTransfer
  {-
  Op. 25 constructs the preparation for the formation instruction

      ┌─────┬──────┬──────┬──────┐
      │ AI  │      │ 0002 │    k │
      └─────┴──────┴──────┴──────┘
  -}
  local "ai-template" (Template (AI zero (Absolute 0x002) zero))

  let tempBuilder = loopBound
  operator 25 $ do
    ai (var "ai-template") counterK tempBuilder
  {-
  Op. 26 verifies if the variable instruction depends on higher-order parameters
  (YES -- op. 29, NO -- op. 27).
  -}
  operator 26 $ do
    compWord zero cellZ (op 29) (op 27)
  {-
  Op. 27 and op. 28 construct and transfers. to block alpha the instruction for
  formation of a variable instruction not depending on higher-order parameters.

      ┌─────┬──────┬──────┬──────┐
      │ AI  │    ɣ │ 0002 │    k │
      └─────┴──────┴──────┴──────┘

  NOTE
  ====

    cell B contains a shifted version of gamma because of operator 14
  -}
  operator 27 $ do
    ai tempBuilder cellB gammaBuilder

  operator 28 $ do
    clcc alphaTransfer
    cccc (Procedure "MP-2" (op 27))

  {-
  Op. 29 and op. 30 construct and transfer to block alpha the instruction for formation of a variable instruction dependent on higher-order parameters,

      ┌─────┬──────┬──────┬──────┐
      │ AI  │    k │ 0002 │    k │
      └─────┴──────┴──────┴──────┘
  -}
  operator 29 $ do
    ai tempBuilder shiftedK gammaBuilder

  operator 30 $ do
    clcc alphaTransfer
    cccc (Procedure "MP-2" (op 27))
  {-
  Op. 31, comparing (V) with zero, verifies if it is necessaary to transfer the
  instruction for denormalization of the parameter to the programme in
  restoration of the given variable instruction (if (V) = 0, op. 32 functions,
  if (V) != 0 -- op. 38).
  -}
  operator 31 $ do
    compMod zero cellV (op 38) (op 32)
  {-
  Operators 32 - 37 transfer the instructions for denormalization of i_fin and
  for obtaining i_fin in the complementary code to block beta and form a marker
  on this, transferring to cell V a number different from zero.
  -}
  operator 32 $ do
    tN one cellB
    tN' normInstr gammaBuilder

  operator 33 $ do
    clcc betaTransfer


  local "add-template" (Template (Add (Absolute 0x1169) (Absolute 1) (Absolute 1) UnNormalized))
  operator 34 $ do
    tN' (var "add-template") gammaBuilder

  operator 35 $ do
    clcc betaTransfer

  local "ai-template" (Template (AI (Absolute 0x001) one (Absolute 0x001)))
  operator 36 $ do
    tN' (var "ai-template") gammaBuilder

  operator 37 $ do
    clcc betaTransfer

  {-
  Operators 38 - 43 form and transfer to block beta the instructions for
  restoration of the variable instruction.
  -}
  operator 38 $ do
    ai (var "xa-template") shiftedS gammaBuilder

  operator 39 $ do
    clcc betaTransfer

  operator 40 $ do
    tN' (var "xb-template") gammaBuilder

  operator 41 $ do
    clcc betaTransfer

  operator 42 $ do
    ai counterK shiftedK tempBuilder
    ai (var "ai-template") tempBuilder gammaBuilder

  operator 43 $ do
    clcc betaTransfer

  {-
  Op. 44 verifies if i_in = 0 (YES -- exit from block II-PP-2, NO -- op. 18).
  -}

  iinShifted <- extern "iinShifted"
  operator 44 $ mdo
    compMod zero iinShifted (op 18) b

    b <- block $ do
      cccc (Procedure "MP-2" (op 27))

    return ()

  {-
  Operators 45-50 form the sub-routine which verifies if the constructed
  address-modification constant is among the constants of some block. In
  dependence on the choice of the initial value of the instruction for selection
  from the block, this block will be either the group of positive
  address-modification constants of IS or the group of negative
  address-modification constants of IS or the block gamma.

  Op. 45 clears the counter for selection from the block.
  -}
  operator 45 $ do
    tN' zero counter
  {-
  Op. 46 selects the next constant from the block.
  -}
  operator 46 $ do
    empty
  {-
  Op. 47 verifies if the selected constant agrees with the constructed
  address-modification constant (YES -- op. 48, NO -- op. 49).
  -}
  let fixedConstant = wm `offAddr` 1
  operator 47 $ do
    compWord gammaBuilder fixedConstant (op 49) (op 48)
  {-
  Op. 48 fixes the address of the address-modification constant in the block.
  -}
  operator 48 $ do
    bitAnd (op 46) firstAddr fixedConstant
    shift' fixedConstant (right 22) fixedConstant
  {-
  Op. 49 ensures transfer to selection of the following constant.
  -}
  operator 49 $ do
    cccc (op 9)
  {-
  Op. 50 according to the contents of the selection counter, determines if all
  constants of the block have been tested.

  -}
  subRet <- operator 50 $ mdo
    ai (op 46) oneFirstAddr (op 46)
    ai counter one counter
    compMod counter loopBound (op 46) b

    b <- block $ do
      retRTC

    return b

  return ()

{-# LANGUAGE RecursiveDo #-}
module Besm.PP2.Loop where

import           Besm.Assembler.Monad
import           Besm.Assembler.Syntax

{-
  The block I-PP-2 for loop formation processes information on the parameter,
  the code of which "i" is found in counter i in the third address.
-}

cellU = Unknown "U"
cellV = Unknown "V"

alphaBuilder = Unknown "α-builder"
betaBuilder  = Unknown "β-builder"
gammaBuilder = Unknown "ɣ-builder"

alphaCounter = Unknown "α-counter"
betaCounter  = Unknown "β-counter"
gammaCounter = Unknown "ɣ-counter"

alphaTransfer = Unknown "α-transfer"
betaTransfer  = Unknown "β-transfer"
gammaTransfer = Unknown "ɣ-transfer"

alphaInitial = Unknown "α-initial"
betaInitial  = Unknown "β-initial"
gammaInitial = Unknown "ɣ-initial"

transferToAlpha = undefined

counterK = Unknown "k"
counterKInitial = Unknown "k-initial"

counterI = Unknown "i"

pp2_1 = do
  let cellT = Unknown "t"
  let shiftedI = Unknown "i-shifted"

  let parameterSelect = Unknown ",TN _ parameter-info"
  let parameterInfo = Unknown "parameter-info"

  let iin = Unknown "i_in"

  let kInitial = Unknown "k-trans-initial"

  let normTemplate = Unknown "I _ 0001"
  let normalizeInstr = Unknown "norm-instruction"

  let incrTemplate = Unknown "+ _ 1081 _"
  let incrInstr = Unknown "incr-instruction"

  {-
  Op. 1 clears standard cells U and V of markers on the transmission to the
  programme of instructions for normalization of the parameter, carried out at
  the start and the end of the loop for formation and restoration of variable
  instructions respectively, sets the counters and transfer-instructions to
  block α and β in the inital position, selects inforamtion on the
  parameter in the standard cell, forms the instructions for normalization of
  the parameter in the standard cell:

    ┌─────┬──────┬──────┬──────┐
    │ I   │ "i"  │      │ 0001 │
    └─────┴──────┴──────┴──────┘

  and parameter change

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "i"  │ 1081 │    i │
    └─────┴──────┴──────┴──────┘

  Counter K and the instruction for selection from block K are set in the
  initial position.
  -}
  mdo
    operator 1 $ mdo
      -- Clear cells U and V
      tN' zero cellU
      tN' zero cellV

      -- Reset α and β counters

      tN' zero alphaCounter
      tN' zero betaCounter

      -- Reset α and β transfer-instructions

      tN' betaInitial betaTransfer
      tN' alphaInitial alphaTransfer

      -- Having "i" in the first address is very useful
      shift counterI (left 22) shiftedI

      -- Select parameter info

      ai parameterSelect shiftedI param
      param <- empty

      -- Form instructions

      ai normTemplate shiftedI normalizeInstr -- I _ 0001
      ai incrTemplate shiftedI incrInstr -- + _ 1081 _
      ai incrInstr    counterI incrInstr

      tN' counterKInitial counterK
      tN' kInitial kTrans

      chain (op 2)

    {-
    Op. 2 selects the instruction from block K.
    -}
    let selected = Unknown "selected"
    kTrans <- operator 2 $ do
      empty -- ,TN _ selected

    return ()

    {-
    Op. 3 repeats selection of instructions until the open parentheses of the
    loop over i is selected, having form

    ┌─────┬──────┬──────┬──────┐
    │     │ "i"  │      │      │
    └─────┴──────┴──────┴──────┘
    -}
    operator 3 $ do
      compWord selected shiftedI (op 2) (op 4)

    {-
    Op. 4 transfers the address of the open-parentheses of the loop to the
    standard cell and selects the code i_in from the information on the
    parameter.
    -}

    let parenCode = Unknown "paren-code"

    operator 4 $ mdo
      shift kTrans (left 22) parenCode
      shift parameterInfo (left 22) iin

  {-
  Op. 5 verifies if i_in is equal to zero (YES -- op. 7, NO -- op. 6).
  -}

  operator 5 $ do
    compWord iin zero (op 6) (op 7)

  let vf = Unknown "V_f"

  {-
  Op. 6 comparing "i_in" with the quantity V_f, verifies if i_in is variable
  (YES -- op. , NO -- op. 7).
  -}
  operator 6 $ do
    comp iin vf (op 8) (op 7)
  {-

  Op. 7 forms the instruction for dispatching the initial value of the
  parameter and transfers it to block alpha.
  -}
  let mkTn = Absolute 0x14

  operator 7 $ do
    shift iin (left 22) alphaBuilder
    ai alphaBuilder unity alphaBuilder
    ce' alphaBuilder mkTn alphaBuilder

    transferToAlpha

    chain (op 9)
  {-
  Op. 8 sends to the standard cell t the code of the parameter "i" and
  transfers control to the sub-routine for forming the instructions for
  calculating the variable i_sigma (op. 15 - op. 32), which form the
  instructiosn for calculating i_in and transsfers them to the block.
  -}

  operator 8 $ do
    tN' counterI cellT

    callRtc (op 15) (op 32)

    chain (op 9)


  {-
  Op. 9 determines the case of a characteristic loop, for which op. 14
  functions.
  -}

  operator 9 $ do
    let compOp = Unknown "comp-operator"

    tExp' parameterInfo compOp

    comp zero compOp (op 14) (op 10)

  {-
  Op. 10 forms the preparation for the comparison instruction of an ordinary
  loop and extracts the code "i_fin" from the information on the parameter.
  -}
  let mkCmp = Absolute 0x14
  let builtComp = Unknown "built-comp"

  operator 10 $ do
    ce' shiftedI mkCmp builtComp

  {-
  Op. 11 comparing code i_fin with quantity V_f, verifies if i_fin is
  variable.
  -}

  let ifin = Unknown "i_fin"

  operator 11 $ do
    comp ifin vf (op 13) (op 12)

  {-
  Op. 12 completes the comparison instruction for the case of i_fin independent
  of higher-order parameters.
  -}

  operator 12 $ do
    ai builtComp ifin builtComp

    chain (Procedure "MP-2" (op 4))

  {-
  Op. 13 in the case of i_fin dependent on higher-order parametrs, clears the
  next cell of block gamma, reserving it for storage of the calculated value
  of i_fin. The code of this cell is transferred to the standard cell t. After
  this the comparison instruction of the loop is formed and then control is
  transferred to the sub-routine forming the instruction for calculating i_fin
  and transfers them to block alpha.
  -}
  operator 13 $ do
    tN' zero gammaBuilder
    clcc gammaTransfer

    tN' gammaCounter cellT


  {-
  Op. 14 forms the comparison for a characteristic loop.
  -}

  {-
  After the functioning of block I-PP-2 control is transferred to op. 4 of
  MP-2.

  Operators 15-32 related to the sub-routien which, according to the code
  i_sigma, selects information on its dependence fon higher-order parameters
  and forms the instructions for calculating i_sigma, directing the results
  according to the "address" shown in the third address of cell t. For each of
  the twelve cases of dependence of i_sigma shown in Table 14 the
  corresponding insturctions for calculating i_sigma are constructed.

  Op. 15 according to information  on i_sigma, forms the following constants:


    ┌─────┬──────┬──────┬──────┐
    │     │"i_0" │      │      │
    └─────┴──────┴──────┴──────┘

    ┌─────┬──────┬──────┬──────┐
    │     │ "A"  │      │      │
    └─────┴──────┴──────┴──────┘

    ┌─────┬──────┬──────┬──────┐
    │     │      │ "j"  │      │
    └─────┴──────┴──────┴──────┘

    ┌─────┬──────┬──────┬──────┐
    │     │ "j"  │      │      │
    └─────┴──────┴──────┴──────┘

    ┌─────┬──────┬──────┬──────┐
    │     │      │ "k"  │      │
    └─────┴──────┴──────┴──────┘
  -}

  {-
  Op. 16 refers to op. 17 if "k"= 0 and to op. 23 if "k" != 0

  Op. 17 refers to op. 18 if "A"=0 and to op. 21 if "A" != 0

  Op. 18 refers to op. 19 if "i0" = 0, and to op. 20 if "i0" != 0.

  Op. 19 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ T   │ "j"  │      │      │
    └─────┴──────┴──────┴──────┘

  and sets it in the standard cell S.

  Op. 20 forms the instruciton:

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "i0" │ "j"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in the standard cell S.

  Op. 21 forms the instruction:

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "A"  │ "j"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in cell S.

  Op. 22 refers to op. 32 if "i0" = 0 and to op. 31 if "i0" != 0.

  Op. 23 according to information on i_sigma, forms the constant

    ┌─────┬──────┬──────┬──────┐
    │     │ "B"  │      │      │
    └─────┴──────┴──────┴──────┘

  Op. 24 refers to op. 25 if "A" = 0 and to op. 28 if "A" != 0

  Op. 25 refers to op. 26 if "B" = 0 and to op. 27 if "B" != 0

  Op. 26 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "j"  │ "k"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in cell S.

  Op. 27 forms the instruction (rho_2 = 0001, rho_1 = 0002)

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "B"  │ "k"  │ 0001 │
    └─────┴──────┴──────┴──────┘

  transfers it to block alpha, constructs the instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ 0001 │ "j"  │      │
    └─────┴──────┴──────┴──────┘

  and transfers it to cell s.

  Op. 28 refers to op. 29 if "B" = 0, and to op. 30 if "B" != 0.

  Op. 29 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "A"  │ "j"  │ 0001 │
    └─────┴──────┴──────┴──────┘

  transfers it to block alpha, constructs the instruction and sets it in cell
  S.

  Op. 30 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "B"  │ "k"  │ 0002 │
    └─────┴──────┴──────┴──────┘

  transfers it to block alpha, forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "A"  │ "j"  │ 0001 │
    └─────┴──────┴──────┴──────┘

  and also transfers it to block alpha and sets in cell S the instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ 0001 │ 0002 │      │
    └─────┴──────┴──────┴──────┘

  Op. 31 adds 0001 to the third address of the instruction standing in cell S,
  transfers the constructed instruction to block alpha and then forms the
  instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "i0" │ 0001 │      │
    └─────┴──────┴──────┴──────┘

  and sets in cell S.

  Op. 32 adds (t) to the instruction standing in cell S and transfers the
  constructed instruction to block alpha.

  -}

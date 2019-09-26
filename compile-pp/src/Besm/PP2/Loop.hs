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
cellS = Unknown "S"

gammaBuilder = Unknown "builder"
gammaCounter = Unknown "ɣ-counter"

kTransfer     = Procedure "MP-2" (op 32)
alphaTransfer = Procedure "MP-2" (op 33) -- address where the transfer to alpha instruction is
betaTransfer  = Procedure "MP-2" (op 36)
gammaTransfer = Procedure "MP-2" (op 39)

header       = Unknown "programme header table" `offAddr` 6

betaInitial  = header `offAddr` 8
alphaInitial = header `offAddr` 7
gammaInitial = header `offAddr` 6

gammaTransInitial = Unknown "ɣ-trans-initial"

pp2_1 = do
  extern "programme header table"

  counterK <- extern "k"
  counterI <- extern "i"

  alphaCounter <- extern "α-counter"
  alphaBuilder <- extern "builder"
  alphaTransInitial <- extern "α-trans-initial"

  betaCounter <- extern "β-counter"
  -- extern "β-builder"
  betaTransInitial <- extern "β-trans-initial"

  extern "ɣ-counter"
  extern "U"
  extern "V"
  wm <- extern "working-cells"

  kInitial <- extern "k-trans-initial"
  extern "selected"


  parameterSelect <- local ",TN _ parameter-info" $ Template (TN zero (var "parameter-info") UnNormalized)
  normTemplate <- local "I _ 0001" (Template (I zero zero zero))
  incrTemplate <- local "+ _ 1081 _" (Template (Add zero zero zero UnNormalized))

  local "+ _ 0001 _"  $ Template (Add zero (addr 1) zero UnNormalized)
  local "+ 0001 0002" $ Template (Add (addr 1) (addr 2) zero UnNormalized)
  local "x _ _ 0002"  $ Template (Mult zero zero (addr 2) UnNormalized)
  local "+ 0001 _ _"  $ Template (Add (addr 1) zero zero UnNormalized)
  local "x _ _ 0001"  $ Template (Mult zero zero (addr 1) UnNormalized)
  local ",TN _ firstHalf" $ Template (TN zero (wm `offAddr` 0) UnNormalized)

  cellT <- local "t" Cell
  shiftedI <- local "i-shifted" Cell
  parameterInfo <- local "parameter-info" Cell
  normalizeInstr <- global "norm-instruction" Cell
  incrInstr <- local "incr-instruction" Cell

  local "S" Cell

  -- Intermediate values used to build up instructions

  local "paren-code" Cell
  local "comp-operator" Cell
  local "built-comp" Cell

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
  initial  position.
  -}
  mdo
    operator 1 $ mdo
      -- Clear cells U and V
      tN' zero cellU
      tN' zero cellV

      -- Reset α and β counters

      tN' alphaInitial alphaCounter
      tN' betaInitial betaCounter

      -- Reset α and β transfer-instructions

      ai alphaTransInitial alphaCounter alphaTransfer
      ai betaTransInitial betaCounter betaTransfer

      -- Having "i" in the first address is very useful
      shift counterI (left 22) shiftedI
      -- Select parameter info

      ai parameterSelect shiftedI param
      param <- empty

      -- Form instructions

      ai normTemplate shiftedI normalizeInstr -- I _ 0001
      ai incrTemplate shiftedI incrInstr -- + _ 1081 _
      ai incrInstr    counterI incrInstr

      let k0 = Absolute 0x0001
      --
      --      v this should be the index into the info table
      shift (header `offAddr` 4) (left 22) k0

      ai kInitial k0 kTransfer

      tN' (header `offAddr` 4) counterK

      chain (op 2)

    {-
    Op. 2 selects the instruction from block K.
    -}
    let selected = Unknown "selected"
    kTrans <- operator 2 $ do
      clcc kTransfer

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

  -- important to use global memory for this value because it'll be passed to the
  -- control instruction block
  iinShifted <- global "iinShifted" Cell

  let iin = Absolute 0x001 -- cell which holds the current part of the parameter we are analyzing

  operator 4 $ mdo
    bitAnd parameterInfo firstAddr iin
    shift iin (right 22) iinShifted
    tN' counterK parenCode

    chain (op 5)
  {-
  Op. 5 verifies if i_in is equal to zero (YES -- op. 7, NO -- op. 6).
  -}

  operator 5 $ do
    compWord iin zero (op 6) (op 7)

  let vf = header `offAddr` 1

  {-
  Op. 6 comparing "i_in" with the quantity V_f, verifies if i_in is variable
  (YES -- op. , NO -- op. 7).
  -}
  operator 6 $ do
    comp iinShifted vf (op 8) (op 7)
  {-

  Op. 7 forms the instruction for dispatching the initial value of the
  parameter and transfers it to block alpha.
  -}
  let mkTn = Absolute 0xC

  operator 7 $ do
    ai iin counterI alphaBuilder
    ce' alphaBuilder mkTn alphaBuilder

    clcc alphaTransfer

    chain (op 9)
  {-
  Op. 8 sends to the standard cell t the code of the parameter "i" and
  transfers control to the sub-routine for forming the instructions for
  calculating the variable i_sigma (op. 15 - op. 32), which form the
  instructions for calculating i_in and transsfers them to the block.
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

  let ifin = Absolute 0x001

  operator 11 $ do
    bitAnd parameterInfo secondAddr ifin
    shift ifin (right 11) ifin
    comp ifin vf (op 13) (op 12)

  {-
  Op. 12 completes the comparison instruction for the case of i_fin independent
  of higher-order parameters.

  Should this transfer?
  -}

  operator 12 $ do
    ai builtComp ifin builtComp

    cccc (Procedure "MP-2" (op 4))

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

    cccc (Procedure "MP-2" (op 4))
  {-
  Op. 14 forms the comparison for a characteristic loop.
  -}

  operator 14 $ do
    shift parameterInfo (left 11) builtComp
    addE  builtComp parameterInfo builtComp
    cccc (Procedure "MP-2" (op 4))

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
  let i0 = wm `offAddr` 2
  let k  = wm `offAddr` 3
  let j' = wm `offAddr` 4
  let j  = wm `offAddr` 5
  let a  = wm `offAddr` 6

  let iSigma = Absolute 0x0001

  let firstHalf = wm `offAddr` 0
  let secondHalf = wm `offAddr` 1
  let firstParamSelect = Unknown ",TN _ firstHalf"

  operator 15 $ mdo
    ai firstParamSelect iSigma mkFirstHalf -- ,TN _ firstHalf
    mkFirstHalf <- empty

    ai mkFirstHalf oneFirstAndThird mkSecondHalf -- +1 _ +1
    mkSecondHalf <- empty

    bitAnd firstHalf firstAddr i0

    bitAnd firstHalf secondAddr a
    shift a (left 11) a

    bitAnd secondHalf secondAddr j
    shift j (left 11) j'

    bitAnd secondHalf thirdAddr k
    shift k (left 11) k

    chain (op 16)
  {-
  Op. 16 refers to op. 17 if "k"= 0 and to op. 23 if "k" != 0
  -}
  operator 16 $ do
    compMod oneSndAddr k (op 23) (op 17)

  {-
  Op. 17 refers to op. 18 if "A" = 0 and to op. 21 if "A" != 0
  -}
  operator 17 $ do
    compMod oneFirstAddr a (op 21) (op 18)

  {-
  Op. 18 refers to op. 19 if "i0" = 0, and to op. 20 if "i0" != 0.
  -}
  operator 18 $ do
    compMod oneFirstAddr i0 (op 20) (op 19)

  {-
  Op. 19 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ T   │ "j"  │      │      │
    └─────┴──────┴──────┴──────┘

  and sets it in the standard cell S.
  -}
  operator 19 $ do
    ce j (Absolute 0x14) cellS

    chain (op 32)

  {-
  Op. 20 forms the instruciton:

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "i0" │ "j"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in the standard cell S.
  -}
  operator 20 $ do
    ai i0 j cellS
    ce cellS (Absolute 0x1) cellS

    chain (op 32)
  {-
  Op. 21 forms the instruction:

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "A"  │ "j"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in cell S.
  -}
  operator 21 $ do
    ai a j cellS
    ce cellS (Absolute 0x3) cellS

    chain (op 22)

  {-
  Op. 22 refers to op. 32 if "i0" = 0 and to op. 31 if "i0" != 0.
  -}
  operator 22 $ do
    compMod oneFirstAddr i0 (op 31) (op 32)
  {-
  Op. 23 according to information on i_sigma, forms the constant

    ┌─────┬──────┬──────┬──────┐
    │     │ "B"  │      │      │
    └─────┴──────┴──────┴──────┘
  -}

  let b = wm `offAddr` 7

  operator 23 $ do
    shift iSigma (left 22) b

    chain (op 24)

  {-
  Op. 24 refers to op. 25 if "A" = 0 and to op. 28 if "A" != 0
  -}
  operator 24 $ do
    compMod oneFirstAddr a (op 28) (op 25)

  {-
  Op. 25 refers to op. 26 if "B" = 0 and to op. 27 if "B" != 0
  -}
  operator 25 $ do
    compMod oneFirstAddr b (op 27) (op 26)

  {-
  Op. 26 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "j"  │ "k"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in cell S.
  -}
  operator 26 $ do
    ai j k cellS
    ce cellS (Absolute 1) cellS

    chain (op 22)

  {-
  Op. 27 forms the instruction (rho_2 = 0001, rho_1 = 0002)

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "B"  │ "k"  │ 0001 │
    └─────┴──────┴──────┴──────┘

  transfers it to block alpha, constructs the instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ 0001 │ "j"  │      │
    └─────┴──────┴──────┴──────┘

  and transfers it to cell s.
  -}
  let template1 = Unknown "x _ _ 0001"
  let template2 = Unknown "+ 0001 _ _"
  operator 27 $ do
    ai b k alphaBuilder
    ai template1 alphaBuilder alphaBuilder

    clcc alphaTransfer

    ai template2 j cellS

    chain (op 22)

  {-
  Op. 28 refers to op. 29 if "B" = 0, and to op. 30 if "B" != 0.
  -}
  operator 28 $do
    compMod oneFirstAddr b (op 30) (op 29)

  {-
  Op. 29 forms the instruction

    ┌─────┬──────┬──────┬──────┐
    │ X   │ "A"  │ "j"  │ 0001 │
    └─────┴──────┴──────┴──────┘

  transfers it to block alpha, constructs the instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ 0001 │ "A"  │      │
    └─────┴──────┴──────┴──────┘

  and sets it in cell S.
  -}
  operator 29 $ do
    ai a j alphaBuilder
    ai template1 alphaBuilder alphaBuilder -- x _ _ 0001

    clcc alphaTransfer

    ai template2 a cellS -- + 0001 _ _

    chain (op 22)

  {-
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
  -}
  let template3 = Unknown "x _ _ 0002"
  let template4 = Unknown "+ 0001 0002"
  operator 30 $ do
    ai b k alphaBuilder
    ai template3 alphaBuilder alphaBuilder -- x _ _ 0002

    clcc alphaTransfer

    ai a j alphaBuilder
    ai template1 alphaBuilder alphaBuilder -- x _ _ 0001

    clcc alphaTransfer

    tN' template4 cellS -- + 0001 0002 _

    chain (op 22)
  {-
  Op. 31 adds 0001 to the third address of the instruction standing in cell S,
  transfers the constructed instruction to block alpha and then forms the
  instruction

    ┌─────┬──────┬──────┬──────┐
    │ +   │ "i0" │ 0001 │      │
    └─────┴──────┴──────┴──────┘

  and sets in cell S.
  -}

  let template5 = Unknown "+ _ 0001 _"

  operator 31 $ do
    add' unity cellS cellS

    clcc alphaTransfer

    ai template5 i0 cellS -- + _ 0001 _

    chain (op 32)

  {-
  Op. 32 adds (t) to the instruction standing in cell S and transfers the
  constructed instruction to block alpha.

  -}

  operator 32 $ do
    add' cellS cellT cellS
    clcc alphaTransfer
    retRTC

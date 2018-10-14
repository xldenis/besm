{-# LANGUAGE RecursiveDo, BinaryLiterals #-}
module Besm.PP1.Logical where

{-

  Defn: Internal instructions have either bar(N) or 0202 in the third address.
  Defn: Output addresses have N_1, N_2, ..., N_k in the third address.

  In accordance with teh algorithm for programming operators (section 13) the
  block 1-PP-1 consists of three basic parts.

  The first part (operators 1 -4, 12- 34) realies the algorithm for
  constructing an elementary logical operator from information on the next
  segment, extracted from the logical operator. The functioning of the first
  part ois repeated with teh choice of each new line of information on a
  logical operator.

  The second part (operators 35- 45) realizes the "purging" algorithm and
  transfers the formed instruction to the block of completed instructions. The
  second part functions during the constructionof each new instruction of the
  logical operator.

  The third part (operators 5-11, using in their functioning operators (42
  -45)), functions once, after which the selected line of information no
  longer constitutes information on a logical operator.

  We present once more the table of elementary logical operators (sec 12), in
  which instructions of a single type are written on the same lines.

  As is evident from the table, each elementary logical operator consists of
  not more than four instructions, which will be termed respectively "first
  comparison", "intermediate CCCC", "second comparison" and "last CCCC". Thus,
  for example, an operator for a segment of type 1 consists of the first
  comparison and the last CCCC, while an operator for a segment of type 18
  contains instructions of all four forms. In accordance with this, in the
  first part of block 1-PP-1 there are operators which construct each  of
  these instructions. The first comparison is constructed by operators 12-20,
  the intermediate CCCC by operators 23-23, the second comparison operators
  25-30, and the last CCCC by operators 32-33. IN addition, in each of these
  groups of operators there are instructions, verifying whether for the given
  type of segment it is necessary to construct the corresponding instruction,
  and if so, in which form.

  Let us consider the functioning of the first part of block 1-PP-1. THis
  block begins its functioning when the first line of infromation on the
  logical operator is already placed in cell A.
-}
import Besm.Assembler.Monad
import Besm.Assembler.Syntax
import qualified Data.Bits as B (shift)

cellC = Unknown "C"

four = Absolute (unsafeFromBesmAddress "1084")
zero = Absolute 0
two  = Absolute (unsafeFromBesmAddress "1082")

constantMap =
  [ ("constant-x", Raw 0)
  , ("constant-n", Raw 0)
  , ("2", Raw 2)
  , ("3", Raw 3)
  , ("8", Raw 8)
  , ("18", Raw 18)
  , ("4", Raw 4)
  , ("concluding transfers", Raw 0)
  , ("α", Size 4)
  , ("Y", Cell)
  , ("Y'", Cell)
  , ("<", Raw 0)
  , ("0202", Raw 0x202)
  , ("firstAndSndAddr", Raw 0)
  , ("scratch-cell-1", Cell)
  , ("scratch-cell-2", Cell)
  , ("scratch-cell-3", Cell)
  , ("x", Cell)
  , ("b", Cell)
  ]

pp1_1 = do
  let constantX = Unknown "constant-x"
  let constantN = Unknown "constant-n"

  let alpha1 = Unknown "α" `offAddr` 0
  let alpha2 = Unknown "α" `offAddr` 1
  let alpha3 = Unknown "α" `offAddr` 2
  let alpha4 = Unknown "α" `offAddr` 3

  let cellK0 = alpha1
  let cellK1 = alpha2
  let cellK2 = alpha3
  let cellK3 = alpha4

  let cellY = Unknown "Y"
  let eight = Unknown "8"

  let purge = callRtc (op 35) (op 45)

  {-

    Op. 1 forms the constants

    ┌───┬────┬─────┬───┐  ┌───┬───┬───┬───┐
    │   │    │ "x" │   │  │   │   │   │ N̅ │
    └───┴────┴─────┴───┘, └───┴───┴───┴───┘

    Notes:

    Since we are coming from MP-1 the first line of information on the logical
    operator is already in cell A with the form:

    ┌─────┬────┬─────┬───┐
    │ 018 │    │ "x" │ N̅ │
    └─────┴────┴─────┴───┘
  -}

  operator 1 $ do
    bitAnd cellA secondAddr constantX
    bitAnd cellA thirdAddr  constantN

    chain (op 2)

  {-
    Op. 2 selects the next line of information.

    Note
    ====
    Has the form:
    ┌─────┬────┬─────┬───┐
    │ y   │ a  │ b   │ N │
    └─────┴────┴─────┴───┘

    where y = segment type, a, b are the bounds, N is the desired operator code.
  -}

  operator 2 $ do
    let _mp1_17 = Procedure "MP-1" (op 17)
    let _mp1_20 = Procedure "MP-1" (op 20)

    callRtc _mp1_17 _mp1_20

    chain (op 3)
  {-
    Op. 3 extracts the third address of this line.
  -}

  operator 3 $ do
    bitAnd cellA thirdAddr cellB
    chain (op 4)
  {-
    Op. 4 verifies if the extracted address is vacant. If it is not vacant,
    control is transferred to op. 12, and in the opposite case to op. 5, the
    third and concluding part of block 1-PP-1.
  -}

  operator 4 $ do
    compWord zero cellB (op 12) (op 5)

  {-
    We shall term a comparison containing "x" in the first address a direct
    comparison, and a comparison having "x" in the second address, inverse.

    Op. 12 extracts from the next line of information the number of the type of
    segment y and forms the preparation

      ┌───┬────┬────┬──────┐
      │ < │    │    │ 0202 │
      └───┴────┴────┴──────┘


    for the internal comparison and

      ┌───┬─────┬─────┬──────┐
      │   │ "a" │ "x" │      │
      └───┴─────┴─────┴──────┘

    for the inverse comparison.

  -}

  operator 12 $ mdo

    {-

      Holds the value:
      ┌───┬────┬────┬──────┐
      │   │    │    │ 0202 │
      └───┴────┴────┴──────┘
    -}

    let template = Unknown "0202"
    tExp' cellA cellY

    comp cellY eight direct inverse


    inverse <- block $ do
      bitAnd cellA firstAddr cellC
      ai constantX cellC alpha1

      chain direct

    direct <- block $ mdo
      comp1 <- comp cellY four internal comp2
      comp2 <- comp eight cellY internal (op 13)

      return comp1

    internal <- block $ do
      ai' alpha1 template alpha1

      chain (op 13)
    return ()
  {-

  Op. 13 for y > 8 transfers control to op. 19, forming for this case the
  internal inverse comparison.

  -}

  operator 13 $ do
    comp eight cellY (op 19) (op 14)

  {-
  For y <= 8

  Op. 14 forms the preparation

    ┌───┬────┬────┬──────┐
    │ < │    │    │    N │
    └───┴────┴────┴──────┘

  for the output comparison.

  Notes
  =====

  Output comparison are types 4, 8

  Therefore we need to check 4 <= y <= 8
  -}

  operator 14 $ mdo
    comp cellY four (op 15) template -- if Y < 4 then next else apply template

    template <- block $ do
      let compTemplate = Unknown "<" -- cell with just < code
      ai' alpha1 compTemplate alpha1
      ai' alpha1 cellB alpha1 -- we stored N in cell B in operator 3x

      chain (op 15)

    return ()
  {-
  Op. 15 for y = 8 transfers control to op. 19, forming for this case the
  output inverse comparison.


  Notes
  =====
  We already know that y <= 8 from Op. 14, the possible values are 1, 2, 3, 4, 8

  Can be combined with teh conditional in op 14.
  -}

  operator 15 $ do
    comp cellY eight (op 16) (op 19) -- if y < 8 then (op 19) else (op 16)

  {-

  For y < 8

  Op. 16 forms the preparation

  ┌───┬─────┬─────┬──────┐
  │   │ "x" │ "a" │      │
  └───┴─────┴─────┴──────┘

  for the direct comparison.
  -}

  operator 16 $ do
    shift constantX (left 11) cellC
    ai alpha1 cellC alpha1

    bitAnd cellA firstAddr cellC
    shift cellC (left 11) cellC
    ai alpha1 cellC alpha1

    chain (op 17)

  {-
  Op. 17 for y > 3 transfers control to op. 19, forming for this case teh
  output direct comparison.
  -}
  operator 17 $ do
    let three = Unknown "3"
    comp three cellY (op 19) (op 18)

  {-
  For y <= 3

  Op. 18 forms the preparation

  ┌───┬────┬────┬──────┐
  │ < │    │    │    N̅ │
  └───┴────┴────┴──────┘

  for the internal comparison.

  -}
  operator 18 $ do
    ce constantN (Absolute 0x14) alpha1

    chain (op 19)

  {-
  Op. 19 forms the the first comparison.

  NOTES
  =====

  Uh what?

  Unclear what "forming the first comparison" means!

  I suppose if all the 'preparations' were held in special memory and then added
  here that could make sense...
  -}
  operator 19 $ do
    chain (op 20)

  {-
  Op. 20 transfers control to the subroutine "purging".
  -}

  operator 20 $ do
    purge

    chain (op 21)

  {-
  Op. 21 determines if it is necessary to construct an intermediate CCCC and
  for y >= 18

  -}

  operator 21 $ do
    let eighteen = Unknown "18"
    comp cellY eighteen (op 24) (op 22)

  {-
  Op. 22 constructs the intermediate CCCC.
  -}

  operator 22 $ do
    ce constantN (Absolute 0x1B) alpha1

    chain (op 23)
  {-
  Op. 23 transfers control to the sub-routine "purging".
  -}

  operator 23 $ do
    purge

    chain (op 24)

  {-
  Op. 24 extracts y', the contents of the two lowest orders in the number of
  the type of segment. The coding of types is so chosen that according to the
  quantity y' it is possible to decide what type of instructions are necessary
  to construct for the right end of the segment.

  -}

  let cellY' = Unknown "Y'"
  operator 24 $ do
    bitAnd cellY four cellY' -- four == 2^3 * 0.5 this means that the two lowest bits of the exp are 1
    tExp cellY' cellY' -- objective is that cellY' holds a normalized version of those bits.

    chain (op 25)

  {-
  Op. 25 determines the case y' = 0 (y = 4, y = 8), for which the construction
  of the elementary operator has already been compelted and transfers control
  to selection of the following line of information.

  -}
  operator 25 $ do
    compWord cellY' unity (op 26) (op 34)
  {-

  Op. 26 determines the case y' = 1, for which only the last CCCC remains to
  be constructed, formed by op. 32

  -}

  operator 26 $ do
    comp cellY' two (op 32) (op 27)

  {-
  Op. 27 forms the preparations

    ┌───┬────┬────┬──────┐     ┌───┬─────┬─────┬─────┐
    │ < │    │    │    N │     │   │ "x" │ "b" │     │
    └───┴────┴────┴──────┘,    └───┴─────┴─────┴─────┘

  for the output direct comparison.
  -}

  let xVal = Unknown "x"
  let bVal = Unknown "b"

  operator 27 $ do
    ce cellB (Absolute 0x1b) alpha1

    shift constantX (left 11) xVal
    bitAnd cellA secondAddr   bVal

    chain (op 28)
  {-
  Op. 28, for y' = 3 (y = 3, y = 19), transfers control to op. 30, forming for
  this case the output direct comparison.

  -}

  operator 28 $ do
    compWord two cellY' (op 29) (op 30)

  {-
  For y' != 3, ie for y'=2, (y = 2, y =18)

  Op. 29 forms the preparations

    ┌───┬────┬────┬──────┐     ┌───┬─────┬─────┬─────┐
    │ < │    │    │ 0202 │     │   │ "b" │ "x" │     │
    └───┴────┴────┴──────┘,    └───┴─────┴─────┴─────┘

  for the internal inverse comparison.
  -}
  operator 29 $ do
    let template = Unknown "0202"

    ce template (Absolute 0x14) alpha1

    shift xVal (right 11) xVal
    shift bVal (left  11) bVal

    chain (op 30)


  {-
  Op. 30 forms the second comparison and transfers control to the sub-routine
  "purging".
  -}
  operator 30 $ do
    ai alpha1 bVal alpha1
    ai alpha1 xVal alpha1
    purge
    chain (op 31)

  {-

  Op. 31 determines if it is necessary to construct the last CCCC, and for
  y'=2 (y = 2, y = 18) transfers control to op. 32.
  -}

  operator 31 $ do
    compWord cellY' two (op 34) (op 32)

  {-
  Op. 32 constructs the last CCCC.
  -}
  operator 32 $ do
    ce cellB (Absolute 0x1b) alpha1

    chain (op 33)

  {-
  Op. 33 transfers control to the sub-routine "purging".
  -}
  operator 33 $ do
    purge
    chain (op 34)

  {-
  Op. 34 sends the next line of information to selection.
  -}

  operator 34 $ do
    chain (op 2)

  {-
  Let us consider the functioning of the second part of the block 1-PP-1,
  realizing the algorithm "purging" S 12.

  Op. 35 extracts the first two addresses from instructions K_1, K_2, K_3.
  -}
  let temp1 = Unknown "scratch-cell-1"
  let temp2 = Unknown "scratch-cell-2"
  let temp3 = Unknown "scratch-cell-3"

  operator 35 $ do
    let firstAndSndAddr = Unknown "firstAndSndAddr"
    bitAnd cellK1 firstAndSndAddr temp1
    bitAnd cellK2 firstAndSndAddr temp2
    bitAnd cellK3 firstAndSndAddr temp3

  {-
  Op. 36 verifies if K_1 is a comparison. If K_1 is CCCC, control is
  transferred to op. 42.

  NOTES
  =====

  Check if the first two arguments of k1 are empty (which implies a CCCC)
  -}

  operator 36 $ do
    compWord cellK1 unity (op 42) (op 37)

  {-
  If K_1 is comparison then op. 37 and op. 38 verify if the first two
  addresses of instructions K_1 and K_2 coincide. In the case of coincidence
  control is transferred to op. 41

  NOTES
  =====

  The only reason this would take two operators is if we extracted the first
  and second address separately.
  -}
  operator 37 $ do
    compWord temp1 temp2 (op 39) (op 41)


  {-
  In the case of non-coincidence

  Op. 39 verifies if the first two addresses of instructions K_1 and K_3
  coincide.
  -}
  operator 39 $ do
    compWord temp1 temp3 (op 42) (op 40)

  {-

  In the case of coincidence

  Op. 40 transfers K_1 to the place of K_3 and strikes out CCCC, standing in
  cell α+1.

  -}
  operator 40 $ do
    tN' cellK1 cellK3
    tN' zero alpha1

    chain (op 41)

  {-

  Op. 41 eliminates comparison K_1.

  -}
  operator 41 $ do
    tN' zero cellK1


  {-
  Op. 42 compares the contents of cell α+4 with the constant c, having the
  form

  ┌───┬────┬────┬──────┐
  │   │    │    │ 13FF │
  └───┴────┴────┴──────┘

  If (α+4) < c, operators 43 and 44 are by-passed. This form of constant c is
  explained by the fact that N may be added in the third address of the
  elminated comparison by op. 8.

  -}
  operator 42 $ do
    comp alpha4 thirdAddr (op 45) (op 43)

  {-
  Op. 43 and op. 44 transfer K3 to the block of completed instructions.

  NOTES
  =====

  The book says it calls MP(2) but that seems wrong.. the correct subroutine is at MP(21).
  What does op 44 do then though?


  -}

  operator 43 $ do
    let cellA1 = Unknown "A + 1"
    let _mp1_21 = Procedure "MP-1" (op 21)
    let _mp1_23 = Procedure "MP-1" (op 23)

    tN' cellK3 cellA1
    callRtc _mp1_21 _mp1_23

  {-
  Op. 45 realizes "shift":

  (α+3) -> α+4, (α+2) -> α+3, (α+1) -> α+2

  and clears α+1.

  -}

  operator 45 $ do
    tN' alpha3 alpha4
    tN' alpha2 alpha3
    tN' alpha1 alpha2
    tN' zero   alpha1

    retRTC
  {-
  In the third, concluding part of the block 1-PP-1

  Op. 5 clears the "counter of concluding transfers".

  -}

  let counterTransfer = Unknown "concluding transfers"

  operator 5 $ do
    tN' zero counterTransfer
    chain (op 6)
  {-

  Op. 6, in accordance with rule III Sec 12 verifies if it is necessary to
  insert in the programme the instruction

    ┌──────┬────┬────┬──────┐
    │ CCCC │    │    │    N̅ │
    └──────┴────┴────┴──────┘

  ======
  The rule appears to be: if cell α+2 is a comparison then we set α+1 to a CCCC
  If it is a CCCC then we change α+3 to point to N̅
  -}

  operator 6 $ do
    comp (Absolute 0x110F) alpha2 (op 7) (op 8)

  {-
  Op. 7 places the instruction

    ┌──────┬────┬────┬──────┐
    │ CCCC │    │    │    N̅ │
    └──────┴────┴────┴──────┘

  in cell α+1
  -}
  operator 7 $ do
    ce constantN (Absolute 0x1b) alpha1

    chain (op 9)
  {-
  Op. 8 sets N in the third address of the comparison located in the cell α+3.
  -}

  operator 8 $ do
    let firstAndSndAddr = Unknown "firstAndSndAddr"
    bitAnd alpha3 firstAndSndAddr alpha3

    ai alpha3 constantN alpha3

    chain (op 9)
  {-
  Op. 9 transfers control to the sub-routine "purging". Since op. 9 consists
  only of a single instruction, the instruction of RTc located at the end of
  the sub-routine will transfer control to op. 10
  -}
  operator 9 $ do
    purge

    chain (op 10)
  {-
  Op. 10 adds 1 to the counter of concluding transfers.
  -}
  operator 10 $ do
    add' unity counterTransfer counterTransfer

  {-
  Op. 11, using the counter of concluding transfers, ensures repitition of
  operators 42-45 and 10 three times. Transfer of control from op. 45 to op.
  10 is realized by the instruction of RTc, formed during the sub-routine
  "purging" after op. 9. In fulfilling these operators the remaining
  instructions in cells α+4, α+3 and α+2 are transferred to the block of
  completed instructions while α+1, ..., α+4 themselves are cleared.

  After termiantion of the functioning of the block of logical operators
  control is transferred to op. 3 of MP-1 for examining the selected line, no
  longer constituting information on the given logical operators.
-}
  operator 11 $ mdo
    let _mp1_3 = Procedure "MP-1" (op 3)
    comp counterTransfer four b11 _mp1_3

    b11 <- block $ do
      callRtc (op 42) (op 45)
      chain (op 10)
    return ()

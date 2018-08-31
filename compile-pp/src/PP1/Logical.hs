module PP1.Logical where

{-
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


  Op. 1 forms the constants

  +-----------+  +-----------+
  | | | "x" | |  | | |   | N |
  +-----------+, +-----------+

  Op. 2 selects the next line of information.

  Op. 3 extracts the third address of this line.

  Op. 4 verifies if the extracted address is vacant. If it is not vacant,
  control is transferred to op. 12, and in the opposite case to op. 5, the
  third and concluding part of block 1-PP-1.

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

  Op. 13 for y > 8 transfers control to op. 19, forming for this case the
  internal inverse comparison.

  For y <= 8

  Op. 14 forms the preparation

    ┌───┬────┬────┬──────┐
    │ < │    │    │    N │
    └───┴────┴────┴──────┘
  for the output comparison.

  Op. 15 for y = 8 transfers control to op. 19, forming for this case the
  output inverse comparison.

  For y < 8

  Op. 16 forms the preparation

  ┌───┬─────┬─────┬──────┐
  │   │ "x" │ "a" │      │
  └───┴─────┴─────┴──────┘

  for the direct comparison.

  Op. 17 for y > 3 transfers control to op. 19, forming for this case teh
  output direct comparison.

  For y <= 3

  Op. 18 forms the preparation

  ┌───┬────┬────┬──────┐
  │ < │    │    │    N │
  └───┴────┴────┴──────┘

  for the internal comparison.

  Op. 19 forms the the first comparison.

  Op. 20 transfers control to the subroutine "purging".

  Op. 21 determines if it is necessary to consrtruct an intermediate CCCC and
  for y >= 18

  Op. 22 constructs the intermediate CCCC.

  Op. 23 transfers control to the sub-routine "purging".

  Op. 24 extracts y', the contents of the two lowest orders in teh number of
  the type of segment. The coding of types is so chosen that according to the
  quantity y' it is possible to decide what type of instructions are necessary
  to construct for the right end of the segment.

  Op. 25 determines the case y' = 0 (y = 4, y = 8), for which the construction
  of the elementary operator has already been compelted and transfers control
  to selection of the following line of information.

  Op. 26 determines the case y' = 1, for which only the last CCCC remains to
  be constructed, formed by op. 32

  Op. 27 forms the preparations

    ┌───┬────┬────┬──────┐     ┌───┬─────┬─────┬─────┐
    │ < │    │    │    N │     │   │ "x" │ "b" │     │
    └───┴────┴────┴──────┘,    └───┴─────┴─────┴─────┘

  for the output direct comparison.

  Op. 28, for y' = 3 (y = 3, y = 19), transfers control to op. 30, forming for
  this case the output direct comparison.

  For y' != 3, ie for y'=2, (y = 2, y =18)

  Op. 29 forms the preparations

    ┌───┬────┬────┬──────┐     ┌───┬─────┬─────┬─────┐
    │ < │    │    │ 0202 │     │   │ "b" │ "x" │     │
    └───┴────┴────┴──────┘,    └───┴─────┴─────┴─────┘

  for the internal inverse comparison.

  Op. 30 forms the second comparison and transfers control to the sub-routine
  "purging".

  Op. 31 determines if it is necessary to construct the last CCCC, and for
  y'=2 (y = 2, y = 18) transfers control to op. 32.

  Op. 32 constructs the last CCCC.

  Op. 33 transfers control to the sub-routine "purging".

  Op. 34 sends teh next line of information to selection.

  Let us consider the functioning of the second part of the block 1-PP-1,
  realizing the algorithm "purging" S 12.

  Op. 35 extracts the first two addresses from instructions K_1, K_2, K_3.

  Op. 36 verifies if K_1 is a comparison. If K_1 is CCCC, control is
  transferred to op. 42. If K_1 is comparison then op. 37 and op. 38 verify if
  the first two addresses of instructions K_1 and K_2 coincide. In the case of
  coincidence control is transferred to op. 41.

  In the case of non-coincidence

  Op. 39 verifies if the first two addresses of instructions K_1 and K_3
  coincide. In the case of coincidence

  Op. 40 transfers K_1 to the place of K_3 and strikes out CCCC, standing in
  cell α+1.

  Op. 41 eliminates comparison K_1.

  Op. 42 compares the contents of cell α+4 with the constant c, having the
  form

  ┌───┬────┬────┬──────┐
  │   │    │    │ 1355 │
  └───┴────┴────┴──────┘

  If (α+4) < c, operators 43 and 44 are by-passed. This form of constant c is
  explained by the fact that N may be added in the third address of the
  elminated comparison by op. 8.

  Op. 43 and op. 44 transfer K3 to the block of completed instructions.

  Op. 45 realizes "shift":

  (α+3) -> α+4, (α+2) -> α+3, (α+1) -> α+2

  and clears α+1.

  In the third, concluding part of the block 1-PP-1

  Op. 5 clears the "counter of concluding transfers".

  Op. 6, in accordance with rule III Sec 12 verifies if it is necessary to
  insert in the programme the instruction


    ┌──────┬────┬────┬──────┐
    │ CCCC │    │    │    N │
    └──────┴────┴────┴──────┘

  Op. 7 places the instruction

    ┌──────┬────┬────┬──────┐
    │ CCCC │    │    │    N │
    └──────┴────┴────┴──────┘

  in cell α+1

  Op. 8 sets N in the third address of the comparison located in the cell α+3.

  Op. 9 transfers control to the sub-routine "purging". Since op. 9 consists
  only of a single instruction, the instruction of RTc located at the end of
  the sub-routine will transfer control to op. 10

  Op. 10 adds 1 to the counter of concluding transfers.

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

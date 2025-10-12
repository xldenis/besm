{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}

module Besm.PP3 where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7


{-

  The third part of the programming programme contains approximately 450
  instructions and 30 constants and consists of four blocks, functioning in
  succession one after the other: the block for forming relative addresses
  (I-PP-3), the block for distributing the store (OO-PP-3), the block for
  assigning true addresses (III-PP-3) and the block for printing information
  (IV-PP-3). The functioning of PP-3 is organized by a small master programme
  (MP-3), containing also the standard sub-routine for printing instructions in
  reference to them by the instruction found in cell 0001. This sub-routine is
  taken from the library of standard programmes and therefore its description
  will not be presented here.

  During the functioning of PP-3 the store contains MP-3 and one of four
  blocks of PP-3, called up when necessary, since the PP can engage not more
  than 256 cells of IS. Information on the problem in standard position is
  stored in IS completely. At the termination of functioning of PP-3 the
  finished programme is found on MD-1.

  PP-3 realizes the third stage of function of the programming algorithm(S 19)
  and functions in the following manner (Fig 15).
-}
mp3 = do
  pinned "header" "programme header table" (Size 15)
  {-
    Op. 1 of MP-3 reads the programme block I-PP-3 from MD-4 and transfers
    control to it. The block I-PP-3 changes the codes of the operator numbers
    in the instructions to the relative addresses of the first instructions of
    these operators, eliminates the open-parentheses of loops from the
    programme and signs of operator numbers and changes the relative address of
    the instructions in correspondence with these.
  -}
  operator 1 $ do
    readMD 4 (ProcStart "I-PP-3") (ProcEnd "I-PP-3") (ProcStart "I-PP-3")
    cccc (Procedure "I-PP-3" (op 1))
    chain (op 2)

  {-
    Op. 2 reads from MD-4 the programme of block II-PP-3 and transfers control
    to it. The block II-PP-3 carries out distribution of the store,determining
    the final location of the storage blocks employed in the solution of the
    programme and calculates the corrections which must be added to the codes
    of quantities to obtain their true addresses.
  -}
  operator 2 $ do
    readMD 4 (ProcStart "II-PP-3") (ProcEnd "II-PP-3") (ProcStart "II-PP-3")
    cccc (Procedure "II-PP-3" (op 1))
    chain (op 3)

  {-
  Op. 3 reads from MD-4 the programme of block III-PP-3 and transfers
  control to it. The block III-PP-3 carries out assignment of true
  addresses.
  -}
  operator 3 $ do
    readMD 4 (ProcStart "III-PP-3") (ProcEnd "III-PP-3") (ProcStart "III-PP-3")
    cccc (Procedure "III-PP-3" (op 1))
    chain (op 4)
  {-
  Op. 4 reads from MD-4 the programme of block IV-PP-3 and transfers
  control to it. The block IV-PP-3 prints information on the storage
  distribution, prints programme components and constants relating to them,
  composes the programme and the constants in accordance with the
  distribution of the store and writes them on MD-1, completing by this the
  functioning of the PP.
  -}
  operator 4 $ do
    readMD 4 (ProcStart "IV-PP-3") (ProcEnd "IV-PP-3") (ProcStart "IV-PP-3")
    cccc (Procedure "IV-PP-3" (op 1))
    stop
{-
  In the description of the functioning of PP-3 it is necessary to consider the two positions of blocks in the store: standard and "final", ie that which they are to have in correspondence with which position is under consiteration the characteristics of the block locations in their final positions will be denoted by an asterisk. Thus for example 𝜎*f denotes the last cell of block 𝜎 in its final position in the store in distinction to 𝜎f, denoting hte same characteristic in the standard position of block 𝜎.
-}

{-
  Block for the formation of relative addresses

  The block I-PP-3 consists of the basic programme (operators 1-33) and four sub-routines:
    the sub-routines for forming relative addresses (operators 34-38),
    the sub-routine for changing relative addresses (operators 39- 45),
    the sub-routine for selecting instructions from block K (op. 46)
    and the sub-routine for transferring instructions to block K (op. 47).

  The first part of the basic programme (operators 1-13) carries out the
  substitution in the instructions of the operator numbers by the relative
  addresses of the first instructions of the operators with the given
  numbers. Operators 1-13 organize the sequence of selection of programme
  instruction addresses and return of instructions wit changed addresses to
  the programme. The direct testing of addresses and the substitution of
  operator numbers by relative addresses is realized by the sub-routine for
  forming relative addresses.
-}

pp3_1 = do
  -- Shared with PP-2 (pinned)
  counterK <- global "k" Cell
  counterKPrime <- global "k'" Cell  -- Counter for subroutines
  cellA <- local "A" Cell
  cellB <- local "B" Cell
  currentInstr <- local "currentInstr" Cell

  -- Templates for instruction matching
  maTemplate <- local "maTemplate" (Template (Ma zero zero zero))
  mbTemplate <- local "mbTemplate" (Template (Mb zero))
  opcodeTemplate <- local "0x18-template" (Raw 0x18)

  -- Counter for true addresses (ops 26-33)
  trueAddrCounter <- global "true-addr-counter" Cell

  -- Delta - magnitude of relative address
  delta <- local "delta" Cell

  -- Count of eliminated items
  eliminatedCount <- local "eliminated-count" Cell

  {-
  Op. 1 sends K0 to counter K, in which is fixed the address of the last instruction selected from block K.
  -}
  operator 1 $ do
    -- todo: missing instructions here
    tN' (header `offAddr` 4) counterK
    chain (op 2)

  {-
  Op. 2 selects the next instruction in block K and adds unity to counter K.
  -}
  operator 2 $ mdo
    -- Address left blank
    empty
    -- Select instruction from K using subroutine
    ai counterK one counterK
    empty -- bitwise and of something
    chain (op 3)

  {-
  Op. 3 determines the case where open-parentheses of the loop or the sign of
  an operator number have been selected, transferring control to op. 12.
  -}
  operator 3 $ do
    tExp' currentInstr cellB
    compWord cellB opcodeTemplate (op 12) (op 4)

  {-
  Op. 4 in the case of selection of instructions Ma or Mb transfers control
  immediately to op. 10 since in these instructions the first two addresses
  are not examined.
  -}
  operator 4 $ do
    tExp' currentInstr cellB
    compWord cellB (Absolute 22) (op 10) (op 5)  -- Ma opcode = 22
    compWord cellB (Absolute 23) (op 10) (op 5)  -- Mb opcode = 23

  {-
  Op. 5 determines the first address of the instruction and shifts it to the
  third address of the standard cell A.
  -}
  operator 5 $ do
    shift' currentInstr (right 22) cellA
    chain (op 6)

  {-
  Op. 6 examines the extracted address by calling the relative address subroutine.
  -}
  operator 6 $ do
    clcc (op 34)
    chain (op 7)

  {-
  Op. 7 sets the tested first address in the instruction and extracts the second
  address from the latter, shifting it to the third address of cell A.
  -}
  operator 7 $ mdo
    -- Reconstruct instruction with new first address
    shift cellA (left 22) cellB
    bitAnd currentInstr (Unknown "not-first-addr") currentInstr
    ai currentInstr cellB currentInstr

    -- Extract second address
    shift' currentInstr (right 11) cellA
    bitAnd cellA thirdAddr cellA
    chain (op 8)

  {-
  Op. 8 tests the selected address.
  -}
  operator 8 $ do
    clcc (op 34)
    chain (op 9)

  {-
  Op. 9 sets the tested second address in the instruction.
  -}
  operator 9 $ mdo
    -- Reconstruct instruction with new second address
    shift cellA (left 11) cellB
    bitAnd currentInstr (Unknown "not-second-addr") currentInstr
    ai currentInstr cellB currentInstr
    chain (op 10)

  {-
  Op. 10 extracts the third address of the instruction and sets it in cell A.
  -}
  operator 10 $ do
    bitAnd currentInstr thirdAddr cellA
    chain (op 11)

  {-
  Op. 11 tests the selected address and then sets it in the instruction.
  -}
  operator 11 $ mdo
    clcc (op 34)

    -- Reconstruct instruction with new third address
    bitAnd currentInstr (Unknown "not-third-addr") currentInstr
    ai currentInstr cellA currentInstr
    chain (op 12)

  {-
  Op. 12 transfers the instruction back to block K.
  -}
  operator 12 $ do
    clcc (op 47)
    chain (op 13)

  {-
  Op. 13 transfers control to op. 2 if all instructions from block K have not been examined.
  -}
  operator 13 $ do
    comp counterK (header `offAddr` 6) (op 2) (op 14)
  {-
  Operators 14-25 realize change in relative address, connected with elimination
  from the programme of open-parentheses and signs of operator numbers.
  -}

  {-
  Op. 14 sends K0 to counter K.
  -}
  operator 14 $ do
    tN' (header `offAddr` 4) counterK
    chain (op 15)

  {-
  Op. 15 selects the next instruction from the block K and adds unity to counter K.
  -}
  operator 15 $ do
    clcc (op 46)
    ai counterK one counterK
    chain (op 16)

  {-
  Op. 16, in the case of selection of instructions Ma and Mb transfers control to
  op. 22 since in these instructions the relative addresses may be located only
  in the third address.
  -}
  operator 16 $ do
    tExp' currentInstr cellB
    compWord cellB (Absolute 22) (op 22) (op 17)
    compWord cellB (Absolute 23) (op 22) (op 17)

  {-
  Op. 17 extracts the first address of the instruction and sends it to the third
  address of the standard cell A.
  -}
  operator 17 $ do
    shift' currentInstr (right 22) cellA
    chain (op 18)

  {-
  Op. 18 examines the extracted address using the relative address change subroutine.
  -}
  operator 18 $ do
    clcc (op 39)
    chain (op 19)

  {-
  Op. 19 sets the tested address in the instruction, extracts the second address
  and sends it to the third address of cell A.
  -}
  operator 19 $ mdo
    -- Reconstruct with modified first address
    shift cellA (left 22) cellB
    bitAnd currentInstr (Unknown "not-first-addr") currentInstr
    ai currentInstr cellB currentInstr

    -- Extract second address
    shift' currentInstr (right 11) cellA
    bitAnd cellA thirdAddr cellA
    chain (op 20)

  {-
  Op. 20 tests the extracted address.
  -}
  operator 20 $ do
    clcc (op 39)
    chain (op 21)

  {-
  Op. 21 sends the tested address to the instruction.
  -}
  operator 21 $ mdo
    shift cellA (left 11) cellB
    bitAnd currentInstr (Unknown "not-second-addr") currentInstr
    ai currentInstr cellB currentInstr
    chain (op 22)

  {-
  Op. 22 extracts the third address of the instruction, sending it to cell A.
  -}
  operator 22 $ do
    bitAnd currentInstr thirdAddr cellA
    chain (op 23)

  {-
  Op. 23 tests the extracted address.
  -}
  operator 23 $ do
    clcc (op 39)
    chain (op 24)

  {-
  Op. 24 sends the tested address to the instruction and transfers the
  instruction back to block K.
  -}
  operator 24 $ mdo
    bitAnd currentInstr (Unknown "not-third-addr") currentInstr
    ai currentInstr cellA currentInstr
    clcc (op 47)
    chain (op 25)

  {-
  Op. 25 transfers control to op. 15 if all instructions in block K have not been examined.
  -}
  operator 25 $ do
    comp counterK (header `offAddr` 6) (op 15) (op 26)
  {-
  Operators 26-33: Remove open-parentheses and operator signs
  Source: 9_261.gif, 9_262.gif
  -}

  {-
  Op. 26 calculates the true initial K_1* of the programme and sets it in a special
  counter for true instruction addresses.
  -}
  operator 26 $ do
    -- Calculate K_1* (true starting address of programme)
    add' (header `offAddr` 4) (Unknown "Delta-K-pp3") trueAddrCounter
    tN' (header `offAddr` 4) counterK
    chain (op 27)

  {-
  Op. 27 selects the next instruction of the programme from block K.
  -}
  operator 27 $ do
    clcc (op 46)
    ai counterK one counterK
    chain (op 28)

  {-
  Op. 28 transfers control to op. 29 if an instruction has been selected,
  to op. 30 if the open-parentheses of a loop or the sign of an operator number has been selected.
  -}
  operator 28 $ do
    tExp' currentInstr cellB
    compWord cellB opcodeTemplate (op 30) (op 29)

  {-
  Op. 29 sends the instruction back to the programme and adds unity to the true address counter.
  -}
  operator 29 $ do
    clcc (op 47)
    ai trueAddrCounter one trueAddrCounter
    chain (op 32)

  {-
  Op. 30 sets in the third address of the open-parentheses of the loop or the sign
  of the operator number the true address of the following instruction.
  -}
  operator 30 $ mdo
    -- Place true address of next instruction in third address
    bitAnd currentInstr (Unknown "not-third-addr") currentInstr
    ai currentInstr trueAddrCounter currentInstr
    chain (op 31)

  {-
  Op. 31 prints the open-parentheses of the loop or the operator number sign.
  -}
  operator 31 $ do
    -- Would print using PN instruction
    -- pn currentInstr
    chain (op 32)

  {-
  Op. 32 transfers control to op. 27 if all instructions of the programme have not been examined.
  -}
  operator 32 $ do
    comp counterK (header `offAddr` 6) (op 27) (op 33)

  {-
  Op. 33 calculates and sets in the header the new value of K_f, changed through
  elimination of the open-parentheses of loops and signs of operator numbers.
  -}
  operator 33 $ mdo
    tN' trueAddrCounter (header `offAddr` 6)
    -- Return to MP-3
    jcc

  {-
  Operators 34-38: Subroutine for forming relative addresses
  Source: 9_261.gif
  -}

  {-
  Op. 34 transfers control to op. 35 if the number of an operator is in standard cell A.
  -}
  operator 34 $ do
    -- Check if address is an operator number (>= 0x0200 && < 0x0400)
    -- If not, return via jcc
    comp cellA (Absolute 0x0200) (op 35) (op 46)
    comp cellA (Absolute 0x0400) (op 46) (op 35)

  {-
  Op. 35 shifts the operator number to the first address and sets K_1 in counter K'.
  -}
  operator 35 $ do
    shift cellA (left 22) cellA
    tN' (header `offAddr` 4) counterKPrime
    chain (op 36)

  {-
  Op. 36 selects the next instruction from block K, beginning with the first
  instruction, and adds unity to counter K'.
  -}
  operator 36 $ mdo
    clcc (op 46)
    ai counterKPrime one counterKPrime
    chain (op 37)

  {-
  Op. 37 repeats op. 36 until the sign of the given operator number is selected.
  -}
  operator 37 $ do
    -- Check if selected instruction matches operator sign (has 0x18 and matches operator)
    tExp' currentInstr cellB
    compWord cellB opcodeTemplate (op 36) (op 38)

  {-
  Op. 38 obtains the code of the relative address.
  Formula: 0200 + |(C.K') - (C.K)| for positive or 1200 + |(c.K') - (c.K)| for negative.
  -}
  operator 38 $ mdo
    -- Calculate relative address
    sub' counterKPrime counterK delta
    tMod' delta delta
    ai delta (Absolute 0x0200) cellA
    jcc

  {-
  Operators 39-45: Subroutine for changing relative addresses
  Source: 9_262.gif
  -}

  {-
  Op. 39 transfers control to op. 40 if a relative address is in standard cell A.
  -}
  operator 39 $ do
    -- Check if it's a relative address (0x0200-0x03FF or 0x1200-0x13FF)
    -- If not, return via jcc
    comp cellA (Absolute 0x0200) (op 40) (op 46)
    comp cellA (Absolute 0x0400) (op 46) (op 46)

  {-
  Op. 40 clears counter K' and standard cell B (eliminatedCount).
  -}
  operator 40 $ do
    tN' zero counterKPrime
    tN' zero eliminatedCount
    chain (op 41)

  {-
  Op. 41 selects the next instruction from the programme and adds unity to counter K'.
  -}
  operator 41 $ do
    clcc (op 46)
    ai counterKPrime one counterKPrime
    chain (op 42)

  {-
  Op. 42 determines if an open-parenthesis or operator sign was selected.
  -}
  operator 42 $ do
    tExp' currentInstr cellB
    compWord cellB opcodeTemplate (op 43) (op 44)

  {-
  Op. 43 adds 1 to cell B (eliminatedCount).
  -}
  operator 43 $ do
    ai eliminatedCount one eliminatedCount
    chain (op 44)

  {-
  Op. 44 compares counter K' with the absolute magnitude Δ of the relative address,
  repeating operators 41-43 Δ times.
  -}
  operator 44 $ do
    -- Extract magnitude from relative address
    bitAnd cellA (Absolute 0x01FF) delta
    comp counterKPrime delta (op 41) (op 45)

  {-
  Op. 45 forms the new value of the absolute magnitude: Δ - (eliminatedCount).
  -}
  operator 45 $ mdo
    sub' delta eliminatedCount delta
    -- Reconstruct relative address with new magnitude
    bitAnd cellA (Absolute 0x1E00) cellA
    ai cellA delta cellA
    jcc

  {-
  Op. 46: Subroutine for selecting instructions from block K.
  Source: 9_262.gif
  -}
  operator 46 $ do
    -- Select instruction from K[counterK] into currentInstr
    selectK <- tN' (Unknown "K") currentInstr
    ai selectK one selectK
    jcc

  {-
  Op. 47: Subroutine for transferring instructions to block K.
  Source: 9_262.gif
  -}
  operator 47 $ do
    -- Transfer instruction from currentInstr back to K[counterK]
    writeK <- tN' currentInstr (Unknown "K")
    ai writeK one writeK
    jcc

  -- Helper constants (Source: various operators)
  local "not-first-addr" (Raw $ 0b000000_11111111111_11111111111)
  local "not-second-addr" (Raw $ 0b111111_00000000000_11111111111)
  local "not-third-addr" (Raw $ 0b111111_11111111111_00000000000)
  local "Delta-K-pp3" Cell  -- Will be set by II-PP-3

  -- Pinned K block (shared with PP-2)
  pinned "K" "programme" (Size 750)

{-

  Storage Distribution Block

  In accordance with the diagram on page 70 the final distribution of the store appears as follows: parameter counters (block ), constants and variable quantities (block C), programme (block K), constants for the programme (block \ɣ), block O, M and, finally the working cells (block R). For each of these seven blocks M^i the storage distribution block (II-PP-3) forms the following quantities: M_O^i*, the address of the cell directly before the block, Mbar^i, the extent of the block and M_f*^i, the address of the last cell of the block (we note that M_f*^i = M_o^(i + 1). Then the corrections to the codes of quantities are calculated: Δ C, the correction for obtaining the true addresses of constants and variable quantities from the block C and the parameter counters, Δ K, the correction for obtaining the true addresses of programme instructions Δɣ, the correction for obtaining the true addresses of constants from block ɣ and Δ R, the correction for obtaining the true addresses of working cells.

  In addition, the block II-PP-3 carries out transformation of information on variable addresses in the block V and prints information on the variable addresses in the block V and the positions of the blocks M^i in the store. The third address of the first cell of M^i is placed in each "main head" with information on storage blocks M^i (i = 1..l); the "main head" itself with the eliminated operation code are printed in the form of an instruction. In each line of information about a variable address relating to the storage block M^i (i = 1..l), the magnitude of the block M^i is set in the first address while in the second address , the address of its start M*_l^i.
  These data will be utilized in forming the initial values of variable addresses during the period of functioning of block III-PP-3.
  the scheme of block II-PP-3 is represented in Fig. 17.

  Op 1. calculates Pbar, Cbar, Kbar, ɣbar Obar and C_0*, K_0*, ɣ_0*, O_0*, M_0*

  Op. 2 calculates the correction Δ C, forms the instructions for processing the block V and prints a number of special form, which denotes that immediately after it on the tape will be printed information on the strange blocks.

  Op. 3 selects the next line of information of block V.

  Op. 4 tests the selected line, transferring control to op. 5 if a "main head" has been selected for the next block M^i (i = 1.. l).

  Op. 5 extracts the third address of the "main head".

  Op. 6 compares the extracted address with zero. If it is not equal to zero this denotes that the corresponding block M^i relates to the group of constants from block C. In this case op. 10 functions.

  Op. 7 sets M_l^i* in the third address of the "main head" and calculates M_l^(i+1)* = M_l^i* +Mbar^i.

  Op. 8 compares M_l^(i+1)* with 03FFF.

  Op. 9 is a check stop for M_l^(i+1)* 03FFF.

  If M^i relates to the group of constants from block C,

  Op. 10 obtains the true address M_l^i*, adding Δ C to the third address of the "main head".

  Op. 11 eliminates the operation code in the "main head".

  Op. 12 prints information on the current block M^i.

  Op. 13 sets Mbar^i in the first address and M_l^i* in the second address of a certain working cell for arranging these quantities in the line of information on the variable addresses relating to the block M^i.

  If a line of information on a variable address has been selected,

  Op. 14 sets Mbar^i in the first address of this line and in the second address M_l^i*, leaving the magnitude of the shift delta of the variable address in the third address, and the sign of the shift in the eleventh place of the first address.

  Op. 15  transfers the processed line of information back to block V.

  Op. 16 carries out modification of the selection instruction address and the arrangements in op. 3 and op. 15.

  Op. 17 repeats the functioning of operators 3-16 for all lines of the block V.

  Op. 18 forms Mbar, R_O*, Rbar and R_f*.

  Op. 19 compares R_f* with 03FFF

  Op. 20 is a check stop for R_f* > 03FFF.

  Op. 21 calculates the corrections Δ ɣ, Δ R, Δ K.

-}

pp3_2 = do
  -- Shared with PP-2
  pinned "V" "V" (Size 16) -- Block V: variable address information

  -- Working cells for storage distribution
  wm <- global "working-cells-pp3" (Size 8)

  -- Storage for block sizes and addresses
  pBar <- global "Pbar" Cell
  cBar <- global "Cbar" Cell
  kBar <- global "Kbar" Cell
  gammaBar <- global "gamma-bar" Cell
  oBar <- global "Obar" Cell
  mBar <- global "Mbar" Cell
  rBar <- global "Rbar" Cell

  c0Star <- global "C_0*" Cell
  k0Star <- global "K_0*" Cell
  gamma0Star <- global "gamma_0*" Cell
  o0Star <- global "O_0*" Cell
  m0Star <- global "M_0*" Cell
  r0Star <- global "R_0*" Cell
  rfStar <- global "R_f*" Cell

  -- Corrections
  deltaC <- global "Delta-C" Cell
  deltaK <- global "Delta-K" Cell
  deltaGamma <- global "Delta-gamma" Cell
  deltaR <- global "Delta-R" Cell

  -- Counters and temp cells
  vCounter <- global "V-counter" Cell
  miStar <- global "M_i*" Cell  -- Current block final address
  mbarI <- global "Mbar_i" Cell  -- Current block size

  -- Constants
  maxAddr <- local "max-addr" (Raw 0x3FFF)

  {-
  Op. 1 calculates Pbar, Cbar, Kbar, ɣbar Obar and C_0*, K_0*, ɣ_0*, O_0*, M_0*
  -}
  operator 1 $ mdo
    -- Calculate P bar (size of parameter block)
    sub' (header `offAddr` 2) (header `offAddr` 1) pBar

    -- Calculate C bar (size of constants block)
    sub' (header `offAddr` 4) (header `offAddr` 2) cBar

    -- Calculate K bar (size of programme block)
    sub' (header `offAddr` 6) (header `offAddr` 4) kBar

    -- Calculate gamma bar (from header)
    tN' (header `offAddr` (-1)) gammaBar  -- gamma counter from PP-2

    -- Calculate O bar (block O size)
    tN' zero oBar  -- Block O is empty in this implementation

    -- Calculate M bar (assuming no M block for now)
    -- note: where did claude get this from?
    tN' zero mBar

    -- Calculate initial addresses (C_0* = P_1 + Pbar)
    add' (header `offAddr` 1) pBar c0Star
    add' c0Star cBar k0Star
    add' k0Star kBar gamma0Star
    add' gamma0Star gammaBar o0Star
    add' o0Star oBar m0Star

    chain (op 2)

  {-
  Op. 2 calculates the correction Δ C, forms the instructions for processing
  the block V and prints a number of special form
  -}
  operator 2 $ mdo
    -- Calculate Delta C: the correction to get true addresses from codes
    -- ΔC = C_0* - P_1
    sub' c0Star (header `offAddr` 1) deltaC

    -- Initialize V counter to start of block V
    tN' (Absolute 0x10) vCounter

    -- Print special marker (simplified - would use PN instruction)
    -- pn specialMarker

    chain (op 3)

  {-
  Op. 3 selects the next line of information of block V.
  -}
  operator 3 $ mdo
    -- Select from block V
    selectV <- tN' (Unknown "V") (wm `offAddr` 0)
    ai selectV oneFirstAddr selectV
    ai vCounter one vCounter
    chain (op 4)

  {-
  Op. 4 tests the selected line, transferring control to op. 5 if a "main head"
  has been selected for the next block M^i
  -}
  operator 4 $ do
    -- Check if this is a main head (operation code field != 0x18)
    tExp' (wm `offAddr` 0) (wm `offAddr` 1)
    compWord (wm `offAddr` 1) (Unknown "0x18") (op 14) (op 5)

  {-
  Op. 5 extracts the third address of the "main head".
  -}
  operator 5 $ do
    bitAnd (wm `offAddr` 0) thirdAddr (wm `offAddr` 1)
    chain (op 6)

  {-
  Op. 6 compares the extracted address with zero. If it is not equal to zero
  this denotes that the corresponding block M^i relates to the group of
  constants from block C. In this case op. 10 functions.
  -}
  operator 6 $ do
    compWord (wm `offAddr` 1) zero (op 10) (op 7)

  {-
  Op. 7 sets M_l^i* in the third address of the "main head" and calculates
  M_l^(i+1)* = M_l^i* + Mbar^i.
  -}
  operator 7 $ mdo
    -- Set M_i* in third address
    ai (wm `offAddr` 0) miStar (wm `offAddr` 0)

    -- Extract Mbar_i from second address
    shift' (wm `offAddr` 0) (right 11) (wm `offAddr` 2)
    bitAnd (wm `offAddr` 2) thirdAddr mbarI

    -- Calculate M_(i+1)* = M_i* + Mbar_i
    add' miStar mbarI miStar

    chain (op 8)

  {-
  Op. 8 compares M_l^(i+1)* with 03FFF.
  -}
  operator 8 $ do
    comp miStar maxAddr (op 9) (op 13)

  {-
  Op. 9 is a check stop for M_l^(i+1)* > 03FFF.
  -}
  operator 9 $ do
    checkStop

  {-
  Op. 10 obtains the true address M_l^i*, adding Δ C to the third address
  of the "main head".
  -}
  operator 10 $ do
    -- Add Delta C to third address
    ai (wm `offAddr` 1) deltaC miStar
    chain (op 11)

  {-
  Op. 11 eliminates the operation code in the "main head".
  -}
  operator 11 $ mdo
    -- Clear operation code (top 6 bits)
    bitAnd (wm `offAddr` 0) (Unknown "addr-mask") (wm `offAddr` 0)
    chain (op 12)

  {-
  Op. 12 prints information on the current block M^i.
  -}
  operator 12 $ do
    -- Print the main head (would use PN instruction)
    -- pn (wm `offAddr` 0)
    chain (op 13)

  {-
  Op. 13 sets Mbar^i in the first address and M_l^i* in the second address
  of a certain working cell for arranging these quantities in the line of
  information on the variable addresses relating to the block M^i.
  -}
  operator 13 $ mdo
    -- Store Mbar_i in first address of working cell
    shift mbarI (left 22) (wm `offAddr` 3)

    -- Add M_i* in second address
    shift miStar (left 11) (wm `offAddr` 4)
    ai (wm `offAddr` 3) (wm `offAddr` 4) (wm `offAddr` 3)

    chain (op 16)

  {-
  Op. 14 sets Mbar^i in the first address of this line and in the second
  address M_l^i*, leaving the magnitude of the shift delta of the variable
  address in the third address, and the sign of the shift in the eleventh
  place of the first address.
  -}
  operator 14 $ mdo
    -- Extract delta (third address)
    bitAnd (wm `offAddr` 0) thirdAddr (wm `offAddr` 5)

    -- Build new VA info: Mbar in 1st addr, M_i* in 2nd, delta in 3rd
    shift mbarI (left 22) (wm `offAddr` 6)
    shift miStar (left 11) (wm `offAddr` 7)
    ai (wm `offAddr` 6) (wm `offAddr` 7) (wm `offAddr` 6)
    ai (wm `offAddr` 6) (wm `offAddr` 5) (wm `offAddr` 0)

    chain (op 15)

  {-
  Op. 15 transfers the processed line of information back to block V.
  -}
  operator 15 $ mdo
    writeV <- tN' (wm `offAddr` 0) (Unknown "V")
    ai writeV oneFirstAddr writeV
    chain (op 16)

  {-
  Op. 16 carries out modification of the selection instruction address and
  the arrangements in op. 3 and op. 15.
  -}
  operator 16 $ do
    -- Modifications handled by ai instructions in ops 3 and 15
    chain (op 17)

  {-
  Op. 17 repeats the functioning of operators 3-16 for all lines of the block V.
  -}
  operator 17 $ do
    -- Check if we've processed all of block V (16 lines)
    comp vCounter (Absolute 0x20) (op 3) (op 18)

  {-
  Op. 18 forms Mbar, R_O*, Rbar and R_f*.
  -}
  operator 18 $ mdo
    -- R_0* = M_0* + Mbar (working cells start after M block)
    add' m0Star mBar r0Star

    -- Rbar = size of working cells block
    tN' (Absolute 0x10) rBar  -- Assuming 16 working cells

    -- R_f* = R_0* + Rbar
    add' r0Star rBar rfStar

    chain (op 19)

  {-
  Op. 19 compares R_f* with 03FFF
  -}
  operator 19 $ do
    comp rfStar maxAddr (op 20) (op 21)

  {-
  Op. 20 is a check stop for R_f* > 03FFF.
  -}
  operator 20 $ do
    checkStop

  {-
  Op. 21 calculates the corrections Δ ɣ, Δ R, Δ K.
  -}
  operator 21 $ mdo
    -- Δγ = γ_0* - 0x1A0 (gamma block starts at 0x1A0 in standard layout)
    sub' gamma0Star (Absolute 0x1A0) deltaGamma

    -- ΔR = R_0* - 0x11F0 (working cells start at 0x11F0 in standard layout)
    sub' r0Star (Absolute 0x11F0) deltaR

    -- ΔK = K_0* - 0x0200 (programme starts at 0x0200 in standard layout)
    sub' k0Star (Absolute 0x0200) deltaK

    -- Return to caller (would be via JCC or similar)
    jcc

  -- Helper constants
  local "0x18" (Raw 0x18)
  local "addr-mask" (Raw $ 0b111111111111111111111111111111111)  -- Mask out opcode

{-
  Block for Assigning True Addresses

  The block III-PP-3 substitutes their true addresses for the codes of quantities and the relative address of variable instructions in the instructions of the assembled programme. Determination of the types of quantities according to the values of their codes is cvarried out in accordance with Table 17 (S 19).

  The first part of the block (operators 1 -18) carries out assignment of true addresses. The direct substitution of codes of quantities or relative addresses by true address of quantities or instructions is carried out by a sub-routeine (operators 26 -42).

  Op. 1 sets K_0 in counter K, in which will be stored the addres of the current instruction selected from the programme during functioning of the block.

  Op. 2 transfers the next instruction from the block K to the stnadard cell A. and adds l to counter K.

  Op. 3 transfers the operation code of the selected instruction to the stnadard cell B in which will be obtained the instruction with true address.

  Op. 4 transfers control to op. 11 if an instruction Ma or Mb was selected, since in them ttrue addresses are assigned only to the third address.

  Op. 5 extracts the first address of the instruction and shifts it to the third address of the standard cell C which is reserved foir the current codde y, cahnged by the programme to tje true address Y.

  Op. 6 obtains the true address in cell C>

  Op. 7 sets it in the first address of cell B.

  Op. 8 extracts the second address of the instruction and shifts it to the third address C.

  Op. 9 obtains the true address in cell C.

  Op., 10 sets it in the second address of cell B.

  Op. 11 extracts the third address of the instructio nand sends it to cell C.

  Op. 12 obtains the true address in cell C.

  Op. 13 sets it in the third address of cell B.

  In the description of BESM it was indicated that usually in the instruction "CCCC wit hsecond address", in reference t o a sub-routine, the addresss of the second instruction of RTC is given in the second address. In coding the instruction "CCCC with second addresss" the addresss in RTC is an operator number assigned to the instructions of RTC. In assigning true addresses this operator number will be substituted by the true addrses of the first instruction of RTC since to obtain the correct second address of CCCC it is necessary to increas it by 1'

  Op. 14, in accordance with the above, transers control to op. 15 if an instruction CCCC has been selected in the cell.

  op. 15 determines the case of instruction CCCC in the second address of which is a relative address, transferring control to op. 16.

  Op. 16 increases the second address of the instruction CCCC in the cell B by 1.

  Op. 17 transfers the instruction from cell B back to the block.

  Op. 18, comparing the contents of counter K_f,transfers control to op.2 if trure addresses have not been assigned to all instructions of the block K.

  The second part of the block (operators 19- 25) places inthe initail values of variable instrucvtions taken from block K in the cells of blocvk ɣ reserved for them.

  Op. 19 places K_0 in counter K.

  Op. 20 selects the next instruction from the blocvk K.

  Op. 21 transfers control to op., 22 if an instruction AI has been selected.

  Op. 22 transfers control to op. 23 if in the first addrss of the selected instructions AI is an addresss included in the limits between ɣ_1* and gamm_f*. This signifies that an instruction dispatching the inital value of variable instruction has been selected, having the form
    ┌────┬─────┬─────┬──────┐
    │ AI │ "x" │     │ "y"  │
    └────┴─────┴─────┴──────┘
  where x is the true address of the initial value of the variable instruction, stored int he block ɣ, y is the true address of the variable instruction.


  Op. 23 forms and carries out the instruction

    ┌────┬──────┬─────┬──────┐
    │ ,P │ y-ΔK │     │ y-Δɣ │
    └────┴──────┴─────┴──────┘

  which transfers the initial value of the variabl instruction located in the block K to storage in block ɣ.

  Op. 24 adds 1 to counter K.

  Op. 25, comparing the contents of counter K with K_f, transfers control to op. 20 if all instructions of the programme have not been examined.

  Operators 26 -  42 constitutedx a sub-routine which, for the code y located in the third address of c ell C, calculates the true address Y of the corresponding quantity or instruction obtained in the third address of the same cell C.

  Operators 26- 32 test the magnitude of the code y.

  Op. 26 calls up exit from the sub-routine if 0000 <= y <= 000F (y is the address of a standard cell).

  Op. 27 refers to op. 33 if 0010 <= y <= V_f (y is the code of the quantity having a variable address).

  Op. 28 refers to op. 37 if P_1 <= y <= C_f (y is the code of a parameter or quantity from block C).

  Op. 29 refers to op. 38 if 01A0 <= y <= 01FF (y is the code of a quantity from block gamma).

  Op. 30 refers to op. 40 if 0200 <= y <= 03FF (y is the code of a postiive relative address).

  Op. 31 calls up exit from the sub-routine if 10000 <= y <= 11EF (y is the address of a cell in DS).

  Op. 32 crefers top op. 39 if 11F0 <= y <= 11FF (y is the code of a working cell), and top op. 41 if 1200 <= y <= 13F5 (y is the code a negative relative address).

  Operators 33- 36 obitain the initial value of the variable address Y according to the formula:

    Y = M_1 - 1/2 (sign delta - 1) ( m - 1) + delta

  where m_1 is the address of the first cell of the storage block to which the given variable adddress refers, m is the number of cells in the block, delta is the shift of the variable adddress.

  Op. 33 selects the information on the gien addess according to the magnitude of the code y, obtaining from m_1, m, and delta.

  Op. 34 determines the case of a negative shift, transferring control to

  Op. 35 which forms the quantity m_l + (m - 1) = m_f.

  Op. 36 obtains the initial value of the variable address Y = m_sigma + delta, where m_sigma is equal to m_1 or m_f.

  Op. 37 obtains the true address of the parameter counter for the quantity from block C according to the formula Y = y + Delta C.

  Op. 38 obtains the true address of the quantity from block gamma according to the formula Y = y + Delta gamma.

  Op. 39 obtains the t rue address of the working cell according to the formula Y = y + Delta R.

  Op. 40 obtains the address k' of the instructiosn in the block K according to the formula k' = y - 02200 + (c.K).

  Op. 41 obtains the address of instruction k' in the block K according to the formula k' = (c.K) - (y- 1200).

  Op. 42 obtains the true address of the isntruction according to the formula Y = k' + Delta K.

-}


{-
  The block IV-PP-3 prints the information on the storage distribution, prints if necessary, the assembled programme and constants, transforms the constants, if necessary t the binary systema nd writes the assembled programme and cvonstants on MD-1.

  Information on storage distribution gives data on the distribution in IS of all seven blocks (S 33): block P, block C, block K, block gamma, block O, block M and block R. The printed inforamtion on each of these blocks M (i 1..7) has the form of an instructon with zero operation code, in the addresses of which are the following quantities:

    ┌────┬────────┬────────┬────────┐
    │    │ M_1^i* │ Mbar^i │ M_f^i* │
    └────┴────────┴────────┴────────┘

  ie the address of the first cell of the block, the extend of the block and the address of the last cell of the block. In addition, a conditional number is printed for each block, an index according to which information on the given block is conveniently located on the printed tape.
  For the i-th block thsi number is printed in the form

      + iiiiiiii,i+

  Information on the storage distribution processed by block II-PP-3 , is distributed ain the storage cells in successsion (one number in each cell), in the following order: Pbar, Cbar, Kbar, gamma bar, obar, Mbar, Rbar.

  Pbar_0*=0, C_0*,K_0*,gmma_0*,O_0*,M_0*,R_0*,R_f* permitting the unambiguous formation of lines of infroamtion on all blocks.

  The indices determining if it is necessary to print the programme and constants, and if it is necessary to transform constants to the binary system are given by the contents of cell DS 1100, led out to the control console of BESM.
  Unity in the third place deontoes that the programme need not be printed, unity in the second place denotes that the constants need not be printed , unity in the first place denotes that the constants need not be transformed to the binary system.
  Zeros in the corresponding places denote the contrary.

  After printing the programme and constants (if they were required to be printed), two vcheck sums are printed (the contents of the programme with all constants relating to it are summed before and after writing on MD-1). Agreement of the check sums ensures correct recording of the constructed programme on MD-1.

  The block IV-PP-3 consists of the following operators:

  Op. 1 carries out the prepatory instructions.

  Op. 2 prints a number, the index of hte i-th block and formas a line of information on this block.

  Op. 3 prints the line of information.

  Op. 4 realizes address-modification in op. 2

  Op. 5 repreats the functioning of operators 2-4 seven times.

  Op. 6 carries out the prepatory instructions connected with summing and prining the instructions.

  Op. 7 selects the next instruction from block K and adds it to the check sum.

  Op.  8 verifies if it is necessary to print the instruction (YES  -- op. 9, NO -- op. 10).

  Op. 9 prints the instruction.

  Op. 10 pcarries out address-modification of selection instructions in op. 7.

  Op. 11 repeats operators 7-10 for all instructions of the programme.

  Op. 12 carries out prepatory instructions connected with summation and printing the constnats from block gamma.

  Op. 13 selects from block gamma the next constant and adds it to the check sum.

  Op. 14 verifies if it is necessar yto print the constants from the block (YEs -- op. 15, No -- op. 16).

  Op. 15 prints the constant sfrom block gamma.

  Op. 16 carries out modification of the selection instruction addresses in op. 13.

  Op. 17 repeats the function of operators 13-16 for all constants of block gamma.

  Op. 18 writes blocks K and gamma on MD-1 in cells K_1* -K_f* and gamma _1* - gamma_f * respectively and reads block C from MD-2 into IS.

  Op. 19 transfers the constant from block C to the stnadard cell.

  Op. 20 verifies if itis necessary to tansform the constant to the binary system (YES -- op. 21, No -- op. 22).

  Op.  21 transforms the constant to the binary system..

  Op. 22 transforms a binary constant to the decimal system, preparing it at the same time for printing.

  Op. 23 veirfies it if  is necessary to print the constant (YES -- op. 24, NO -- op. 25).

  Op. 24 prints the constant in the decimal system.

  Op. 25 adds teh binary constant to the check sum and sends it to block C.

  Op. 26 carries out modification of addresses in op. 25 -and op .19 .

  Op. 27 repeats functioning of operators 19-26 for all constants of the block C.

  Op. 28 writes blcok C on Md-1 in cells C_1* - C_f* and prints the first check sum.

  Op. 29 reads teh contents of the programme with all of its constant from MD-1.

  Op. 30 and op. 31 repeat summation of the programme and its constants.

  Op. 32 prints the seocnd check sum.
-}
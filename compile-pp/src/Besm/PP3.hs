{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}

module Besm.PP3 where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

header = Unknown "programme header table" `offAddr` 6 -- This should be cell 7

counterKlast = header `offAddr` 5

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
  pinned "prog" "programme" (Size 750)
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

-- Probably a shared counter?
pp3_1 = do
  pinned "header" "programme header table" (Size 15)
  -- Should be shared with PP-2 (pinned)
  counterK <- global "k" Cell
  counterK' <- local "trueAddresses" Cell -- might be a global
  cellA <- local "A" Cell
  cellC <- local "C" Cell

  currentInstr <- local "currentInstr" Cell
  local "0x18" (Raw 0x18)

  counterK' <- local "counterK'" Cell
  fetchK <- local "fetchK" Cell
  let finalK = counterKlast

  maTemplate <- global "maTemplate" (Template (Ma zero zero zero))
  mbTemplate <- global "mbTemplate" (Template (Mb zero))

  -- Probably need to make cellA global for this to work
  transTemplate <- local "trans_template" (Template $ TN cellA (Absolute 1) UnNormalized)
  selTemplate <- local "sel_template" (Template $ TN (Absolute 1) currentInstr UnNormalized)
  local "all-but-third" (Raw $ 0b1_11_1111_1111 `B.shift` 22 B..|. 0b1_11_1111_1111 `B.shift` 11)
  local "all-but-first" (Raw $ 0b1_11_1111_1111 `B.shift` 11 B..|. 0b1_11_1111_1111)
  local "all-but-second" (Raw $ 0b1_11_1111_1111 `B.shift` 22 B..|. 0b1_11_1111_1111)

  local "transferTemplate" (Template $ TN zero (var "fetchK") UnNormalized)

  local "B" Cell

  local "0200"   (Raw 0x0200)
  local "1200"   (Raw 0x1200)
  local "1000"   (Raw 0x1000)


  let k0 = header `offAddr` 4
  {-
  Op. 1 sends K0 to counter K, in which is fixed the address of the last instruction selected from block K.
  -}
  operator 1 $ do
    shift k0 (left 22) counterK
    ai (Unknown "sel_template") counterK (op 46)  -- Form selection command
    ai (Unknown "trans_template") finalK (op 47)  -- Form transmission command
    tN' k0 counterK
    chain (op 2)

  {-

  Op. 2 selects the next instruction in block K and adds unity to counter K.
  -}
  operator 2 $ do
    clcc (op 46)
    ai counterK one counterK
    chain (op 3)
  {-
  Op. 3 determines the case where open-parentheses of the loop or the sign of
  an operator number have been selected, transferring control to op. 12.

  test the opcode against the value 0x018 (as both logical operators and loops start with that)
  -}
  operator 3 $ do
    comp currentInstr (var "0x18") (op 12) (op 4)
  {-

  Op. 4 in the case of selection of instructions Ma or Mb transfers control
  immediately to op. 10 since in theses instructions the first two addresses
  are not examined.
  -}
  operator 4 $ do
    comp currentInstr mbTemplate (op 4) (op 12)
    comp currentInstr maTemplate (op 12) (op 4)

  {-
  Op. 5 determines the first address of the instruction and shifts it to the
  third address of the standard cell A.
  -}
  operator 5 $ do
    bitAnd currentInstr firstAddr cellA
    shift cellA (right 22) cellA -- original code uses a shiftall? but that seems unnecessary / wrong?
    chain (op 6)
  {-

  Op. 6 examines the extracted address.
  ----
  what??
  -}
  operator 6 $ do
    clcc (op 34)
    chain (op 7)

  {-

  Op. 7 sets the tested first address in the instruction and extracts the second address from the latter, shifting it to the third address of cell A.

  -}
  operator 7 $ do
    shift cellA (left 22) cellA
    bitAnd currentInstr (var "all-but-first") currentInstr
    ai currentInstr cellA currentInstr
    bitAnd currentInstr secondAddr cellA
    shift cellA (left 11) cellA
    chain (op 8)
  {-

  Op. 8 tests the selected address.

  -}
  operator 8 $ do
    clcc (op 34)
    chain (op 9)

  {-

  Op. 9 sets the selected second address in the instruction.

  -}
  operator 9 $ do 
    shift cellA (left 11) cellA
    bitAnd currentInstr (var "all-but-second") currentInstr
    ai currentInstr cellA currentInstr
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
  operator 11 $ do
    clcc (op 34)
    bitAnd currentInstr (var "all-but-third") currentInstr
    ai currentInstr cellA currentInstr
    chain (op 9)
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

    comp (counterK) (counterKlast) (op 2) (op 14)
  {-

  Operators 14-25 realize change in relative address, connected with elimination from the programmme of open-parentheses and signs of operator numbers. For this a special sub-routine  (operators 39-45) examines the addresses of instructions and for each relative address Δ calculates the number n of the open-parentheses of loops and signs of operator numbers located in the programme between the instruction having the given relative address Δ and the instruction in which this address is encountered. The number n obtained is the n subtracted from the absolute magnitude of Δ.

  -}

  {-

  Op. 14 sends K0 to counter K.

  -}
  operator 14 $ do
    tN' k0 counterK 
    chain (op 15)
  {-

  Op. 15 selects the next instruction from the block K and adds unity to counter K.

  -}
  operator 15 $ do 
    clcc (op 46)
    ai counterK one counterK
    chain (op 16)
  {-

  Op. 16, in the case of selection of instructions Ma and Mb transfers control to op. 22 since in these instructions the relative addresses may be located only in the third address.

  -}
  {-

  Op. 17 extracts the first address of the instruction and sends it to the third address of the standard cell A.

  -}
  operator 17 $ do
    bitAnd currentInstr firstAddr cellA
    shift cellA (right 22) cellA
    chain (op 18) 
  {-

  Op. 18 examines the extracted address.

  -}
  operator 18 $ do
    clcc (op 39)
    chain (op 19)
  {-

  Op. 19 sets the tested address in the instruction, extracts the second address and sends it to the third address of cell A.

  -}
  operator 19 $ do
    shift cellA (left 22) cellA
    bitAnd currentInstr (var "all-but-first") currentInstr
    ai currentInstr cellA currentInstr
    bitAnd currentInstr secondAddr cellA
    shift cellA (left 11) cellA
    chain (op 8)
  {-

  Op. 20 tests the extracted address.

  -}
  operator 20 $ do
    clcc (op 39)
    chain (op 21)
  {-

  Op. 21 sends the tested address to the instruction.

  -}

  operator 21 $ do
    shift cellA (left 11) cellA
    bitAnd currentInstr (var "all-but-first") currentInstr
    ai currentInstr cellA currentInstr
    chain (op 22)
  {-

  Op. 22 extracts the third address of the instruction, sending it to cell A.

  -}
  operator 10 $ do
    bitAnd currentInstr thirdAddr cellA
    chain (op 11)
  {-

  Op. 23 tests the extracted address.

  -}
  operator 20 $ do
    clcc (op 39)
    chain (op 24)
  {-

  Op. 24 sends the tested address to the instruction and transfers the instruction back to block K.

  -}
  operator 24 $ do
    bitAnd currentInstr (var "all-but-third") currentInstr
    ai currentInstr cellA currentInstr
    clcc (op 47)
    chain (op 25)
  {-

  Op. 25 transfers control to op. 15 if all instructions in block K have not been examined.

  -}
  operator 25 $ do
    comp counterK counterKlast (op 15) (op 26)
  {-

  Operators 26 - 33 remove open-parentheses of loops and the signs of operator numbers from the assembled programme. Printing of information on the constructed programme takes place this time. The open-parentheses of loops are printed and the signs of operator numbers, in the third addresses of which is first place d the true address of the first instruction of the loop (for open-parentheses) or the true address of the first instruction of the operator (for signs of operator numbers).

  -}
  {-

  Op. 26 calculates the true initial K_1* of the programme and sets it in a special counter in which, at the moment of selection from the programme of the open-parentheses of a loop or the sign of an operator number, will be the true address of the following instruction.

  -}
  operator 26 $ do
    ai k0 one (var "trueAddresses") -- "K_1*" is somewhere in the header
    tN' k0 counterK
    chain (op 27)
  {-

  Op. 27 selects the next instruction of the programme from block K.

  -}
  operator 27 $ do 
    clcc (op 46)
    ai counterK unity counterK -- inferred
    chain (op 28)

  {-

  Op. 28 transfers control to op. 29 if an instruction has been selected to op. 30 if the open-parentheses of a loop or the sign of an operator number has been selected.

  -}
  operator 28 $ do 
    comp currentInstr (var "0x18") (op 30) (op 29) 
  {-

  Op. 29 sends the instruction back to the programme and adds unity to the special counter of true instruction addresses.

  -}
  operator 29 $ do
    clcc (op 47)
    ai (var "trueAddresses") unity (var "trueAddresses")
    chain (op 32)
  {-

  Op. 30 sets in the third address of the open-parentheses of the loop or the sign of the operator number the true addresses of the following instruction.

  -}
  operator 30 $ do 
    bitAnd currentInstr (var "all-but-third") currentInstr
    ai currentInstr (var "trueAddresses") currentInstr
    chain (op 31)
  {-

  Op. 31 prints the open-parentheses of the loop or the operator number sign.

  -}
  operator 31 $ do
    -- todo
    chain (op 32)
  {-

  Op. 32 transfers control to op. 28  if all instructions of the programme have not been examined.

  -}
  operator 32 $ do
    comp counterK counterKlast (op 28) (op 33) -- inferred
  {-

  Op. 33 calculates and sets the cell 000C the new value of K_f, changed through elimination of the open-parentheses of loops and signs of operator numbers.

  -}
  operator 33 $ do
    sub' (var "trueAddresses") one counterKlast
    cccc (Procedure "MP-3" (op 2)) 

  {-

  Op. 34-48 constitute a sub-routine for the formulation of relative addresses.

  -}
  {-

  Op. 34 transfers control to op. 35 if the number of an operator is in standard cell a.

  -}
  operator 34 $ mdo
    comp cellA (var "1200") exit (op 35)
    exit <- jcc
    pure ()
  {-

  Op.  35 shifts the numbers of the operators to the first address and sets K_1 in the special counter K'.

  -}
  operator 35 $ do
    shift' cellA (left 22) cellB
    empty -- ???
    shift counterK (left 22) counterK'
    ai (var "transferTemplate") counterK (op 36) -- todo: unclear if it should be counter k
    tN' k0 counterK'
    chain (op 36)
  {-

  Op. 36 elects the next instruction from block K. beginning with the first instruction and adds unity to counter K'.

  -}
  operator 36 $ mdo
    transfer <- empty
    ai transfer oneFirstAddr transfer
    ai counterK' one counterK'
    chain (op 37)
  {-

  Op. 37 repeats op. 36 until the sign of the given operator number is selected.

  -}
  operator 37 $ mdo
    comp fetchK cellB (op 36) next
    next <- comp cellB fetchK (op 36) (op 38)
    pure ()

  {-

  Op. 38 obtains the code of the relative address equal to 0200 + |(C.K') - (C.K)| for the positive relative address and 1200 + |(c.K') - (c.K)| for negative.

  -}
  operator 38 $ mdo
    cell_0001 <- local "cell_0001" Cell -- todo: figure out which cell i can safely use here
    sub' counterK' counterK cell_0001
    -- shift cell_0001 (left 22) cell_0001

    comp zero cell_0001 pos neg

    pos <- block $ do
      ai cell_0001 (var "0200") cell_0001
      jcc
    neg <- tMod' cell_0001 cell_0001
    ai cell_0001 (var "1200") cell_0001
    jcc 
  {-

  Operators 39 - 45 constitute the sub-routine changing the relative addresses.

  -}
  {-

  Op. 39 transfers control to op. 40 if a relative address is in standard cell A.


  todo: add loop making code
  -}
  operator 39 $ mdo
    comp cellA (var "0200") exit cont
    cont <- comp cellA (var "1000") (op 40) cont2
    cont2 <- comp (var "1200") cellA (op 40) exit
    exit <- jcc 
    pure ()
  {-

  Op. 40 clears counter K' and standard cell B.

  -}
  operator 40 $ do 
    -- tN' zero counterK'
    tN' zero cellB
    tN' (var "transferTemplate") (op 41)
    -- rather than performing a comparison between K' and delta we just decrement delta and compare against zero. 
    -- this is what appears to be done in op 27 of the source.
    tMod' cellA counterK' 

    chain (op 41)
  {-

  Op. 41 selects the next instruction from the part of the programme located between the instruction with the given relative address and the instruction containing this relative address and adds unity to counter K'.

  -}
  operator 41 $ do
    empty
    sub' counterK' one counterK'
    tExp' fetchK cellC
    chain (op 42)
  {-

  Op. 42 determines the case of selection of an open-parenthesis of a loop or the sign of an operator number, transferring control to op. 43

  -}
  operator 42 $ do
    comp (var "0x18") cellC (op 42) (op 44) 
  {-

  Op. 43 which adds 1 to cell B.

  -}
  operator 43 $ do
    ai cellB one cellB
    ai (op 41) oneFirstAddr (op 41)
    chain (op 44)
  {-

  Op. 44, comparing the indications of counter K' with the absolute magnitude Δ of the relative address, repeats the functioning of operators 41-43 Δ times.

  -}
  operator 44 $ do
    comp zero counterK' (op 41) (op 45)
  {-

  Op. 45 forms the new value of the absolute magnitude of the relative address, equal to Δ - (B).

  -}
  operator 45 $ do
    -- do we need to do anything here?
    -- maybe the delta logic used in op 40 i actually incorrect?
    tN' cellB cellA
    jcc
  {-

  Op. 46 is the sub-routine for selecting instructions from block K.
  
  todo: make self-incrementing?
  -}
  operator 46 $ do
    empty
    jcc
  {-

  Op. 47 is the sub-routine for transferring instructions to block K.

    todo: make self-incrementing?

  -}
  operator 47 $ do
    empty
    jcc

{-

  Storage Distribution Block

  In accordance with the diagram on page 70 the final distribution of the store appears as follows: parameter counters (block ), constants and variable quantities (block C), programme (block K), constants for the programme (block \ɣ), block O, M and, finally the working cells (block R). For each of these seven blocks M^i the storage distribution block (II-PP-3) forms the following quantities: M_O^i*, the address of the cell directly before the block, Mbar^i, the extent of the block and M_f*^i, the address of the last cell of the block (we note that M_f*^i = M_o^(i + 1). Then the corrections to the codes of quantities are calculated: Δ C, the correction for obtaining the true addresses of constants and variable quantities from the block C and the parameter counters, Δ K, the correction for obtaining the true addresses of programme instructions Δɣ, the correction for obtaining the true addresses of constants from block ɣ and Δ R, the correction for obtaining the true addresses of working cells.

  In addition, the block II-PP-3 carries out transformation of information on variable addresses in the block V and prints information on the variable addresses in the block V and the positions of the blocks M^i in the store. The third address of the first cell of M^i is placed in each "main head" with information on storage blocks M^i (i = 1..l); the "main head" itself with the eliminated operation code are printed in the form of an instruction. In each line of information about a variable address relating to the storage block M^i (i = 1..l), the magnitude of the block M^i is set in the first address while in the second address , the address of its start M*_l^i.
  These data will be utilized in forming the initial values of variable addresses during the period of functioning of block III-PP-3.
  the scheme of block II-PP-3 is represented in Fig. 17.
-}
pp3_2 = do
  pinned "header" "programme header table" (Size 15)
  pinned "prog" "programme" (Size 750)

  -- Working cells and variables
  cell_03F0 <- local "03F0" Cell  -- Standard working cell
  cell_0001 <- local "0001" Cell  -- Standard working cell
  -- cell_0004 <- local "0004" Cell  -- M_1^(i+1)* counter

  -- Constants
  pBar <- global "Pbar" Cell      -- 03E1
  cBar <- local "Cbar" Cell       -- 03E2
  kBar <- local "Kbar" Cell       -- 03E3
  gammaBar <- local "gammaBar" Cell  -- 03E4
  p0 <- local "P0" Cell          -- 0008
  c0 <- local "C0" Cell          -- 03EA (also Ko)
  k0 <- local "K0" Cell          -- 03EB (also T0)
  -- hrrmmm 
  gamma0 <- local "gamma0" Cell  -- 03EC (also F0)
  o0 <- local "O0" Cell          -- 03ED (also M0)
  m1 <- local "M1" Cell          -- M^(1)
  o  <- local "O" Cell           -- 03E5

  deltaC <- local "deltaC" Cell  -- 03F7
  deltaGamma <- global "deltaGamma" Cell  -- 03F9
  deltaK <- global "deltaK" Cell  -- 03F8
  deltaRPD <- local "deltaRPD" Cell  -- 03FA

  fetchCounter <- local "fetchCounter" Cell  -- 03FD
  cellA <- local "cellA" Cell    -- 03FB
  cellB <- local "cellB" Cell    -- 03FE

  -- Constants from transcription
  -- local "0x18" (Raw 0x18)


  local "const_0337" (Raw 0x0337)  -- Value 31 for comparison (2^5,F8)
  local "const_0338" (Raw 0x0338)  -- Value 03F0 for comparison
  local "const_033A" (Raw 0x033A)  -- Forward address shift info mask (-2^0,000000FF)??
  local "const_033C" (Raw 0x033C)  -- Old RPD end (2^1,11111111)???
  local "const_033B" (Raw 0x34F)  

  local "const_0339" Cell
  local "const_03EE" Cell -- check where this shows up in III
  local "const_03E6" Cell -- as far as i can tell this is *never* used again (unless its copied as part of an Ma / Mb???)
  local "const_03EF" Cell 
  local "const_03E7" Cell -- as far as i can tell this is never used again... highly sus 

  -- 0334: ,TN 0010 03F0
  fetchTemplate <- local "fetchTemplate" (Template $ TN (var "programme") cell_03F0 UnNormalized)
  -- 0334: ,TN 0010 03F0
  returnTemplate <- local "returnTemplate" (Template (TN cell_03F0 (var "programme") UnNormalized))
  counterTemplate <- local "counterTemplate" (Raw 0xF)


  global "printEntry_039c" Cell -- actually a program
  global "printSubroutine_03a4" Cell -- actually a program

  {-
    Op 1. calculates Pbar, Cbar, Kbar, ɣbar Obar and C_0*, K_0*, ɣ_0*, O_0*, M_0*
  -}
  operator 1 $ do
    -- check these offsets!!!
    -- /Pₓ +1 /  - P₀  =P+1
    sub' (header `offAddr` 9) (header `offAddr` 8) (var "0001")
    -- P
    sub' (var "0001") one pBar
    -- Cₓ + P₀ =C+P=Cₘ=Ko
    sub' (header `offAddr` 10) (header `offAddr` 8) c0
    -- (C+P) -P = C
    sub' c0 pBar cBar
    -- K = Kₓ - K₀
    sub' (header `offAddr` 12) (header `offAddr` 11) kBar
    -- γbar
    sub' (header `offAddr` 6) (header `offAddr` 13) gammaBar
    -- 0 constant
    tN' (header `offAddr` 7) o
    -- (P+C) + K = Kₓ =T₀
    ai c0 kBar k0
    -- (P+C + K) + F = Fₓ = 0₀
    ai k0 gammaBar gamma0
    -- (P+C+K+F) +0 = 0ₓ = M₀
    ai gamma0 o o0
    -- M₀+1 = M₁ = M⁽¹⁾
    ai o0 one m1
    chain (op 2)

  {-
    Op. 2 calculates the correction Δ C, forms the instructions for processing the block V
    and prints a number of special form, which denotes that immediately after it on the tape
    will be printed information on the strange blocks.
  -}
  operator 2 $ do
    -- ΔС = -Пₖ
    tMod' p0 deltaC
    -- ΔС with order 31
    addE' deltaC (op 9) (var "const_0339")
    -- Loading fetch command from array П
    tN' fetchTemplate (op 3)
    -- Loading return command to array П
    tN' returnTemplate (op 15)
    -- Fetch counter from array П to initial position /П₀/
    tN' counterTemplate fetchCounter
    -- Print information flags about memory arrays
    pN' (var "const_033C")
    pN' (var "const_033C")
    chain (op 3)

  {-6
    Op. 3 selects the next line of information of block V.
  -}
  operator 3 $ do
    -- Fetch from array П to 03F0 (template filled in)
    empty
    -- +1 to fetch counter
    ai fetchCounter one fetchCounter
    -- Extraction of order x
    tExp cell_03F0 cell_0001
    chain (op 4)

  {-
    Op. 4 tests the selected line, transferring control to op. 5 if a "main head"
    has been selected for the next block M^i (i = 1.. l).
  -}
  operator 4 $ do
    -- 31 ≥ℐ? NO - op 14
    comp cell_0001 (var "const_0337") (op 14) (op 5)

  {-
    Op. 5 extracts the third address of the "main head".
  -}
  operator 5 $ do
    -- Extraction of Az
    bitAnd cell_03F0 thirdAddr cell_0001
    chain (op 6)

  {-
    Op. 6 compares the extracted address with zero. If it is not equal to zero this denotes
    that the corresponding block M^i relates to the group of constants from block C.
    In this case op. 10 functions.
  -}
  operator 6 $ do
    -- Start assigned to array? YES - op 10
    comp zero cell_0001 (op 10) (op 7)

  {-
    Op. 7 sets M_l^i* in the third address of the "main head" and calculates M_l^(i+1)* = M_l^i* +Mbar^i.
  -}
  operator 7 $ do
    -- Next М₁⁽ⁱ⁾ in Az /start assigned to array М⁽ⁱ⁾
    ai cell_03F0 m1 cell_03F0
    -- Extraction of А₂ /size of array М⁽ⁱ⁾/
    bitAnd cell_03F0 secondAddr cell_0001
    -- А₂ to Ae
    shift cell_0001 (right 11) cell_0001
    -- М₁⁽ⁱ⁺¹⁾ = М₁⁽ⁱ⁾ + М⁽ⁱ⁾
    ai m1 cell_0001 m1
    chain (op 8)

  {-
    Op. 8 compares M_l^(i+1)* with 03FFF.
  -}
  operator 8 $ do
    -- N₁⁽ⁱ⁺²⁾ ≥ 0350 ? NO - op 11
    comp m1 (var "const_0338") (op 11) (op 9)

  {-
    Op. 9 is a check stop for M_l^(i+1)* 03FFF.
  -}
  operator 9 $ do
    -- Control stop
    stop

  {-
    Op. 10 obtains the true address M_l^i*, adding Δ C to the third address of the "main head".
  -}
  operator 10 $ do
    -- Decrease array start from constants by ΔС
    add' cell_03F0 deltaC cell_03F0
    chain (op 11)

  {-
    Op. 11 eliminates the operation code in the "main head".
  -}
  operator 11 $ do
    -- Separation of operation code
    bitAnd cell_03F0 negAddrModifConstant cell_0001
    chain (op 12)

  {-
    Op. 12 prints information on the current block M^i.
  -}
  operator 12 $ do
    -- Print information about array :М⁽ⁱ⁾
    -- part of mp-3
    callRtc (var "printSubroutine_03a4") (var "printEntry_039c")
    empty
    chain (op 13)

  {-
    Op. 13 sets Mbar^i in the first address and M_l^i* in the second address of a certain
    working cell for arranging these quantities in the line of information on the variable
    addresses relating to the block M^i.
  -}
  operator 13 $ do
    -- Shift header to А₁ and А₂ for placement in forward address info
    shift cell_0001 (left 11) cellA
    cccc (op 15)
    empty -- ???
    chain (op 14)

  {-
    Op. 14 sets Mbar^i in the first address of this line and in the second address M_l^i*,
    leaving the magnitude of the shift delta of the variable address in the third address,
    and the sign of the shift in the eleventh place of the first address.
  -}
  operator 14 $ do
    -- Extraction of information about forward address shift
    bitAnd cell_03F0 (var "const_033A") cellB
    -- The size and start of the array is loaded into the forward address info
    ai cellB cellA cellB
    chain (op 15)

  {-
    Op. 15 transfers the processed line of information back to block V.
  -}
  operator 15 $ do
    empty
    chain (op 16)

  {-
    Op. 16 carries out modification of the selection instruction address and the arrangements
    in op. 3 and op. 15.
  -}
  operator 16 $ do
    -- Counter of fetch command from array Г
    ai (op 3) two (op 3)
    -- Counter of return command to array П
    ai (op 15) one (op 15)
    chain (op 17)

  {-
    Op. 17 repeats the functioning of operators 3-16 for all lines of the block V.
  -}
  operator 17 $ do
    -- Fetch counter < Пk ? YES - op 3
    comp fetchCounter p0 (op 3) (op 18)

  {-
    Op. 18 forms Mbar, R_O*, Rbar and R_f*.
  -}
  operator 18 $ do
    -- (P/d)_к = Mк
    sub' m1 one (var "const_03EE")
    -- М = M_к - М_0
    sub' (var "const_03EE") o0 (var "const_03E6")
    -- (P/d)_к = Mк + P/d
    -- not sure that the header value thing is correct
    -- not sure where 0x3EE is from and whetehr there's a well known value associated
    sub' (var "const_03EE") (header `offAddr` 5) (var "const_03EF")
    chain (op 19)

  {-
    Op. 19 compares R_f* with 03FFF
  -}
  operator 19 $ do
    -- (P/d)к ≥ 03F0 NO - op 21
    comp (var "const_03EF") (var "const_0338") (op 21) (op 20)

  {-
    Op. 20 is a check stop for R_f* > 03FF.
  -}
  operator 20 $ do
    -- Control stop
    stop

  {-
    Op. 21 calculates the corrections Δ ɣ, Δ R, Δ K.
  -}
  operator 21 $ do
    -- ΔГ = Г₀ - ɣ₀
    -- need to correct the header offsets
    sub' k0 (header `offAddr` 13) deltaGamma
    -- Δ(P/d) = (P/ya)₀ - (P/ya)ₑ old
    sub' (var "const_03EE") (var "const_033B") deltaRPD
    -- ΔК = Кg - Кo
    sub' c0 (header `offAddr` 11) deltaK
    -- inlined end to op 21
    -- header values are off!
    tN' (header `offAddr` 5) (var "const_03E7")
    -- Exit from program to MP-3
    cccc (Procedure "MP-3" (op 3))


{-
  Block for Assigning True Addresses

  The block III-PP-3 substitutes their true addresses for the codes of quantities and the relative address of variable instructions in the instructions of the assembled programme. Determination of the types of quantities according to the values of their codes is cvarried out in accordance with Table 17 (S 19).

  The first part of the block (operators 1 -18) carries out assignment of true addresses. The direct substitution of codes of quantities or relative addresses by true address of quantities or instructions is carried out by a sub-routeine (operators 26 -42).

  This one is quite different from the machine code. unclear why.
-}

pp3_3 = do
  pinned "header" "programme header table" (Size 15)

  let k0 = header `offAddr` 4

  relocRes <- local "relocRes" Cell -- 03F2
  deltaK <- global "deltaK" Cell  -- 03F8
  deltaGamma <- global "deltaGamma" Cell  

  gammaFinal <- local "gammaFinal" (Raw 0x01FF) -- probs should be 0x200
  let gamma0 = header `offAddr` 7

  relocationTemplate <- local "relocationTemplate" (Template (TN zero relocRes UnNormalized))

  local "0x11EF" (Raw 0x11EF)
  local "0x11FF" (Raw 0x11FF)
  local "0200"   (Raw 0x0200)
  local "1200"   (Raw 0x1200)
  local "0x1B"   (Raw 0x1B)
  local "0x12"   (Raw 0x12)
  local "0xF"    (Raw 0xF)
  local "0x3ff"  (Raw 0x3ff)

  local ",P template" (Template $ TN zero zero UnNormalized)

  local "22" (Raw 22)
  local "23" (Raw 23)

  local "A" Cell
  local "B" Cell

  cellZ <- local "cellZ" Cell
  cellX <- local "cellX" Cell
  cellC <- local "cellC" Cell

  counterK <- local "counterK" Cell
  k' <- local "k'" Cell

  deltaR <- global "deltaR" Cell
  deltaC <- global "deltaC" Cell

  {-
  Op. 1 sets K_0 in counter K, in which will be stored the addres of the current instruction selected from the programme during functioning of the block.
  -}
  operator 1 $ do
    tN' k0 counterK
    -- todo: init op 2
    -- todo: init "send to program op"
    chain (op 2)

  {-
  Op. 2 transfers the next instruction from the block K to the standard cell A. and adds 1 to counter K.
  -}
  operator 2 $ do
    empty
    ai' counterK one counterK
    chain (op 3)

  {-
  Op. 3 transfers the operation code of the selected instruction to the standard cell B in which will be obtained the instruction with true address.
  -}
  operator 3 $ do
    tExp' cellA cellB
    chain (op 4)

  {-
  Op. 4 transfers control to op. 11 if an instruction Ma or Mb was selected, since in them true addresses are assigned only to the third address.
  (op 51)???
  -}
  operator 4 $ mdo
    comp cellB (var "22") (op 5) next
    next <- comp cellB (var "23") (op 11) (op 5)
    pure ()

  {-
  Op. 5 extracts the first address of the instruction and shifts it to the third address of the standard cell C which is reserved for the current code y, changed by the programme to the true address Y. 

  Machine code op 58
  -}
  operator 5 $ do
    bitAnd cellA firstAddr cellC
    shift'  cellC (right 22) cellC
    -- todo: empty?
    chain (op 6)

  {-
  Op. 6 obtains the true address in cell C
  -}
  operator 6 $ do
    clcc (op 26)
    chain (op 7)

  {-
  Op. 7 sets it in the first address of cell B.
  -}
  operator 7 $ do
    shift cellC (left 22) cellC
    ai cellB cellC cellB
    chain (op 8)

  {-
  Op. 8 extracts the second address of the instruction and shifts it to the third address C.
  -}
  operator 8 $ do
    bitAnd cellA secondAddr cellC
    shift cellC (right 11) cellC
    chain (op 9)

  {-
  Op. 9 obtains the true address in cell C.
  -}
  operator 9 $ do
    clcc (op 26)
    chain (op 10)
    

  {-
  Op. 10 sets it in the second address of cell B.
  -}
  operator 10 $ do
    shift cellC (left 11) cellC
    ai cellB cellC cellB
    chain (op 11)

  {-
  Op. 11 extracts the third address of the instruction and sends it to cell C.
  -}
  operator 11 $ do
    bitAnd cellA thirdAddr cellC
    chain (op 12)

  {-
  Op. 12 obtains the true address in cell C.
  -}
  operator 12 $ do
    clcc (op 26)
    chain (op 13)

  {-
  Op. 13 sets it in the third address of cell B.
  -}
  operator 13 $ do
    ai cellB cellC cellB
    chain (op 14)

  {-
  In the description of BESM it was indicated that usually in the instruction "CCCC with second address", in reference to a sub-routine, the addresss of the second instruction of RTC is given in the second address. In coding the instruction "CCCC with second addresss" the addresss in RTC is an operator number assigned to the instructions of RTC. In assigning true addresses this operator number will be substituted by the true addrses of the first instruction of RTC since to obtain the correct second address of CCCC it is necessary to increas it by 1'

  Op. 14, in accordance with the above, transers control to op. 15 if an instruction CCCC has been selected in the cell.
  -}
  operator 14 $ do
    compWord cellB (var "0x1B") (op 17) (op 15)

  {-
  op. 15 determines the case of instruction CCCC in the second address of which is a relative address, transferring control to op. 16.

  Machine code op 21
  -}
  operator 15 $ do
    bitAnd cellA secondAddr cellC
    compWord cellC zero (op 17) (op 16) -- need a neq?

  {-
  Op. 16 increases the second address of the instruction CCCC in the cell B by 1.

  op 26 in machien code
  -}
  operator 16 $ do
    ai cellB oneSndAddr cellB
    chain (op 17)

  {-
  Op. 17 transfers the instruction from cell B back to the block.
  -}
  operator 17 $ do
    empty 
    chain (op 18)

  {-
  Op. 18, comparing the contents of counter K_f, transfers control to op. 2 if true addresses have not been assigned to all instructions of the block K.
    op 32.
  -}
  operator 18 $ do
     comp counterK counterKlast (op 2) (op 19)


  {-
  The second part of the block (operators 19- 25) places the initial values of variable instructions taken from block K in the cells of block ɣ reserved for them.

  Op. 19 places K_0 in counter K.
  -}
  operator 19 $ do
    tN' k0 counterK
    -- todo: init some empty cells (op 20) and wherever the return op is
    chain (op 20)

  {-
  Op. 20 selects the next instruction from the block K.
  -}
  operator 20 $ do
    empty 
    chain (op 21)

  {-
  Op. 21 transfers control to op., 22 if an instruction AI has been selected.
  -}
  operator 21 $ do
    compWord cellA (var "0x12") (op 24) (op 22)

  {-
  Op. 22 transfers control to op. 23 if in the first address of the selected instructions AI is an addresss included in the limits between ɣ_1* and ɣ_f*. This signifies that an instruction dispatching the inital value of variable instruction has been selected, having the form
    ┌────┬─────┬─────┬──────┐
    │ AI │ "x" │     │ "y"  │
    └────┴─────┴─────┴──────┘
  where x is the true address of the initial value of the variable instruction, stored in the block ɣ, y is the true address of the variable instruction.
  -}
  operator 22 $ mdo
    bitAnd cellA firstAddr cellC 
    shift cellC (right 22) cellC
    comp cellC gamma0 (op 24) next
    next <- comp gammaFinal cellC (op 24) (op 23)
    pure ()

  {-
  Op. 23 forms and carries out the instruction
    -- todo: probably a typo should be tn
    ┌────┬──────┬─────┬──────┐
    │ ,P │ y-ΔK │     │ x-Δɣ │
    └────┴──────┴─────┴──────┘

  which transfers the initial value of the variable instruction located in the block K to storage in block ɣ.
  -}
  operator 23 $ mdo
    sub' cellC deltaK cellC
    ai (var ",P template") cellC transInit
    bitAnd cellA thirdAddr cellC 
    sub' cellC deltaGamma cellC 
    shift cellC (left 22) cellC
    ai transInit cellC transInit 
    transInit <- empty
    chain (op 24)

  {-
  Op. 24 adds 1 to counter K.
  -}
  operator 24 $ do
    ai counterK one counterK
    -- todo: increment empty cells

    chain (op 25)

  {-
  Op. 25, comparing the contents of counter K with K_f, transfers control to op. 20 if all instructions of the programme have not been examined.
  -}
  operator 25 $ mdo
    comp counterK counterKlast (op 20) ret
    ret <- cccc (Procedure "MP-3" (op 4))  
    pure ()
  {-
  
  Operators 26 -  42 constituted a sub-routine which, for the code y located in the third address of cell C, calculates the true address Y of the corresponding quantity or instruction obtained in the third address of the same cell C.

  Operators 26- 32 test the magnitude of the code y.

  Corresponds to 39-54 in machine code

  todo: check that all the bounds aren't off by one!!!! (I think all the conditions need to be flipped actually)
  -}
  {-
  Op. 26 calls up exit from the sub-routine if 0000 <= y <= 000F (y is the address of a standard cell).
  -}
  operator 26 $ do
    comp cellC (var "0xF") (op 31 `offAddr` 1) (op 27)

  {-
  Op. 27 refers to op. 33 if 0010 <= y <= V_f (y is the code of the quantity having a variable address).
  -}
  operator 27 $ do
    -- might need to flip this comparison around since comp is <
    -- V_f is 0008
    comp cellC (header `offAddr` 2) (op 33) (op 28)

  {-
  Op. 28 refers to op. 37 if P_1 <= y <= C_f (y is the code of a parameter or quantity from block C).
  -}
  operator 28 $ do
    -- todo: do I need to check for P_1 or is is it contiguous with V_f?
    comp cellC (header `offAddr` 4) (op 37) (op 29)

  {-
  Op. 29 refers to op. 38 if 01A0 <= y <= 01FF (y is the code of a quantity from block gamma).
  -}
  operator 29 $ do
    -- todo: check this....
    comp cellC gammaFinal (op 38) (op 30)

  {-
  Op. 30 refers to op. 40 if 0200 <= y <= 03FF (y is the code of a postiive relative address).
  -}
  operator 30 $ do
    comp cellC (var "0x3ff") (op 40) (op 31)

  {-
  Op. 31 calls up exit from the sub-routine if 1000 <= y <= 11EF (y is the address of a cell in DS).
  -}
  operator 31 $ mdo
    comp cellC (var "0x11EF") exit (op 32)
    exit <- jcc
    pure ()

  {-
  Op. 32 refers to op. 39 if 11F0 <= y <= 11FF (y is the code of a working cell), and top op. 41 if 1200 <= y <= 13F5 (y is the code a negative relative address).
  -}
  operator 32 $ do
    -- todo: do I actually need to do the last test?
    comp cellC (var "0x11FF") (op 39) (op 41)

  {-
  Operators 33- 36 obtain the initial value of the variable address Y according to the formula:

    Y = M_1 - 1/2 (sign delta - 1) ( m - 1) + delta

  where m_1 is the address of the first cell of the storage block to which the given variable adddress refers, m is the number of cells in the block, delta is the shift of the variable adddress.

  Op. 33 selects the information on the given address according to the magnitude of the code y, obtaining from m_1, m, and delta.
  -}
  operator 33 $ mdo
    -- [ - | 1000 | - | 00FF ]
    const_02FF <- local "const_02FF"  (Raw 0b000_000_1_00_0000_0000_0_00_0000_0000_0_00_0000_0000)
    shift cellC (left 22) cellX
    ai relocationTemplate cellX next
    next <- empty

    -- m & sign(\delta)?
    bitAnd relocRes const_02FF cellX
    -- Array start 
    bitAnd relocRes secondAddr cellZ
    shift cellZ (right 11) cellZ

    chain (op 34)

  {-
  Op. 34 determines the case of a negative shift, transferring control to
  -}
  operator 34 $ do
    comp zero relocRes (op 36) (op 35)

  {-
  Op. 35 which forms the quantity m_l + (m - 1) = m_f.
  is this math correct
  -}
  operator 35 $ do
    bitAnd relocRes firstAddr cellC
    shift cellC (right 22) cellC
    add' cellC one cellC
    sub'  cellZ cellC cellZ
    chain (op 50)

  {-
  Op. 36 obtains the initial value of the variable address Y = m_sigma + delta, where m_sigma is equal to m_1 or m_f.
  -}
  operator 36 $ do
    -- -- Delta
    -- bitAnd cellA thirdAddr cellC
    -- -- ???? not correct
    -- ai cellZ cellC cellC
    jcc

  {-
  Op. 37 obtains the true address of the parameter counter for the quantity from block C according to the formula Y = y + Delta C.
  todo: is cellC + deltaC safe? 
   -}
  operator 37 $ do
    ai cellC deltaC cellC
    jcc

  {-
  Op. 38 obtains the true address of the quantity from block gamma according to the formula Y = y + Delta gamma.
  -}
  operator 38 $ do
    ai cellC deltaGamma cellC
    jcc

  {-
  Op. 39 obtains the true address of the working cell according to the formula Y = y + Delta R.
  -}
  operator 39 $ do
    ai cellC deltaR cellC
    jcc

  {-
  Op. 40 obtains the address k' of the instructiosn in the block K according to the formula k' = y - 0200 + (c.K).
  -}
  operator 40 $ do
    sub' cellC (var "0200") cellC
    ai cellC counterK k'
    chain (op 42)

  {-
  Op. 41 obtains the address of instruction k' in the block K according to the formula k' = (c.K) - ( y - 1200).
  -}
  operator 41 $ do
    sub' cellC (var "1200") cellC
    sub' counterK cellC k'
    chain (op 42)

  {-
  Op. 42 obtains the true address of the isntruction according to the formula Y = k' + Delta K.
  -}
  operator 42 $ do
    ai k' deltaK cellC
    jcc


{-
  The block IV-PP-3 prints the information on the storage distribution, prints if necessary, the assembled programme and constants, transforms the constants, if necessary t the binary systema nd writes the assembled programme and cvonstants on MD-1.

  Information on storage distribution gives data on the distribution in IS of all seven blocks (S 33): block P, block C, block K, block gamma, block O, block M and block R. The printed inforamtion on each of these blocks M (i 1..7) has the form of an instructon with zero operation code, in the addresses of which are the following quantities:

    ┌────┬────────┬────────┬────────┐
    │    │ M_1^i* │ Mbar^i │ M_f^i* │
    └────┴────────┴────────┴────────┘

  ie the address of the first cell of the block, the extetd of the block and the address of the last cell of the block. In addition, a conditional number is printed for each block, an index according to which information on the given block is conveniently located on the printed tape.
  For the i-th block thsi number is printed in the form

      + iiiiiiii,i+

  Information on the storage distribution processed by block II-PP-3 , is distributed in the storage cells in successsion (one number in each cell), in the following order: Pbar, Cbar, Kbar, gamma bar, obar, Mbar, Rbar.

  Pbar_0*=0, C_0*,K_0*,gmma_0*,O_0*,M_0*,R_0*,R_f* permitting the unambiguous formation of lines of infroamtion on all blocks.

  The indices determining if it is necessary to print the programme and constants, and if it is necessary to transform constants to the binary system are given by the contents of cell DS 1100, led out to the control console of BESM.
  Unity in the third place deontoes that the programme need not be printed, unity in the second place denotes that the constants need not be printed , unity in the first place denotes that the constants need not be transformed to the binary system.
  Zeros in the corresponding places denote the contrary.

  After printing the programme and constants (if they were required to be printed), two check sums are printed (the contents of the programme with all constants relating to it are summed before and after writing on MD-1). Agreement of the check sums ensures correct recording of the constructed programme on MD-1.
-}

pp3_4 = do
  pinned "header" "programme header table" (Size 15)

  let binaryConvertSub_1120 = Absolute 0x1120
  let decimalConvertSub_10a2 = Absolute 0x10a2

  const_035d <- local "const_035d" (Raw 0x0) -- 2^1,11111111
  const_035e <- local "const_035e" (Raw 0x0) -- 2^8,88888888

  checksum <- local "checksum" Cell
  cell_03f1 <- local "cell_03f1" Cell
  cell_03f2 <- local "cell_03f2" Cell  
  cell_0001 <- local "cell_0001" Cell
  let const_03d2 = var "const_03d2"

  let addr_0344 = var "addr_0344"

  let controlFlags = Absolute 0x1100

  transferCounter <- local "transferCounter" Cell

  cell_0004 <- local "cell_0004" Cell
  cell_0002 <- local "cell_0002" Cell

  let cell_000a = var "cell_000a" -- something from header
  let cell_0006 = var "cell_0006" -- something from header

  let gamma0 = header `offAddr` 7
  let c0 = var "C_0"

  const_0305 <- local "const_0305" (Raw 0x3)

  let const_03ff = var "const_03ff"

  let pBar = var "pBar"

  c0 <- local "c0" Cell -- 0x3e9

  local "0304" (Raw 4)

  p0 <- local "P0" Cell          -- 0x3D8

  global "pBar" Cell

  -- this makes no sense?
  aiTemplate <- local "const_03ea" (Template $ AI (var "??") one (var "??"))

  global "printEntry_039c" Cell -- actually a program
  global "printSubroutine_03a4" Cell -- actually a program

  local "const_03d2" (Template $ TN (Absolute 0b1_11_1111_1111) (var "const_03F1") UnNormalized)
  {-
  Op. 1 carries out the prepatory instructions.
  -}
  operator 1 $ do
    tN' zero p0
    tN' pBar c0
    tN' transferCounter cell_0004
    chain (op 2)


  {-
  Op. 2 prints a number, the index of hte i-th block and formas a line of information on this block.
  -}
  operator 2 $ do
    pN' cell_0004
    ai p0 one cell_0001
    shift cell_0001 (left 22) cell_0001
    shift pBar (left 11) cell_0002
    ai cell_0001 cell_0002 cell_0001
    ai cell_0001 c0 cell_0001
    chain (op 3)

  {-
  Op. 3 prints the line of information.
  -}
  operator 3 $ do
    callRtc (var "printSubroutine_03a4") (var "printEntry_039c")
    chain (op 4)


  {-
  Op. 4 realizes address-modification in op. 2
  -}
  operator 4 $ do
    ai (op 2 `offAddr` 1) oneFirstAddr (op 2 `offAddr` 1)
    ai (op 2 `offAddr` 3) oneFirstAddr (op 2 `offAddr` 3)
    ai (op 2 `offAddr` 5) oneSndAddr   (op 2 `offAddr` 5)
    add' cell_0004 const_035d cell_0004
    chain (op 5)

  {-
  Op. 5 repeats the functioning of operators 2-4 seven times.
  -}
  operator 5 $ do
    comp cell_0004 const_035d (op 2) (op 6)

  {-
  Op. 6 carries out the prepatory instructions connected with summing and prining the instructions.
  -}
  operator 6 $ do
    pN' const_035e
    pN' const_035d
    bitAnd controlFlags (var "0304") cell_0004
    ai oneFirstAndThird const_03ff (op 7)
    tN' (header `offAddr` 11) transferCounter
    chain (op 7)

  {-
  Op. 7 selects the next instruction from block K and adds it to the check sum.
  -}
  operator 7 $ do
    empty  -- Transfer from K 
    ai' checksum cell_0001 checksum
    ai transferCounter one transferCounter
    chain (op 8)


  {-
  Op.  8 verifies if it is necessary to print the instruction (YES  -- op. 9, NO -- op. 10).
  -}
  operator 8 $ do
    comp const_0305 controlFlags (op 10) (op 9)

  {-
  Op. 9 prints the instruction.
  -}
  operator 9 $ do
    callRtc (var "printSubroutine_03a4") (var "printEntry_039c")
    chain (op 10)

  {-
  Op. 10 carries out address-modification of selection instructions in op. 7.
  -}
  operator 10 $ do
    ai (op 7) oneFirstAddr (op 7)
    chain (op 11)

  {-
  Op. 11 repeats operators 7-10 for all instructions of the programme.
  -}
  operator 11 $ do
    comp transferCounter (header `offAddr` 12) (op 7) (op 12)

  {-
  Op. 12 carries out prepatory instructions connected with summation and printing the constnats from block gamma.
  -}
  operator 12 $ do
    shift gamma0 (left 22) cell_0001
    ai oneFirstAndThird cell_0001 (op 13)
    tN' gamma0 transferCounter
    chain (op 13)

  {-
  Op. 13 selects from block gamma the next constant and adds it to the check sum.
  -}
  operator 13 $ do
    empty  -- Transfer from gamma 
    ai transferCounter one transferCounter
    ai' checksum cell_0001 checksum
    chain (op 14)

  {-
  Op. 14 verifies if it is necessar yto print the constants from the block (YEs -- op. 15, No -- op. 16).
  -}
  operator 14 $ do
    comp const_0305 controlFlags (op 26) (op 15)

  {-
  Op. 15 prints the constant sfrom block gamma.
  -}
  operator 15 $ do
    callRtc (var "printSubroutine_03a4") (var "printEntry_039c")
    chain (op 16)

  {-
  Op. 16 carries out modification of the selection instruction addresses in op. 13.
  -}
  operator 16 $ do
    ai (op 13) oneFirstAddr (op 13)
    chain (op 17)

  {-
  Op. 17 repeats the function of operators 13-16 for all constants of block gamma.
  -}
  operator 17 $ do
    comp transferCounter cell_0006 (op 13) (op 18)

  {-
  Op. 18 writes blocks K and gamma on MD-1 in cells K_1* -K_f* and gamma _1* - gamma_f * respectively and reads block C from MD-2 into IS.
  -}
  operator 18 $ mdo
    -- Write K to MD-1
    ai (header `offAddr` 11) one cell_0001
    ai (var "const_03da") one cell_0002
    shift cell_0002 (left 11) cell_0002
    ai cell_0001 cell_0002 cell_0001
    ai maAddr cell_0001 maAddr
    shift (header `offAddr` 12) (left 11) cell_0001
    ai mbAddr cell_0001 mbAddr
    -- todo investigate this absolute
    maAddr <- ma (Absolute 0x0301) zero zero
    mbAddr <- mb zero

    -- Write gamma to MD-1
    ai gamma0 one cell_0001
    ai (var "const_03db") one cell_0002
    shift cell_0002 (left 11) cell_0002
    ai cell_0001 cell_0002 cell_0001
    ai ma2 cell_0001 ma2
    shift (var "const_03dc") (left 11) cell_0001
    ai mb2 cell_0001 mb2
    ma2 <- ma (Absolute 0x0301) zero zero
    mb2 <- mb zero

    -- Read C from MD-2
    shift c0 (left 11) cell_0001
    ai ma3 cell_0001 ma3
    shift cell_000a (left 11) cell_0001
    ai mb3 cell_0001 mb3
    -- todo: audit the 0x0102
    ma3 <- ma (Absolute 0x0102) zero (var "addr_0010")
    mb3 <- mb zero
 
    -- Prepare for constant processing
    bitAnd controlFlags (var "const_0305") cell_0004
    tN' zero transferCounter
    ai const_035e const_035d cell_0001
    pN' cell_0001
    chain (op 19)

  {-
  Op. 19 transfers the constant from block C to the standard cell.
  -}
  operator 19 $ do
    tN' (var "addr_0010") cell_03f1
    ai' checksum cell_03f1 checksum
    chain (op 20)


  -- comp cell_0004 const_01b9 const_034b (op 22)

  --  comp one cell_0004 const_034c (op 21)
  {-
  flags are stored in 0x1100 ... (dont print prog) (dont print consts) (dont encode binary)
  Op. 20 verifies if it is necessary to tansform the constant to the binary system (YES -- op. 21, No -- op. 22).
  -}
  operator 20 $ do
    comp cell_0004 one (op 21) (op 22)

  {-

  Op. 21 transforms the constant to the binary system..
  -}
  operator 21 $ do
    clcc binaryConvertSub_1120
    chain (op 23)

  {-
  Op. 22 transforms a binary constant to the decimal system, preparing it at the same time for printing.
  -}
  operator 22 $ do
    clcc decimalConvertSub_10a2
    tN' cell_03f2 cell_03f1
    chain (op 22)
  {-
  Op. 23 verifies it if is necessary to print the constant (YES -- op. 24, NO -- op. 25).
  -}
  operator 23 $ do
    comp one cell_0004 (op 25) (op 24)
    -- compMod cell_03f1 one (op 24) (op 25)

  {-
  Op. 24 prints the constant in the decimal system.
  -}
  operator 24 $ do
    pN' cell_03f1
    chain (op 25)

  {-
  Op. 25 adds the binary constant to the check sum and sends it to block C.
  -}
  operator 25 $ do
    add' checksum cell_03f1 checksum
    tN' cell_03f2 (var "addr_0010")
    chain (op 26)
    

  {-
  Op. 26 carries out modification of addresses in op. 25 -and op .19 .
  -}
  operator 26 $ do

    ai (op 19) oneFirstAddr (op 19)
    ai transferCounter one transferCounter
    ai (op 25 `offAddr` 1) one (op 25 `offAddr` 1)
    chain (op 27)

  {-
  Op. 27 repeats functioning of operators 19-26 for all constants of the block C.
  -}
  operator 27 $ do
    comp transferCounter const_03d2 (op 19) (op 28)

  {-
  Op. 28 writes block C on Md-1 in cells C_1* - C_f* and prints the first check sum.
  -}
  operator 28 $ mdo
    add' c0 one cell_0001
    shift cell_0001 (left 11) cell_0001
    -- Write C to MD-1 (addresses uncertain)
    ai maAddr cell_0001 maAddr
    shift (var "const_03ea") (left 11) cell_0001
    ai mbAddr cell_0001 mbAddr 
    maAddr <- ma zero zero zero
    mbAddr <- mb zero 
    tN' checksum cell_03f1 
    clcc decimalConvertSub_10a2
    pN' cell_03f2

    chain (op 29)

  {-
  Op. 29 reads the contents of the programme with all of its constant from MD-1.
  -}
  operator 29 $ do
    -- initialize transfer counter
    -- read (using ma and mb) into standard location
    -- initialize checksum
    chain (op 30)

  {-
  Op. 30 and op. 31 repeat summation of the programme and its constants.
  -}
  operator 30 $ do
    -- add next constant / instr 
    chain (op 31)

  {-
  Op. 31 repeats summation of the programme and its constants.
  -}
  operator 31 $ do
    -- until transfer count < max continue
    comp zero zero (op 30) (op 32)

  {-
  Op. 32 prints the second check sum.
  -}
  operator 32 $ do
    -- Print final checksum
    tN' checksum cell_03f1
    clcc decimalConvertSub_10a2
    pN' cell_03f2
    stop

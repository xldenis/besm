{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}

module Besm.PP2 where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

import qualified Data.Bits as B

{-
  The third part of the programming programme contains approxiamtely 450 instructions and 30 constants and consists of four blocks, functioning in succession one after the other: the block for forming relative addresses (I-PP-3), the block for distributing the store (OO-PP-3), the block for assigning true addresses (III-PP-3) and the block for printing information (IV-PP-3). The functioning of PP-3 is organized by a small master programme (MP-3), containing also the standard sub-routine for printing instructions in reference to them by the instruction found in cell 0001. This sub-routine is taken from the library of standard programmes and therefore its description will not be presented heere.

  During the functioning of PP-3 the store contains MP-3 and one of four blocks of PP-3, called up when necessary, since the PP can engage not more than 256 celles of IS. Information on the problem in standard position is stored in IS completely. At the termination of functioning of PP-3 the finished programme is found on MD-1.

  PP-3 realizes the third stage of functionmin of the programming algorithm (S 19) and functions in the following manner (Fig 15).


  Op. 1 of MP-3 reads the programme block I-PP-3 from MD-4 and transfers control to it. The block I-PP-3 changes the codes of the operator numbers in the instructions to the relative addresses of the first instructions of these operators, eliminates the open-parentheses of loops from the programme and signs of operator numbers and chagnes the relative address of the instructions in correspondence with theese.

  Op. 2 reads from MD-4 the programme of block II-PP-3 and transfers control to it. The block II-PP-3 carries out distribution of the store,determining the final location of the storage blocks employed in the solution of the programme and calculates the corrections which must be added to the codes of quantities to obtain their true addresses.

  Op. 3 reads from MD-4 the programme of block III-PP-3 and transfers control to it. The block III-PP-3 carries out assignment of true addresses.

  Op. 4 reads from MD-4 the programme of block IV-PP-3 and transfers control to it. The block IV-PP-3 prints information on the storage distribution, prints programme components and constants relating to them, composes the programme and the constants in accordance with the distribution of the store and writes them on MD-1, completing by this the functioning of the PP.

  In the description of the functioning of PP-3 it is necessary to consider the two positions of blocks in the store: standard and "final", ie that which they are to have in correspondence with which position is under consiteration the characteristics of the block locations in their final positions will be denoted by an asterisk. Thus for example 𝜎*f denotes the last cell of block 𝜎 in its final position in the store in distinction to 𝜎f, denoting hte same characteristic in the standard position of block 𝜎.
-}

{-
  Block for the formation of relative addresses

  The block I-PP-3 consists of the basic programme (operatores 1 -33) and four sub-routies: the sub-routines for forming relative addresses (operators 34-38) , the sub-routine for changing relative addresses  (operators 39- 45), the sub-routine for selecting instructions from block K (op. 46) and the sub-routine for transferring instructions to block K (op. 47).

  The first part of the basic programme (operators 1-13) carries out the substitution in the instructions of the operator numbers by the relative addresses of the first instructions of the operators with teh =given numbers. Operators 1-13 organize the sequence of selection of programme instruction addresses and return of instructions wit hchagned addresses to the programme. The direct testinf of addresses and the substitution of operator numbers by relative addresses is realized by teh sub-routine for forming relative addresses.

  Op. 1 sends K0 to counter K, in which is fixed the address of the last instruction selectedfrom block K.

  Op. 2 selects the next instruction in block K and adds unity to counter K.

  Op. 3 determines the case where open-parentheses of the loop or the sign of an operator number have been selected, transferring control to op. 12.

  Op. 4 in the case of selection of instructions Ma or Mb transferrs control immediately to op. 10 since tn thises instructions the first wo addresses are not examined.

  Op. 5 determinaes the first address of the instruction and shifts it o the third address of the standard cell A.

  Op. 6 examines the extracted address.

  Op. 7 sets the tested first address in the instruction and extracts the second address from the latter, shifting it o the third address of cell A.

  Op. 8 tests the selected address.

  Op. 9 tests the selected second address in the instruction.

  Op. 10 extracts the third address of the instructioin and sets it in cell A.

  Op. 11 tests the selected address and then sets it in the instruction.

  Op. 12 transfers the instruction back to block K.

  Op. 13 transfers control to op. 2 if all instructions from block K have not been examined.

  Operators 14-25 realize change in relative address, connected with elimination from the programmme of open-parenthesesd and signs of operator numbers. For this a special sub-routine  (operators 39-45) examines the addresses of instructions and for eacvh relative addreess Δ calcuates the number n of the open-parentheses of loops and signs of operator numbers located in the programme betwwen the instruction having the given relative address Δ and the instruction in which this address is encountered. The number n obtained is the n subtracted from the absolute magnitude of Δ.

  Op. 14 sends K0 to counter K.

  Op. 15 selects the next instruction from the block K and adds unity to counter K.

  Op. 16, in the csae of selection of instructions Ma and Mb transfers control to op. 22 since in these instructions the relative addresses may be located only in the third address.

  Op. 17 extracts the first address of the instruction and sends it o the third address of the standard cell A.

  Op. 18 examines the extracted address.

  Op. 19 sets the tested address in the instruction, extracts the second address and sends it to the third address of cell A.

  Op. 20 tests the extracted address.

  Op. 21 sends the tested address to the instruction.

  Op. 22 extracts the third address of the instruction, sending it to cell A.

  Op. 23 tests the extracted address.

  Op. 24 sends the tested address to the instruction and transfers the instruction bakc to block K.

  Op. 25 transfers control to op. 15 if all instructions in block K have not been examined.

  Operators 26 - 33 remove open-parentheses of loops and the isgns of operator numbers from the assembled programme. Printing of information on the constructed programme takes place this time. The open-parentheses of loops are printed and the signs of operator numbers, in the third addresses of which is first place d the true address of the first instruction of the loop (for open-parentheses) or the true address of the first instruction of the operator (for signs of operator numbers).

  Op. 26 calculates the true initial K_1* of the programme and sets it in a special counter in which, at the moment of selection from the programme of the open-parentehses of a loop or the sign of an operator number, will be the true address of the following instruction.

  Op. 27 selects the next instruction of the programme from block K.

  Op. 28 transfers control to op. 29 if an instruction has been selected to op. 30 if the open-parentheses of a loop or the sign of an operator number has been selected.

  Op. 29 sends the instruction back to the programme and adds unity to the special counter of true instruction addresses.

  Op. 30 sets in the third address of the open-parentheses of the loop or the sign of the operator number the true addresses of the following instruction.

  Op. 31 prints the open-parentheses of the loop or the operator number sign.

  Op. 32 transfers control to op. 28  if all instructions of the programme have not been examined.

  Op. 33 calculates and asets the cell 000C the new value of K_f, changted through elimination of the open-parenteheses of loops and signs of operator numbers.

  Op. 34048 constitute a sub-routine for the formulation of relative addresses.

  Op. 34 transfers control to op. 35 if the number of an operator is in standard cell a.

  Op.  35 shifts the numbers of teh operators to the first address and sets K _1 in the special counter K'.

  Op. 36 elects the next instruction from block K. begining with the first instruction and adds unity to counter K'.

  Op. 37 repeats op. 36 until the sign of the given operator number is selected.

  Op. 38 obtains the codde of the relative address equal to 0200 + |(C.K') - (C.L)| for the positive releative addrss and 1200 + |(c.K') - (c.K)| for negative.

  Operators 39 - 45 constitute the sub-routine changing the relative addresses.

  Op. 39 transfers control to op. 40 if a relative address is in standard cell A.

  Op. 40 clears counter K' and standard cell B.

  Op. 41 selects the next instruction from the part of the programme located between the instruction with the given relative address and the instruction containing this relative address and ads unity to counter K'.

  Op. 42 determines the case of selection of an open-parenthesis of a loop or the sign of an operator number, transferring control to op. 43

  Op. 43 which adds 1 to cell B.

  Op. 44, comparing the indications of counter K' with the absolute magnitude Δ of the relative address, repats the functioning of operators 41-43 Δ times.

  Op. 45 forms the new value of the absolute magnitude of the relative address, equal to Δ - (B).

  Op. 46 is the sub-routine for selecting instructions from block K.

  Op. 47 is the sub-routine for transferring instructions to block K.

-}

{-

  Storage Distribution Block

  In accordance with the diagram on page 70 the final distribution of the store appears as follows: parameter counters (block ), constants and variable quantities (block C), programme (block K), constants for the programme (block \ɣ), block O, M and, finally the working cells (block R). For each of these seven bloccks M^i the storage distribution block (II-PP-3) forms the following quantities: M_O^i*, the address of the cell diractly before the block, Mbar^i, the extent of the block and M_f*^i, the address of the last cell of the block (we note that M_f*^i = M_o^(i + 1). Then the corrections to the codes of quantities are calculated: Δ C, the correction for obtaining the true addresses of constants and variable quantiteis from the block C and the parameter counters, Δ K, the correction for obtaining the true addresses of programme instructions Δɣ, the correction for obtaining the true addreses of constants from block ɣand Δ R, the correction for obaining the true addresses of working cells.

  In addition, the block II-PP-3 carries out transformation of information on variable addressses in the block V and prints inforamtion on the variable addresses in the block V and the positions of the blocks M^i in the store. The third address of the first cell of M^i is placed in each "main head" with information on storage blocks M^i (i = 1..l); the "main head" itself with the eliminated operation code are printed in the form of an instruction. In each line of iunformation about a variable address relating to the storage block M^i (i = 1..l), the magnitude of the block M^i is set in the first address while in the second address , the address of its start M*_l^i.
  These data will be utilized in forming the initial values of variable addrsesses during the period of functioning of block III-PP-3.
  the scheme of block II-PP-3 is represented in Fig. 17.

  Op 1. calculates Pbar, Cbar, Kbar, ɣbar Obar and C_0*, K_0*, ɣ_0*, O_0*, M_0*

  Op. 2 calcualtes tje correctio nΔ C, forms the instructions for processing the block V and prints a number of special form, whhich ddenotes that immediately after it on the tape will be printed information on the strange blocks.

  Op. 3 selects the next line of information of block V.

  Op. 4 tetsts the selected line, transferring control to op. 5 if a "main head" has been selected for the next block M^i (i = 1.. l).

  Op. 5 extracts the third address of the "main head".

  Op. 6 compares the extracted address with zero. If it is not equal to zero this deneotes that the corresponding block M^i relates to the group of constants from block C. In this case op. 10 functions.

  Op. 7 sets M_l^i* in tyhe third address of the "main head" and calculates M_l^(i+1)* = M_l^i* +Mbar^i.

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

{-
  Block for Assigning True Addresses

  The block III-PP-3 substitues their true addresses for the codes of quantities and the relative address of variable instructions in the instructions of the assembled programme. Detemrinination of the types of quantities according to the values of their codes is cvarried out in accordance with Table 17 (S 19).

  The first part of the block (operators 1 -18) carries out assignment of true addresses. The direct substitution of codes of quantities or relative addresses by true address of quantities or instructions is carried out by a sub-routeine (operators 26 -42).

  Op. 1 sets K_0 in coutner K, in which will be stored the addres of the current instruction selected from the programme during funcitioning of the block.

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
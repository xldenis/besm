{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE RecursiveDo #-}

module Besm.PP3_IV where

import Besm.Assembler.Monad
import Besm.Assembler.Syntax

{-|
  Block IV-PP-3: Printing Information

  This implementation is based on the transcribed machine code from source_materials/pp3/IV/*.md
  which represents an earlier draft of the code. It differs from the documented final version
  in the following ways:

  - Operators 1-19: Nearly identical to documented version
  - Operators 20-30: Different control flow for constant processing
  - Missing: The final verification phase (ops 29-32 in documentation) that re-reads from MD-1
    and computes/prints a second checksum

  The transcribed version writes the program to MD-1 with one checksum, while the final
  documented version adds additional verification by re-reading and re-computing the checksum.

  Many addresses and constants are marked with `var` to indicate uncertainty in the transcription.
-}

pp3_4 = do
  -- Working cells and counters
  cell_0001 <- local "0001" Cell  -- Standard working cell
  cell_0002 <- local "0002" Cell  -- Standard working cell
  cell_0003 <- local "0003" Cell  -- Standard working cell for flag extraction
  cell_0004 <- local "0004" Cell  -- Group/iteration counter

  -- Checksum accumulator (transcription: 03d8)
  checksum <- local "checksum" (Raw 0x03d8)

  -- Transfer counter (transcription: 03fd)
  transferCounter <- local "transferCounter" (Raw 0x03fd)

  -- Standard cells for constant processing
  cell_03f1 <- local "03f1" Cell  -- Standard cell for constants
  cell_03f2 <- local "03f2" Cell  -- Translated constant storage

  -- Flags and controls from DS 1100
  controlFlags <- local "controlFlags" (Raw 0x1100)

  -- Storage distribution info constants (from pp3_2)
  pBar <- global "Pbar" Cell       -- 03e1
  cBar <- global "Cbar" Cell       -- 03e2
  kBar <- global "Kbar" Cell       -- 03e3
  gammaBar <- global "gammaBar" Cell  -- 03e4

  p0 <- global "P0" Cell           -- 0008 (also 03e8 in transcription)
  c0 <- global "C0" Cell           -- 03EA (also Ko)
  k0 <- global "K0" Cell           -- 03EB (also T0)
  gamma0 <- global "gamma0" Cell   -- 03EC (also F0)

  -- Uncertain addresses and constants from transcription
  let addr_0306 = var "addr_0306"  -- Op 2 address
  let addr_0307 = var "addr_0307"
  let addr_0308 = var "addr_0308"
  let addr_0309 = var "addr_0309"
  let addr_030b = var "addr_030b"
  let addr_0317 = var "addr_0317"  -- Transfer from K template
  let addr_031c = var "addr_031c"  -- Op 10 address
  let addr_0321 = var "addr_0321"  -- Transfer from gamma template
  let addr_0326 = var "addr_0326"  -- Op 16 address
  let addr_0344 = var "addr_0344"  -- Transfer from C instruction
  let addr_034d = var "addr_034d"  -- Return to C instruction
  let addr_0353 = var "addr_0353"  -- gamma_k comparison
  let const_10b7 = var "const_10b7"  -- Address increment
  let const_10b8 = var "const_10b8"
  let const_10b9 = var "const_10b9"  -- +1 constant
  let const_01b9 = var "const_01b9"  -- Decimal flag value
  let const_0bb = var "const_0bb"
  let const_03ff = var "const_03ff"
  let const_0305 = var "const_0305"
  let const_034b = var "const_034b"  -- Op 24 address
  let const_034c = var "const_034c"  -- Op 25 skip address
  let const_034f = var "const_034f"  -- Op 29 address
  let const_035d = var "const_035d"  -- Block count (7)
  let const_035e = var "const_035e"  -- Print marker
  let const_03d2 = var "const_03d2"  -- C extent

  {-
  Op. 1 carries out the preparatory instructions.

  Transcription (9_390.md:1-5):
    ,tn  03e8                    ; P₀ = 0
    ,tn  03e1 03e9               ; Transfer P to general location
    ,tn  03fd 0004               ; Initialize group counter
    clcc 0306                    ; To op 2
  -}
  operator 1 $ do
    tN' zero p0
    tN' pBar (var "03e9")
    tN' transferCounter cell_0004
    chain (op 2)

  {-
  Op. 2 prints a number (index) of the i-th block and forms a line of information.

  Transcription (9_390.md:9-15):
    ,tn  0004 0200                  ; Print number ℓ of next group
    ai   03e8 10b9 0001             ; [P] +1
    <-   0001 0016 0001             ; [P] in A₁
    <-   03e1 000b 0002             ; [P] in A₂
    ai   0001 0002 0001             ; Combine
    ai   0001 03e9 0001             ; Add third address
  -}
  operator 2 $ do
    pN' cell_0004
    ai p0 const_10b9 cell_0001
    shift cell_0001 (left 22) cell_0001
    shift pBar (left 11) cell_0002
    ai cell_0001 cell_0002 cell_0001
    ai cell_0001 (var "03e9") cell_0001
    chain (op 3)

  {-
  Op. 3 prints the line of information.

  Transcription (9_390.md:17-18):
    clcc 03a4 038c               ; Print group info
  -}
  operator 3 $ do
    clcc (var "printSubroutine_03a4") (var "printEntry_038c")
    chain (op 4)

  {-
  Op. 4 realizes address-modification in op. 2 to prepare for next block.

  Transcription (9_390.md:20-24):
    ai   0308 10b7 0307          ; Preparation
    ai   0309 10b7 0309          ; for transition
    ai   030b 10b8 030b          ; to next array group
    ,+   0004 035d 0004
  -}
  operator 4 $ do
    ai addr_0308 const_10b7 addr_0307
    ai addr_0309 const_10b7 addr_0309
    ai addr_030b const_10b8 addr_030b
    add' cell_0004 const_035d cell_0004
    chain (op 5)

  {-
  Op. 5 repeats the functioning of operators 2-4 seven times (for 7 storage blocks).

  Transcription (9_390.md:26-27):
    <    0004 035d 0306          ; All iterations? NO - op 2
  -}
  operator 5 $ do
    comp cell_0004 const_035d (op 2) (op 6)

  {-
  Op. 6 carries out the preparatory instructions connected with summing and printing instructions.

  Transcription (9_390.md:29-34):
    ,tn  035e 0200                  ; Print start markers
    ,tn  035d 0200                  ; for program info
    ^    1100 0304 0004             ; Extract print flag
    ai   0bb 03ff 0317              ; Form transfer command
    ,tn  000b 03fd                  ; K₀ to counter
  -}
  operator 6 $ do
    pN' const_035e
    pN' const_035d
    bitAnd controlFlags (var "0304") cell_0004
    ai const_0bb const_03ff addr_0317
    tN' (header `offAddr` 11) transferCounter
    chain (op 7)

  {-
  Op. 7 selects the next instruction from block K and adds it to the check sum.

  Transcription (9_390.md:36-39):
    blank                       ; Transfer from program to 0001
    ,ai  03d8 0001 03XX         ; Get checksum
    ai   03fd 10b9 03fd         ; +1 to counter K
  -}
  operator 7 $ do
    empty  -- Transfer from K (template in addr_0317)
    add' checksum cell_0001 checksum
    ai transferCounter const_10b9 transferCounter
    chain (op 8)

  {-
  Op. 8 verifies if it is necessary to print the instruction.

  Transcription (9_390.md:41-42):
    <    0305 1100 031c         ; Need to print? NO - op 10
  -}
  operator 8 $ do
    comp const_0305 controlFlags addr_031c (op 9)

  {-
  Op. 9 prints the instruction.

  Transcription (9_390.md:44-45):
    clcc 03a4 039c              ; Print command
  -}
  operator 9 $ do
    clcc (var "printSubroutine_03a4") (var "printEntry_039c")
    chain (op 10)

  {-
  Op. 10 carries out address-modification of selection instructions in op. 7.

  Transcription (9_390.md:47-48):
    ai   0317 10b7 0317         ; Address modification
  -}
  operator 10 $ do
    ai addr_0317 const_10b7 addr_0317
    chain (op 11)

  {-
  Op. 11 repeats operators 7-10 for all instructions of the programme.

  Transcription (9_390.md:50-51):
    <    03f3 000c 0317         ; Counter K < K_k? YES - op 7
  -}
  operator 11 $ do
    comp transferCounter (header `offAddr` 12) (op 7) (op 12)

  {-
  Op. 12 carries out preparatory instructions for gamma block constants.

  Transcription (9_390.md:53-56):
    <-   000d 0016 0001         ; γ₀ in A₁
    ai   10bb 0001 0321         ; Form transfer command
    ,tn  000d 03fd              ; γ₀ to counter
  -}
  operator 12 $ do
    shift gamma0 (left 22) cell_0001
    ai (var "const_10bb") cell_0001 addr_0321
    tN' gamma0 transferCounter
    chain (op 13)

  {-
  Op. 13 selects from block gamma the next constant and adds it to the check sum.

  Transcription (9_390.md:58-61):
    blank                       ; Transfer from gamma to 0001
    ai   03fd 10b9 03fd         ; +1 to counter
    ,tn  03d8 0001 03d8         ; Get checksum
  -}
  operator 13 $ do
    empty  -- Transfer from gamma (template in addr_0321)
    ai transferCounter const_10b9 transferCounter
    tN' checksum cell_0001 checksum
    chain (op 14)

  {-
  Op. 14 verifies if it is necessary to print the constants.

  Transcription (9_390.md:63-64):
    <    0305 1100 0326         ; Need to print? NO - op 16
  -}
  operator 14 $ do
    comp const_0305 controlFlags addr_0326 (op 15)

  {-
  Op. 15 prints the constants from block gamma.

  Transcription (9_390.md:66-67):
    clcc 03a4 0382              ; Print command
  -}
  operator 15 $ do
    clcc (var "printSubroutine_03a4") (var "printEntry_0382")
    chain (op 16)

  {-
  Op. 16 carries out modification of the selection instruction addresses.

  Transcription (9_390.md:69-70):
    ai   0321 10b7 0321         ; Address modification
  -}
  operator 16 $ do
    ai addr_0321 const_10b7 addr_0321
    chain (op 17)

  {-
  Op. 17 repeats the function of operators 13-16 for all constants of block gamma.

  Transcription (9_390.md:72-73):
    <    0353 0004 0321         ; Counter < γ_k? YES - op 13
  -}
  operator 17 $ do
    comp addr_0353 cell_0004 addr_0321 (op 18)

  {-
  Op. 18 writes blocks K and gamma on MD-1 and reads block C from MD-2.

  Transcription (9_390.md:75-83 + 9_391.md:1-21):
    ai   000b 10b9 0001         ; F₀+1=F₁
    ai   03da 10b9 0002         ; K₀+1=K₁
    <-   0002 000a 0002         ; K₁ in A₂
    ai   0001 0002 0001         ; Combine
    ai   032f 0001 032f         ; Form Ma
    <-   03eb 000b 0001         ; Kₘ in A₁
    ai   0330 001 0330          ; Form MC
    ma   0301                   ; Write to MD
    mb                          ; (continues for gamma and C read)

  This operator contains complex MD operations with many uncertain addresses.
  -}
  operator 18 $ do
    -- Write K to MD-1
    ai (header `offAddr` 11) const_10b9 cell_0001
    ai (var "const_03da") const_10b9 cell_0002
    shift cell_0002 (left 10) cell_0002
    ai cell_0001 cell_0002 cell_0001
    ai (var "addr_032f") cell_0001 (var "addr_032f")
    shift (header `offAddr` 12) (left 11) cell_0001
    ai (var "addr_0330") cell_0001 (var "addr_0330")

    -- Write gamma to MD-1
    ai gamma0 const_10b9 cell_0001
    ai (var "const_03db") const_10b9 cell_0002
    shift cell_0002 (left 11) cell_0002
    ai cell_0001 cell_0002 cell_0001
    ai (var "addr_0338") cell_0001 (var "addr_0338")
    shift (var "const_03dc") (left 11) cell_0001
    ai (var "addr_0339") cell_0001 (var "addr_0339")

    -- Read C from MD-2
    shift c0 (left 11) cell_0001
    ai (var "addr_033d") cell_0001 (var "addr_033d")
    shift (var "const_03ea") (left 11) (var "addr_033f")
    ai (var "addr_033f") cell_0001 (var "addr_033b")

    -- Prepare for constant processing
    bitAnd controlFlags (var "const_0305") cell_0004
    tN' zero transferCounter
    ai const_035e const_035d cell_0001
    pN' cell_0001
    chain (op 19)

  {-
  Op. 19 transfers the constant from block C to the standard cell.

  Transcription (9_391.md:23-25):
    ,tn  0010 03f1              ; Transfer constant
    ,ai  03e8 03f1 03e8         ; Get checksum
  -}
  operator 19 $ do
    empty  -- Transfer from C (instruction at addr_0344)
    add' checksum cell_03f1 checksum
    chain (op 20)

  {-
  Op. 20 verifies if it is necessary to print the constant.

  Transcription (9_391.md:27-28):
    <    10b9 0004 034c         ; Print constant? NO - op 25
  -}
  operator 20 $ do
    comp const_10b9 cell_0004 const_034c (op 21)

  {-
  Op. 21 checks if constants are in decimal system.

  Transcription (9_391.md:30-31):
    <    0004 01b9 034b         ; Decimal system? YES - op 24
  -}
  operator 21 $ do
    comp cell_0004 const_01b9 const_034b (op 22)

  {-
  Op. 22 checks if the constant cell contains a dummy value.

  Transcription (9_391.md:33-34):
    |<|  03f1 10b9 034b         ; Dummy? YES - op 24
  -}
  operator 22 $ do
    comp' cell_03f1 const_10b9 const_034b (op 23)

  {-
  Op. 23 translates the constant to decimal system.

  Transcription (9_391.md:36-38):
    clcc 10a2                   ; Convert to decimal
    ,tn  03f2 03f1              ; Transfer converted
  -}
  operator 23 $ do
    clcc (var "decimalConvertSub_10a2")
    tN' cell_03f2 cell_03f1
    chain (op 24)

  {-
  Op. 24 prints the constant.

  Transcription (9_391.md:40-41):
    ,tn  03f1 0200              ; Print constant
  -}
  operator 24 $ do
    pN' cell_03f1
    chain (op 25)

  {-
  Op. 25 extracts flag about constant recording system.

  Transcription (9_391.md:43-44 + 9_392.md:1-2):
    clcc 0360                   ; To op 25
    ^    0004 10b9 0003         ; Extract flag
  -}
  operator 25 $ do
    bitAnd cell_0004 const_10b9 cell_0003
    chain (op 26)

  {-
  Op. 26 checks if constants are in decimal system.

  Transcription (9_392.md:3-4):
    <    0003 034f              ; Decimal? NO → op 29
  -}
  operator 26 $ do
    comp cell_0003 zero const_034f (op 27)

  {-
  Op. 27 translates the constant to binary system.

  Transcription (9_391.md:46-48):
    clcc 1120                   ; Convert to binary
    ,tn  03f2 0010              ; Transfer back to C
  -}
  operator 27 $ do
    clcc (var "binaryConvertSub_1120")
    tN' cell_03f2 (var "addr_0010")
    chain (op 28)

  {-
  Op. 28 carries out address modification.

  Transcription (9_391.md:50-53):
    ai   0344 10b7 0344         ; Modify fetch
    ai   03fd 10b9 03fd         ; +1 to counter
    ai   034d 10b9 034d         ; Modify store
  -}
  operator 28 $ do
    ai addr_0344 const_10b7 addr_0344
    ai transferCounter const_10b9 transferCounter
    ai addr_034d const_10b9 addr_034d
    chain (op 29)

  {-
  Op. 29 repeats the constant processing loop.

  Transcription (9_391.md:55-56):
    <    03fd 03d2 0344         ; Counter < C? YES - op 19
  -}
  operator 29 $ do
    comp transferCounter const_03d2 (op 19) (op 30)

  {-
  Op. 30 writes block C to MD-1, prints checksum, and stops.

  Transcription (9_391.md:58-67 + 9_392.md:6-10):
    ai   0363                   ; Form Ma for C
    ai   0357 0001 0357         ; Cₘ in A₂
    <-   03ea 000b 0001         ; MC instruction
    ai   0358 0001 0358         ; Complete
    ma   0301 0010              ; Write C
    mb                          ;
    ,tn  03e8 03f1              ; Transfer checksum
    clcc 10a2                   ; Convert to decimal
    ,tn  03f2 0200              ; Print checksum
    stop                        ; End of PP
  -}
  operator 30 $ do
    -- Write C to MD-1 (addresses uncertain)
    ai (var "addr_0363") zero (var "uncertain_target")
    ai (var "addr_0357") cell_0001 (var "addr_0357")
    shift (var "const_03ea") (left 11) cell_0001
    ai (var "addr_0358") cell_0001 (var "addr_0358")

    -- Print final checksum
    tN' checksum cell_03f1
    clcc (var "decimalConvertSub_10a2")
    pN' cell_03f2
    stop

  {-
  NOTE: The transcribed version ends here with one checksum.
  Operators 31 and 32 do not exist in the transcribed code.

  The documented final version (in PP3.hs comments) includes:
  - Op. 29: Reads program+constants back from MD-1
  - Op. 30-31: Re-sum program and constants
  - Op. 32: Print second checksum for verification

  This verification phase was added after the transcribed draft.
  -}

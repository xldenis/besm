# PP-BESM Compiler Bugs: `sum_loop.pp` Analysis

Analysis of the compiler output for `sum_loop.pp` compiled via:

```
cargo run --release -- trace --bootloader=boot.txt --md0=mp1.txt --md4=mp2.txt --md3=mp3.txt --start-address=1025 --md2=test_progs/sum_loop.txt --print-md-1 0:30 --no-trace
```

To recompile the compiler after making changes run:

```
stack run -- compile -o ../besm-vm --source-map-dir ../besm-vm
```

## Source Program

```
1. Variable Addresses
2. Parameters
   a : a_in = 1, a_fin = 10
3. List of Constants and Variable Quantities
   a, b, 1, 10
4. Logical Scheme
   [a a + b => b]; a + a => a; Form b ,=> 0; stop
```

Semantics: loop `a` from 1 to 10, accumulating `b = b + a` (computing 1+2+...+10 = 55). Then double `a`, print `b`, halt. No variable addresses (arrays), one simple loop.

## Expected Output

After PP-1 (arithmetic), PP-2 (loops), PP-3 (address assignment + final storage), MD-1 should contain roughly 16-20 words: a data area (zero-initialized working cells + constants 1.0 and 10.0) followed by a single code block:

```
TN   [const_1]      → [cell_a]         ; loop init: a = 1
Add  [cell_a] [cell_b] → [scratch]     ; loop body: scratch = a + b
TN   [scratch]       → [cell_b]        ; b = normalize(scratch)
,Add [cell_a] DS[1153] → [cell_a]      ; loop incr: a = a + 1
Comp [cell_a] [const_10] → body_start  ; loop test: if a < bound, goto body
Add  [cell_a] [cell_a] → [scratch]     ; post-loop: scratch = a + a
TN   [scratch]       → [cell_a]        ; a = normalize(scratch)
TN   [cell_b]        → [scratch]       ; Form b: normalize
TN   [scratch]       → 0               ; output (addr 0 = printer)
Stop
```

Key properties: one contiguous code block, all addresses consistent and in range, working cells near the code, loop comparison jumps to body (not init), no AI/CLCC (no arrays).

## Actual Output (MD-1 dump)

```
 0  0 0 0 0                    ; working cell
 1  0 0 0 0                    ; working cell (variable a)
 2  0 0 0 0                    ; working cell
 3  0 0 0 0                    ; working cell (variable b)
 4  Add  512   0   0           ; constant 1.0  (0x0280000000)
 5  Div  640   0   0           ; constant 10.0 (0x08a0000000)
 6  TN       4         1       ; init: cell[1] = normalize(cell[4])  — a = 1.0
 7  0   1  0  0                ; metadata word (processed open-bracket)
 8  Add      1    3  691       ; body: temp@691 = cell[1] + cell[3]  — a + b
 9  TN     691         3       ; cell[3] = normalize(temp)           — b = result
10  ,Add     1 1153    1       ; incr: cell[1] += DS[1153] (= 1.0)  — a = a + 1
11  Cmp      1    5    6       ; if cell[1] < cell[5], goto 6        — a < 10?
12  Add      1    1  693       ; post-loop: temp@693 = cell[1] + cell[1]
13  TN     693         1       ; cell[1] = normalize(temp)           — a = a + a
14  AI       3    0    1       ; ??? add to address part of cell[3]
15  CLCC            1186       ; ??? call DS[1186] (just JCC → no-op return)
16  TN       2       692       ; Form: temp@692 = normalize(cell[2]) — uses wrong addr for b!
17  TN     692         0       ; output
18  Stop
19  TN    1520        18       ; duplicate block with out-of-range addresses
20  Stop (0x3fffffffff)        ; max float / Stop encoding
21  Add     16   16 1522       ; duplicate of addr 12, addr 1522 out of range
22  TN    1522        16       ; duplicate of addr 13
23  AI      18    0    1       ; duplicate of addr 14
24  CLCC            1186       ; duplicate of addr 15
25  TN       2      1521       ; duplicate of addr 16, addr 1521 out of range
26  TN    1521         0       ; duplicate of addr 17
27  Stop                       ; duplicate of addr 18
```

## Bugs

### Bug 1: Duplicate code block (addrs 19-27) — PP-3

Block 2 (addrs 19-27) is a near-identical copy of the post-loop code (addrs 12-18) but with unrelocated addresses (1520-1522 instead of 691-693, a consistent offset of 829). PP-3 emitted both the raw intermediate code and the partially relocated version. Only one copy should appear.

### Bug 2: Out-of-range addresses 1520, 1521, 1522 — PP-3

These exceed both IS (max 1023) and DS (max 1408). They are raw unrelocated addresses from an intermediate compiler stage that PP-3 failed to fix up or suppress.

### Bug 3: Wildly displaced working cells 691, 692, 693 — PP-3

For a program that is only ~18 words total, scratch cells should be at nearby addresses (like 0-5), not 680+ words away. PP-3's storage distribution (II-PP-3) is miscalculating these offsets.

### Bug 4: Inconsistent address for variable `b` — PP-1 / PP-2

The loop body (PP-2) uses **addr 3** for `b`:
```
 8  Add   1    3  691   ; a + b
 9  TN  691       3     ; → b
```

But the `Form b` statement (PP-1) uses **addr 2**:
```
16  TN    2      692   ; normalize b
17  TN  692        0   ; output
```

`b` should have one consistent address. PP-1 assigned `b` relative address 2, PP-2 assigned 3 for the loop body. These should agree.

### Bug 5: Loop comparison jumps to init, not body — PP-2 / PP-3

```
 6  TN    4         1    ; ← INIT
 7  0   1  0  0          ; ← metadata word (shouldn't be here)
 8  Add   1    3  691    ; ← BODY START
...
11  Cmp   1    5    6    ; jumps to 6 (init), should be 8 (body)
```

The `Comp` third address = 6 (the init TN). Each iteration re-initializes `a` to `a_in`, creating an infinite loop. The jump target should be 8 (the body Add). This is a relative address calculation bug in PP-2 distribution or PP-3 relativization.

### Bug 6: Spurious AI and CLCC instructions (addrs 14-15) — PP-2

```
14  AI     3    0    1      ; address modification
15  CLCC          1186      ; call DS[1186] (no-op JCC stub)
```

`AI` is for address-modification in loops with variable addresses (array indexing). This program has no variable addresses. The PP-2 Distribute documentation marks address-modification instructions with `*` ("only when needed"). PP-2 is emitting them unconditionally.

### Bug 7: Potential off-by-one in loop bound — PP-2

Even with correct addresses, the loop structure is:
```
,Add  (a = a + 1)          ; increment
Comp  a  a_fin  body_start ; branch back if a < a_fin
```

`Comp` branches on strict `<`. After the body runs with a=9, increment gives a=10, and `10 < 10` is false → exit. The body only runs for a=1..9 (sum=45, not 55). For the inclusive range 1..10, the comparison needs to be against `a_fin + 1 = 11`, or the comparison/increment order needs to be swapped.


## Fixed Bugs

### deltaC local→global (PP3.hs line 668)

`deltaC` was declared `local` in pp3_2 (II-PP-3) but `global` in pp3_3 (III-PP-3). Since PP-3 hot-swaps phases by replacing the Procedures area of IS while preserving Globals, II-PP-3's locally-stored ΔC = -V_f = -15 was overwritten when III-PP-3 loaded. III-PP-3 read ΔC = 0 from the uninitialized global, leaving all block C addresses (variables + constants) uncorrected. Changing `local` to `global` fixed the 15-cell offset on all block C references.

This resolved the former "comparison bound points at wrong address" bug — `Cmp` now correctly references addr 5 (constant 10.0) instead of addr 20 (garbage).

### Swapped bracket search targets in Loop.hs Op 3 (PP-2)

`compWord selected shiftedI (op 4) (op 2)` had branch targets swapped — `compWord a b c d` jumps to `c` on not-equal and chains to `d` on equal, but the code put the "found" target in the not-equal position. Fixed to `compWord selected shiftedI (op 2) (op 4)`. This corrected the bracket search so it scans to the actual open bracket instead of exiting on the first K cell. Variable addresses shifted from wildly offset (16, 18, 20) to the correct data area (1, 3, 5).

## Remaining Bugs

The compiler produces the right shape (init, body, increment, comparison, post-loop, Form, Stop) and variable addresses are now in the correct data area (1, 3, 5). However, several issues remain:

- **PP-3 storage distribution**: scratch working cells at wrong addresses (691-693), un-relocated duplicate block (addrs 19-27) with out-of-range addresses (1520-1522).
- **PP-2 loop distribution**: jump target off by one (init vs body), AI/CLCC emitted unconditionally, metadata word at addr 7 leaking into the code block.
- **Cross-phase consistency**: variable `b` has address 3 in the loop body but address 2 in `Form b`.
- **Loop semantics**: potential off-by-one (`a < 10` excludes the final iteration).

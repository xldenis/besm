# Machine Code to Documentation Mapping: Subroutine Correspondence

## Executive Summary

The documentation describes I-PP-3 as having:
- **Main Program**: Three sequential parts (ops 1-13, 14-25, 26-33)
- **Four Subroutines**: Ops 34-38, 39-45, 46, 47

The machine code implements this as:
- **Three Self-Modifying Loops**: Ops 1-10, 11-41, 42-50
- **Inline Processing**: "Subroutines" are implemented inline within loops
- **Self-Modifying Pattern**: Blank instructions + init/update phases implement "subroutines" 46-47

---

## Complete Mapping Table

### LOOP 1: Operator Number Substitution

| Doc Section | Doc Ops | Machine Ops | Implementation Notes |
|-------------|---------|-------------|---------------------|
| **Main Part 1** | 1-13 | **1-10** | Machine code ends earlier (op 10 vs 13) |
| Op 1: Initialize K | 1 | 1 | ✅ Exact match |
| Op 2: Select instruction | 2 | 2 | Uses blank filled by op 1 |
| Op 3: Test for marker | 3 | 3 | ✅ Exact match |
| Ops 4-11: Address processing | 4-11 | 4-8 | **Inline implementation** |
| Op 12: Transfer back | 12 | 8 | Uses blank filled by op 1 |
| Op 13: Loop control | 13 | 9-10 | Op 9 updates, op 10 tests |
| **"Subroutine 34-38"** | 34-38 | **4-7** | **Implemented inline within Loop 1** |
| Doc 34: Test if operator number | 34 | 6 | Two comparison checks |
| Doc 35: Shift, initialize K' | 35 | 4 | Shift A₃→A₁, form selection command |
| Doc 36-37: Search for op sign | 36-37 | 5-6 | Loop with blank + test |
| Doc 38: Calculate relative addr | 38 | 7 | Compute K'-K → A₁, A₃ |

**Key Insight**: Loop 1 does **operator number → relative address** substitution. What the docs call "subroutine 34-38" is **ops 4-7 executed inline** during each iteration.

---

### LOOP 2: Opcode Classification & Relative Address Adjustment

| Doc Section | Doc Ops | Machine Ops | Implementation Notes |
|-------------|---------|-------------|---------------------|
| **Main Part 2** | 14-25 | **11-41** | Machine code much more complex |
| Op 14: Initialize | 14 | 11 | ✅ Form commands, set counter |
| Op 15: Select instruction | 15 | 12 | Uses blank filled by op 11 |
| Op 16: Test Ma/Mb | 16 | 13-18 | **Expanded to classification tree** |
| | | **13-18** | **OPCODE CLASSIFICATION TREE** (undocumented) |
| | | **19-22** | **SPECIAL HANDLERS** (undocumented, relocated) |
| Ops 17-23: Address processing | 17-23 | 23-39 | **Complex inline processing** |
| Op 24: Transfer back | 24 | 39 | Uses blank filled by op 11 |
| Op 25: Loop control | 25 | 40-41 | Op 40 updates, op 41 tests |
| **"Subroutine 39-45"** | 39-45 | **27-34** | **Implemented as nested sub-loop** |
| Doc 39: Test if relative addr | 39 | 21-24 | Routing logic determines if needed |
| Doc 40: Clear K', B | 40 | 27 | Initialize distance counter & hole counter |
| Doc 41: Select instruction | 41 | 28 | Blank selection in sub-loop |
| Doc 42: Test for marker | 42 | 29-30 | Two-stage test: order=0, is-dummy |
| Doc 43: Increment hole counter | 43 | 31 | +1 to hole counter |
| Doc 44: Repeat Δ times | 44 | 33 | Sub-loop test (K'=0?) |
| Doc 45: Form adjusted address | 45 | 34 | Calculate Δ - holes |

**Key Insight**: Loop 2 combines multiple operations:
1. Opcode classification (13-18) - **not documented**
2. Special handling (19-22) - **not documented**
3. Relative address adjustment (23-34) - **implements "subroutine 39-45" as nested sub-loop**
4. Main loop iteration (12←41)

What the docs call "subroutine 39-45" is **ops 27-34 as a nested sub-loop** within Loop 2.

---

### LOOP 3: Hole Processing & Cleanup

| Doc Section | Doc Ops | Machine Ops | Implementation Notes |
|-------------|---------|-------------|---------------------|
| **Main Part 3** | 26-33 | **42-50** | Operator numbers shifted |
| Op 26: Calculate K₁* | 26 | 42 | Also forms commands (init phase) |
| Op 27: Select instruction | 27 | 43 | Uses blank filled by op 42 |
| Op 28: Branch on type | 28 | 44-45 | Two-stage test |
| Op 29: Normal instruction | 29 | 48-49 | Transmit + update |
| Op 30: Set true address | 30 | 46a-46b | Two-part hole address calculation |
| Op 31: Print marker | 31 | 47 | Print hole information |
| Op 32: Loop control | 32 | 50 | Test K < K_f |
| Op 33: Calculate new K_f | 33 | 51 | Exit to MP-3 |

**Key Insight**: Loop 3 processes holes (gaps from removed markers). The documented structure matches reasonably well, but operator numbers are shifted.

---

## "Subroutines" 46-47: The Self-Modifying Code Pattern

### Documentation Description

**Op 46**: "subroutine for selecting instructions from block K"
**Op 47**: "subroutine for transferring instructions to block K"

The docs suggest these are callable utility subroutines used throughout.

### Machine Code Reality

These are **NOT callable subroutines**. They are **the self-modifying code pattern** itself:

| Concept | Implementation | Location |
|---------|----------------|----------|
| **"Op 46" (select)** | **BLANK instructions** that execute selections | Ops 2, 5, 12, 28, 43 |
| **"Op 47" (transfer)** | **BLANK instructions** that execute transmissions | Ops 8, 39, 48 |
| **Init phase** | Forms commands → fills blanks | Ops 1, 4, 11, 27, 42 |
| **Update phase** | Modifies commands → updates blanks | Ops 9, 32(partial), 40, 48(partial), 49 |

### The Pattern in Detail

#### Loop 1 Example:
```
Op 1 (INIT):     Forms selection cmd → fills blank in op 2
                 Forms transmission cmd → fills blank in op 8

Op 2 (EXECUTE):  <BLANK> ← executes filled selection command
                 [This IS "subroutine 46" behavior]

Op 8 (EXECUTE):  <BLANK> ← executes filled transmission command
                 [This IS "subroutine 47" behavior]

Op 9 (UPDATE):   Increments address in blank at op 2
                 Increments address in blank at op 8
                 [Prepares for next iteration]

Op 10 (TEST):    K < K_k? → loop back to op 2
```

#### Loop 2 Example:
```
Op 11 (INIT):    Forms selection cmd → fills blank in op 12
                 Forms transmission cmd → fills blank in op 39

Op 12 (EXECUTE): <BLANK> ← selection ("op 46")
Op 39 (EXECUTE): <BLANK> ← transmission ("op 47")
Op 40 (UPDATE):  Increments both blanks
Op 41 (TEST):    K < K_f? → loop to op 12
```

#### Sub-Loop Within Loop 2:
```
Op 27 (INIT):    Forms selection cmd → fills blank in op 28
Op 28 (EXECUTE): <BLANK> ← selection (nested "op 46")
Op 32 (UPDATE):  Increments blank in op 28
Op 33 (TEST):    All checked? → loop to op 28
```

### Why This Mapping Makes Sense

1. **"Subroutine 46"** (select from block K) appears **every time there's a blank that selects**
2. **"Subroutine 47"** (transfer to block K) appears **every time there's a blank that transmits**
3. The **INIT-EXECUTE-UPDATE-TEST** pattern implements the "subroutine call" behavior through self-modification
4. This explains why ops 46-47 are described as "subroutines" but don't have traditional call/return structure

### Self-Modifying Code = "Subroutine" Implementation

The documentation's "subroutines 46-47" are actually the **architectural pattern** of the entire program:

| Documentation Concept | Machine Code Reality |
|----------------------|---------------------|
| "Call op 46" | Execute a BLANK instruction filled with selection command |
| "Call op 47" | Execute a BLANK instruction filled with transmission command |
| "Subroutine entry" | INIT phase fills blank with command |
| "Subroutine execution" | Blank executes as intended instruction |
| "Subroutine for next iteration" | UPDATE phase modifies blank's address |

---

## Complete Subroutine Correspondence Summary

| Doc Subroutine | Doc Ops | Machine Implementation | Type |
|----------------|---------|----------------------|------|
| **Subroutine 34-38**: Form relative addresses | 34-38 | **Ops 4-7** inline in Loop 1 | Inline code |
| **Subroutine 39-45**: Change relative addresses | 39-45 | **Ops 27-34** sub-loop in Loop 2 | Nested loop |
| **Subroutine 46**: Select from block K | 46 | **BLANK pattern** (ops 2, 5, 12, 28, 43) | Self-modifying |
| **Subroutine 47**: Transfer to block K | 47 | **BLANK pattern** (ops 8, 39, 48) | Self-modifying |

---

## The Three-Loop Structure vs. Three-Part Structure

### Documentation: Three Sequential Parts

```
PART 1 (Ops 1-13): Substitute operator numbers
  ├─ Main loop: ops 2←13
  └─ Calls: Subroutine 34-38

PART 2 (Ops 14-25): Change relative addresses
  ├─ Main loop: ops 15←25
  └─ Calls: Subroutine 39-45

PART 3 (Ops 26-33): Remove loop markers
  ├─ Main loop: ops 27←32
  └─ Calls: None

UTILITIES (Ops 46-47): Block K operations
  ├─ Op 46: Select from K
  └─ Op 47: Transfer to K
```

### Machine Code: Three Self-Modifying Loops

```
LOOP 1 (Ops 1-10): Operator number substitution
  ├─ INIT (op 1): Fill blanks in ops 2, 8
  ├─ BODY (ops 2-8): Select→Process→Transmit
  │   ├─ Op 2: <BLANK> selection
  │   ├─ Ops 4-7: Inline "sub 34-38"
  │   └─ Op 8: <BLANK> transmission
  ├─ UPDATE (op 9): Modify blanks
  └─ TEST (op 10): K < K_k → op 2

LOOP 2 (Ops 11-41): Opcode classification + address adjustment
  ├─ INIT (op 11): Fill blanks in ops 12, 39
  ├─ SELECT (op 12): <BLANK>
  ├─ CLASSIFY (ops 13-18): Route by opcode [UNDOCUMENTED]
  ├─ SPECIAL (ops 19-22): Handle conditionals [UNDOCUMENTED]
  ├─ PROCESS (ops 23-26): Address formation
  ├─ SUB-LOOP (ops 27-34): Inline "sub 39-45"
  │   ├─ INIT (op 27): Fill blank in op 28
  │   ├─ BODY (ops 28-31): Count holes
  │   ├─ UPDATE (op 32): Modify blank
  │   └─ TEST (op 33): Done? → op 34
  ├─ REPLACE (ops 35-39): Update addresses
  │   └─ Op 39: <BLANK> transmission
  ├─ UPDATE (op 40): Modify blanks in ops 12, 39
  └─ TEST (op 41): K < K_f → op 12

LOOP 3 (Ops 42-50): Hole processing
  ├─ INIT (op 42): Fill blanks in ops 43, 48
  ├─ BODY (ops 43-47): Process holes
  │   ├─ Op 43: <BLANK> selection
  │   └─ Op 48: <BLANK> transmission (self-modifying)
  ├─ UPDATE (op 49): Modify blank in op 43
  └─ TEST (op 50): K < K_f → op 43

EXIT (Op 51): Calculate new K_k, exit to MP-3
```

---

## Why Machine Code Op 40 Was Hard to Map

### Original Question: What does machine code op 40 correspond to in documentation?

**Machine Code Op 40:**
```
ai   031e 10b7 031e     Increment blank in op 12 (selection)
ai   034b 10b9 034b     Increment blank in op 39 (transmission)
```

**Documentation Op 40** (in subroutine 39-45):
> "Clear counter K' and standard cell B"

### These are COMPLETELY DIFFERENT

**Why the confusion:**

1. **Documentation Op 40** is part of "subroutine 39-45" (changing relative addresses)
   - Should clear counters before counting holes
   - Actually implemented as **machine code op 27** (sub-loop initialization)

2. **Machine Code Op 40** is the **UPDATE phase** of Loop 2
   - Modifies self-modifying code (blanks) to advance to next instruction
   - No direct correspondence in documentation (it's infrastructure)

3. **Documentation doesn't explicitly describe UPDATE phases**
   - Treats iteration as implicit
   - Machine code must explicitly modify blanks for next iteration

### Correct Mapping

| Documentation Element | Machine Code Implementation |
|----------------------|---------------------------|
| **Doc Op 40**: "Clear K' and B" | **Machine Op 27**: Initialize sub-loop counters |
| **Machine Op 40**: Update loop iteration | **No direct doc correspondence** (implicit) |

Machine code op 40 is **loop infrastructure** (maintains self-modifying code), not an algorithmic step that would appear in high-level documentation.

---

## Conceptual vs. Implementation Gap

### What Documentation Describes (Algorithmic Level)

```
FOR each instruction:
    Extract addresses
    FOR each address:
        IF address is operator number:
            CALL form_relative_address(operator_num)
        END IF
    END FOR
    Store instruction back
END FOR
```

### What Machine Code Does (Implementation Level)

```
// INIT: Set up self-modifying code
form_selection_command() -> fills blank_instruction_A
form_transmission_command() -> fills blank_instruction_B

// BODY: Execute with self-modifying code
LOOP WHILE counter < limit:
    execute blank_instruction_A        // This IS "call subroutine 46"
    process_selected_instruction()
    execute blank_instruction_B        // This IS "call subroutine 47"

    modify blank_instruction_A         // Advance to next address
    modify blank_instruction_B         // Advance to next address
    counter++
END LOOP
```

The documentation describes **WHAT** happens (select, process, store).
The machine code shows **HOW** it's done (self-modifying blanks + init/update phases).

---

## Summary: The Four "Subroutines"

### 1. Subroutine 34-38 (Form Relative Addresses)
- **Documentation**: Callable subroutine, converts operator numbers to relative addresses
- **Machine Code**: **Inline ops 4-7** within Loop 1
- **Pattern**: Sequential code within loop body

### 2. Subroutine 39-45 (Change Relative Addresses)
- **Documentation**: Callable subroutine, adjusts addresses by counting removed markers
- **Machine Code**: **Nested sub-loop ops 27-34** within Loop 2
- **Pattern**: Sub-loop with own INIT-BODY-UPDATE-TEST

### 3. Subroutine 46 (Select from Block K)
- **Documentation**: Utility subroutine for selection
- **Machine Code**: **BLANK instructions** at ops 2, 5, 12, 28, 43
- **Pattern**: Self-modifying code filled by INIT, modified by UPDATE

### 4. Subroutine 47 (Transfer to Block K)
- **Documentation**: Utility subroutine for transmission
- **Machine Code**: **BLANK instructions** at ops 8, 39, 48
- **Pattern**: Self-modifying code filled by INIT, modified by UPDATE

---

## Implications for Understanding I-PP-3

### 1. Self-Modifying Code is Central
The entire program architecture revolves around **BLANK instructions** that get filled and modified. This is not a quirk—it's the **core implementation pattern**.

### 2. "Subroutines" Are Patterns, Not Functions
When documentation says "calls subroutine X," it means:
- **Subroutines 34-38, 39-45**: Execute inline code or sub-loop
- **Subroutines 46-47**: Execute self-modifying blank

There are no traditional function calls with call stacks.

### 3. Documentation Describes Algorithm, Not Implementation
The documentation is a **logical description** of the algorithm, not a **literal description** of the code. It abstracts away:
- Self-modifying code mechanics
- INIT/UPDATE phases
- Loop infrastructure (like machine op 40)
- Opcode classification tree

### 4. Machine Code is More Complex
The actual implementation has:
- **Opcode classification** (ops 13-18) - undocumented
- **Special handlers** (ops 19-22) - undocumented
- **Self-modifying pattern** - abstracted as "subroutines"
- **Four operator numbers** (48-51) - completely undocumented

### 5. Best Way to Map Documentation to Machine Code

Read documentation for **WHAT the algorithm does**:
- Convert operator numbers to relative addresses
- Adjust addresses when markers removed
- Process holes in program

Read machine code for **HOW it's implemented**:
- Three self-modifying loops
- Inline and nested processing
- Opcode-based routing
- Self-modifying BLANK pattern

Use this mapping to bridge the two views.

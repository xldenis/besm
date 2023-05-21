# besm-vm: a vm for the BESM computer

A vm for the BESM with debugging capabilities (not yet).

### Usage

1. Install `rustup`
2. Run `cargo run [BINARY-IS-BINARY-FILE] --md{0,1,2,3,4}=[BINARY-MD-FILE] --start-address=X`
3. успех!

#### Obtaining Binary Files

The VM expects as input the binary representation of each input file. The `pp-besm` and `compile-pp` programs both produce a hex representation of the binary. To actually turn it into binary use the `./to_binary.sh` script. Several programs are provided in `test_progs`, they are meant as input for the compiler program are meant to be mounted on `MD-2`.

#### Example Invocation / Calling the compiler

To actually run the compiler use the following command:

```
cargo run --release -- run --bootloader=boot.txt --md0=mp1.txt --md4=mp2.txt --start-address=1025 --md2=test_progs/variable_address.txt
```

### Implemented instructions:

legend: ✅ = working instruction, 🐛 = instruction has known bugs, ❌ = instruction is not implemented.

| op name  | ✅? |
|:---------|:----|
| Add      | ✅ |
| Sub      | ✅ |
| Mult     | ❌ |
| Div      | ❌ |
| AddE     | ✅ |
| SubE     | ✅ |
| Ce       | ✅ |
| Xa       | ❌ |
| Xb       | ❌ |
| DivA     | ❌ |
| DivB     | ❌ |
| TN       | ✅ |
| PN       | ❌ |
| TMin     | ❌ |
| TMod     | ❌ |
| TSign    | ✅ |
| TExp     | ✅ |
| Shift    | 🐛 |
| ShiftAll | ✅ |
| AI       | ✅ |
| AICarry  | ✅ |
| I        | ✅ |
| Comp     | ✅ |
| CompWord | ✅ |
| CompMod  | ✅ |
| Ma       | 🐛 |
| Mb       | 🐛 |
| JCC      | ✅ |
| CLCC     | ✅ |
| CCCC     | ✅ |
| Stop28   | ✅ |
| LogMult  | ✅ |
| Stop     | ✅ |

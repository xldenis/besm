use crate::vm::instruction::Instruction::*;
use bit_field::BitField;

type Address = u16;

use crate::vm::VMError;

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
    Add { a: Address, b: Address, c: Address, normalize: bool },
    Sub { a: Address, b: Address, c: Address, normalize: bool },
    Mult { a: Address, b: Address, c: Address, normalize: bool },
    Div { a: Address, b: Address, c: Address, normalize: bool },
    AddE { a: Address, b: Address, c: Address, normalize: bool },
    SubE { a: Address, b: Address, c: Address, normalize: bool },
    Ce { a: Address, b: Address, c: Address, normalize: bool },
    Xa { a: Address, b: Address, c: Address, normalize: bool },
    Xb { c: Address, normalize: bool },
    DivA { a: Address, b: Address, c: Address, normalize: bool },
    DivB { c: Address, normalize: bool },
    TN { a: Address, c: Address, normalize: bool },
    PN { a: Address },
    TMin { a: Address, c: Address, normalize: bool },
    TMod { a: Address, c: Address, normalize: bool },
    TSign { a: Address, b: Address, c: Address, normalize: bool },
    TExp { a: Address, c: Address, normalize: bool },
    Shift { a: Address, b: Address, c: Address },
    ShiftAll { a: Address, b: Address, c: Address },
    AI { a: Address, b: Address, c: Address },
    AICarry { a: Address, b: Address, c: Address },
    I { a: Address, b: Address, c: Address },
    Comp { a: Address, b: Address, c: Address },
    CompWord { a: Address, b: Address, c: Address },
    CompMod { a: Address, b: Address, c: Address },
    Ma { a: Address, b: Address, c: Address },
    Mb { b: Address },
    JCC,
    CLCC { c: Address },
    CCCC { b: Address, c: Address },
    Stop28,
    LogMult { a: Address, b: Address, c: Address },
    Stop,
}

pub fn first_addr(x: u64) -> u16 {
    x.get_bits(22..33) as u16
}

pub fn second_addr(x: u64) -> u16 {
    x.get_bits(11..22) as u16
}

pub fn third_addr(x: u64) -> u16 {
    x.get_bits(0..11) as u16
}

impl Instruction {
    // Eventually this should be Result<(), Instruction>
    pub fn from_bytes(word: u64) -> Result<Instruction, VMError> {
        let instr = match word.get_bits(33..38) {
            0x01 => Add {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x02 => Sub {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x03 => Mult {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x04 => Div {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x05 => AddE {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x06 => SubE {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x07 => Ce {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x08 => Xa {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x09 => Xb { normalize: !word.get_bit(38), c: third_addr(word) },
            0x0A => DivA {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x0B => DivB { normalize: !word.get_bit(38), c: third_addr(word) },
            0x0C => {
                let second_addr = second_addr(word);

                if second_addr == 0x200 {
                    PN { a: first_addr(word) }
                } else {
                    TN { normalize: !word.get_bit(38), a: first_addr(word), c: third_addr(word) }
                }
            }
            0x0D => TMin { normalize: !word.get_bit(38), a: first_addr(word), c: third_addr(word) },
            0x0E => TMod { normalize: !word.get_bit(38), a: first_addr(word), c: third_addr(word) },
            0x0F => TSign {
                normalize: !word.get_bit(38),
                a: first_addr(word),
                b: second_addr(word),
                c: third_addr(word),
            },
            0x10 => TExp { normalize: !word.get_bit(38), a: first_addr(word), c: third_addr(word) },
            0x11 => match word.get_bit(38) {
                false => Shift { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
                true => ShiftAll { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            },
            0x12 => match word.get_bit(38) {
                false => AI { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
                true => AICarry { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            },
            0x13 => I { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            0x14 => match word.get_bit(38) {
                false => Comp { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
                true => CompWord { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            },
            0x15 => CompMod { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            0x16 => Ma { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            0x17 => Mb { b: second_addr(word) },
            0x19 => JCC {},
            0x1A => CLCC { c: third_addr(word) },
            0x1B => CCCC { b: second_addr(word), c: third_addr(word) },
            0x1C => Stop28 {},
            0x1D => LogMult { a: first_addr(word), b: second_addr(word), c: third_addr(word) },
            0x1F => Stop {},
            _ => return Err(VMError::InvalidInstruction(word)),
        };

        Ok(instr)
    }
}

use std::fmt;

macro_rules! denormed {
    ($normed:expr, $nm:expr) => {
        format!("{}{}", if $normed { "" } else { "," }, $nm)
    };
}

pub fn get_instruction_parts(
    instr: &Instruction,
) -> (String, Option<u16>, Option<u16>, Option<u16>) {
    use Instruction::*;
    match instr {
        Add { a, b, c, normalize } => (denormed!(*normalize, "Add"), Some(*a), Some(*b), Some(*c)),
        Sub { a, b, c, normalize } => (denormed!(*normalize, "Sub"), Some(*a), Some(*b), Some(*c)),
        Mult { a, b, c, normalize } => {
            (denormed!(*normalize, "Mult"), Some(*a), Some(*b), Some(*c))
        }
        Div { a, b, c, normalize } => (denormed!(*normalize, "Div"), Some(*a), Some(*b), Some(*c)),
        AddE { a, b, c, normalize } => {
            (denormed!(*normalize, "AddE"), Some(*a), Some(*b), Some(*c))
        }
        SubE { a, b, c, normalize } => {
            (denormed!(*normalize, "SubE"), Some(*a), Some(*b), Some(*c))
        }
        Ce { a, b, c, normalize } => (denormed!(*normalize, "Ce"), Some(*a), Some(*b), Some(*c)),
        Xa { a, b, c, normalize } => (denormed!(*normalize, "Xa"), Some(*a), Some(*b), Some(*c)),
        Xb { c, normalize } => (denormed!(*normalize, "Xb"), None, None, Some(*c)),
        DivA { a, b, c, normalize } => {
            (denormed!(*normalize, "DivA"), Some(*a), Some(*b), Some(*c))
        }
        DivB { c, normalize } => (denormed!(*normalize, "DivB"), None, None, Some(*c)),
        TN { a, c, normalize } => (denormed!(*normalize, "TN"), Some(*a), None, Some(*c)),
        PN { a } => (denormed!(true, "PN"), Some(*a), None, None),
        TMin { a, c, normalize } => (denormed!(*normalize, "TMin"), Some(*a), None, Some(*c)),
        TMod { a, c, normalize } => (denormed!(*normalize, "TMod"), Some(*a), None, Some(*c)),
        TSign { a, b, c, normalize } => {
            (denormed!(*normalize, "TSgn"), Some(*a), Some(*b), Some(*c))
        }
        TExp { a, c, normalize } => (denormed!(*normalize, "TExp"), Some(*a), None, Some(*c)),
        Shift { a, b, c } => (denormed!(true, "Shft"), Some(*a), Some(*b), Some(*c)),
        ShiftAll { a, b, c } => (denormed!(false, "Shft"), Some(*a), Some(*b), Some(*c)),
        AI { a, b, c } => (denormed!(true, "AI"), Some(*a), Some(*b), Some(*c)),
        AICarry { a, b, c } => (denormed!(false, "AI"), Some(*a), Some(*b), Some(*c)),
        I { a, b, c } => (denormed!(true, "I"), Some(*a), Some(*b), Some(*c)),
        Comp { a, b, c } => (denormed!(true, "Cmp"), Some(*a), Some(*b), Some(*c)),
        CompWord { a, b, c } => (denormed!(true, "CmpWd"), Some(*a), Some(*b), Some(*c)),
        CompMod { a, b, c } => (denormed!(true, "CmpMd"), Some(*a), Some(*b), Some(*c)),
        Ma { a, b, c } => (denormed!(true, "Ma"), Some(*a), Some(*b), Some(*c)),
        Mb { b } => (denormed!(true, "Mb"), None, Some(*b), None),
        JCC => (denormed!(true, "JCC"), None, None, None),
        CLCC { c } => (denormed!(true, "CLCC"), None, None, Some(*c)),
        CCCC { b, c } => (denormed!(true, "CCCC"), None, Some(*b), Some(*c)),
        Stop28 => (denormed!(true, "Stp28"), None, None, None),
        LogMult { a, b, c } => (denormed!(true, "BitMl"), Some(*a), Some(*b), Some(*c)),
        Stop => (denormed!(true, "Stop"), None, None, None),
    }
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (op, a, b, c) = get_instruction_parts(self);

        write!(f, "{op:<5} ")?;
        match a {
            Some(a) => write!(f, "{a:4} ")?,
            None => write!(f, "{:4} ", "")?,
        };

        match b {
            Some(a) => write!(f, "{a:4} ")?,
            None => write!(f, "{:4} ", "")?,
        };

        match c {
            Some(a) => write!(f, "{a:4}")?,
            None => write!(f, "{:4}", "")?,
        };

        Ok(())
    }
}

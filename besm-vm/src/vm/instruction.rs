use bit_field::BitField;
use crate::vm::instruction::Instruction::*;

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
            0x0C => TN { normalize: !word.get_bit(38), a: first_addr(word), c: third_addr(word) },
            0x2C => PN { a: first_addr(word) },
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

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        macro_rules! denormed {
            ($normed:expr, $nm:expr) => {
                format!("{}{}", if $normed { "" } else { "," }, $nm)
            };
        }

        match self {
            Add { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Add"), a, b, c)
            }
            Sub { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Sub"), a, b, c)
            }
            Mult { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Mult"), a, b, c)
            }
            Div { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Div"), a, b, c)
            }
            AddE { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "AddE"), a, b, c)
            }
            SubE { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "SubE"), a, b, c)
            }
            Ce { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Ce"), a, b, c)
            }
            Xa { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Xa"), a, b, c)
            }
            Xb { c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Xb"), "", "", c)
            }
            DivA { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "DivA"), a, b, c)
            }
            DivB { c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "DivB"), "", "", c)
            }
            TN { a, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TN"), a, "", c)
            }
            PN { a } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "PN"), a, "", ""),
            TMin { a, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TMin"), a, "", c)
            }
            TMod { a, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TMod"), a, "", c)
            }
            TSign { a, b, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TSgn"), a, b, c)
            }
            TExp { a, c, normalize: norm } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TExp"), a, "", c)
            }
            Shift { a, b, c } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "Shft"), a, b, c)
            }
            ShiftAll { a, b, c } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(false, "Shft"), a, b, c)
            }
            AI { a, b, c } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "AI"), a, b, c),
            AICarry { a, b, c } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(false, "AI"), a, b, c)
            }
            I { a, b, c } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "I"), a, b, c),
            Comp { a, b, c } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "Cmp"), a, b, c),
            CompWord { a, b, c } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "CmpWd"), a, b, c)
            }
            CompMod { a, b, c } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "CmpMd"), a, b, c)
            }
            Ma { a, b, c } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "Ma"), a, b, c),
            Mb { b } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "Mb"), "", b, ""),
            JCC => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "JCC"), "", "", ""),
            CLCC { c } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "CLCC"), "", "", c),
            CCCC { b, c } => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "CCCC"), "", b, c),
            Stop28 => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "Stp28"), "", "", ""),
            LogMult { a, b, c } => {
                write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "BitMl"), a, b, c)
            }
            Stop => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true, "Stop"), "", "", ""),
        }
    }
}

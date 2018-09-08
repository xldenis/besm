use bit_field::BitField;
use num;
use std::cmp::*;

#[derive(Debug, Copy, Clone)]
pub struct Float {
    pub mant: u32, // actually only the lower 32 bits are used (u64 lets us store overflow  though...)
    pub overflow: bool,
    pub exp: i8, // actually is only 6 bits long aka (-32 - 31)
    pub sign: bool,
}

macro_rules! equalize_exponents_shift_mant {
    ($left:expr, $right:expr, $left_mant:ident, $right_mant:ident, $exp:ident) => {
        let mut $left_mant: u32 = $left.mant;
        let mut $right_mant: u32 = $right.mant;
        let left_exp: i8 = $left.exp;
        let right_exp: i8 = $right.exp;

        let $exp = match left_exp.cmp(&right_exp) {
            Ordering::Less => {
                $left_mant = $left_mant
                    .checked_shr((left_exp - right_exp) as u32)
                    .unwrap_or(0);
                right_exp
            }
            Ordering::Greater => {
                $right_mant = $right_mant
                    .checked_shr((left_exp - right_exp) as u32)
                    .unwrap_or(0);
                left_exp
            }
            Ordering::Equal => left_exp,
        };
    };
}

impl Float {
    pub fn from_bytes(word: u64) -> Float {
        let exp = (0xFF & word.get_bits(33..39)) as i8;
        let mant = word.get_bits(0..33) as u32;
        let sign = word.get_bit(32);
        Float {
            mant,
            exp,
            sign: sign,
            overflow: false,
        }
    }

    pub fn new(mant: u32, exp: i8) -> Float {
        Float {
            mant,
            exp,
            overflow: false,
            sign: false,
        }
    }

    pub fn to_bytes(&mut self) -> u64 {
        let mut bytes: u64 = 0;
        bytes.set_bits(0..32, self.mant.into());
        bytes.set_bit(32, self.sign);
        *bytes.set_bits(33..39, (self.exp & 0b111111) as u64) // oh boy is this correct?
    }

    pub fn normalize(&mut self) {
        if self.overflow {
            self.exp += 1;
            self.mant >>= 1;
            self.mant.set_bit(31, true);
            self.overflow = false
        } else if self.mant == 0 && self.exp == -32 {
            ()
        } else {
            let mut shift = 0;
            while self.mant.get_bit(31) == false && shift < 32 {
                shift += 1;
                self.mant <<= 1;
            }

            self.exp = num::clamp(self.exp - shift, -32, 31);
        }
    }

    pub fn add_unnormalized(&self, other: &Float) -> Float {
        equalize_exponents_shift_mant!(self, other, left_mant, right_mant, exp);

        let (res_mant, overflow) = left_mant.overflowing_add(right_mant);
        let res_sign = match left_mant.cmp(&right_mant) {
            Ordering::Less => other.sign,
            Ordering::Greater => self.sign,
            Ordering::Equal => false,
        };

        Float {
            mant: res_mant,
            overflow: overflow,
            sign: res_sign,
            exp,
        }
    }

    pub fn sub_unnormalized(&self, other: &Float) -> Float {
        equalize_exponents_shift_mant!(self, other, left_mant, right_mant, exp);

        let (res_mant, overflow) = left_mant.overflowing_sub(right_mant);
        let res_sign = match overflow {
            true => !other.sign,
            false => self.sign,
        };

        Float {
            mant: res_mant,
            overflow: false,
            sign: res_sign,
            exp,
        }
    }

    // what is un normalized floating point multiplication?
    pub fn mul_unnormalized(&self, other: &Float) -> Float {
        equalize_exponents_shift_mant!(self, other, left_mant, right_mant, exp_half);

        let exp: i8 = exp_half + exp_half;

        let res_mant = u64::from(left_mant) * u64::from(right_mant);

        Float {
            mant: res_mant.get_bits(32..64) as u32,
            overflow: false,
            sign: self.sign ^ other.sign,
            exp,
        }
    }
}

use std::fmt;

impl fmt::Display for Float {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let num = 2.0f64.powf(self.exp as f64 - 33.0) * (self.mant as f64);
        write!(f, "{}", num)
    }
}

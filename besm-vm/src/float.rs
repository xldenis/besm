use bit_field::BitField;
use num;
use std::cmp::*;

#[derive(Debug, Copy, Clone, PartialEq)]
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
                $left_mant = $left_mant.checked_shr((right_exp - left_exp) as u32).unwrap_or(0);
                right_exp
            }
            Ordering::Greater => {
                $right_mant = $right_mant.checked_shr((left_exp - right_exp) as u32).unwrap_or(0);
                left_exp
            }
            Ordering::Equal => left_exp,
        };
    };
}

impl Float {
    pub fn from_bytes(word: u64) -> Float {
        // The exponent is 6-bit two's complement (-32..=31).
        let e = word.get_bits(33..39) as i8;
        let exp = if e >= 32 { e - 64 } else { e };
        let mant = word.get_bits(0..33) as u32;
        let sign = word.get_bit(32);
        Float { mant, exp, sign: sign, overflow: false }
    }

    pub fn new(mant: u32, exp: i8) -> Float {
        Float { mant, exp, overflow: false, sign: false }
    }

    // Normalized float for a signed integer.
    pub fn from_int(n: i32) -> Float {
        let mut x = n.unsigned_abs();
        let mut exp = 0;
        let mut mant = 0 << 31;

        while x > 0 {
            mant >>= 1;
            mant |= (x % 2) << 31;

            exp += 1;
            x /= 2;
        }

        let mut float = Float::new(mant, exp);
        float.sign = n < 0;
        float
    }

    pub fn to_bytes(&mut self) -> u64 {
        let mut bytes: u64 = 0;
        bytes.set_bits(0..32, self.mant.into());
        bytes.set_bit(32, self.sign);
        *bytes.set_bits(33..39, (self.exp & 0b111111) as u64) // oh boy is this correct?
    }

    pub fn to_float(&self) -> f64 {
        2.0f64.powf(self.exp as f64 - 32.0) * (self.mant as f64)
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

    // Sign-magnitude addition (p. 154)
    pub fn add_unnormalized(&self, other: &Float) -> Float {
        equalize_exponents_shift_mant!(self, other, left_mant, right_mant, exp);

        if self.sign == other.sign {
            let (mant, overflow) = left_mant.overflowing_add(right_mant);
            return Float { mant, overflow, sign: self.sign, exp };
        }

        match left_mant.cmp(&right_mant) {
            Ordering::Greater => Float { mant: left_mant - right_mant, overflow: false, sign: self.sign, exp },
            Ordering::Less => Float { mant: right_mant - left_mant, overflow: false, sign: other.sign, exp },
            Ordering::Equal => Float { mant: 0, overflow: false, sign: false, exp },
        }
    }

    pub fn sub_unnormalized(&self, other: &Float) -> Float {
        let mut negated = *other;
        negated.sign = !other.sign;
        self.add_unnormalized(&negated)
    }

    /// Whole and fractional parts of |x| as 32-bit mantissas, for E (013).
    pub fn integer_and_fraction(&self) -> (u32, u32) {
        if self.exp <= 0 {
            (0, self.mant.checked_shr((-(self.exp as i32)) as u32).unwrap_or(0))
        } else {
            let e = self.exp as u32; // 1..=31
            (self.mant >> (32 - e), self.mant << e)
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
        let num = self.to_float();

        let string = match f.precision() {
            Some(prec) => format!("{:.*}", prec, num),
            None => format!("{}", num),
        };

        f.pad_integral(true, "", &string)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_int_to_int() {
        assert_eq!(Float::from_int(4).to_float(), 4f64);
        assert_eq!(Float::from_int(16).to_float(), 16f64);
        assert_eq!(Float::from_int(17).to_float(), 17f64);
    }

    #[test]
    fn exponent_is_twos_complement() {
        // 9_214: `,+ 0200 0000 0001` is 2^-31 (2^-1 + 2^-32), exponent field 100001 = -31
        assert_eq!(Float::from_bytes(0x4280000001).exp, -31);
        assert_eq!(Float::from_bytes(0x4000000000).exp, -32); // normalized zero
        assert_eq!(Float::from_bytes(0x3e00000000).exp, 31);
        assert_eq!(Float::from_bytes(0x7e00000000).exp, -1);
        // round trip
        assert_eq!(Float::from_bytes(0x4280000001).to_bytes(), 0x4280000001);
    }

    #[test]
    fn signed_addition() {
        let int = |n: i32| {
            let mut f = Float::new(n.unsigned_abs(), 0);
            f.sign = n < 0;
            f
        };
        assert_eq!(int(3).sub_unnormalized(&int(5)).to_bytes(), int(-2).to_bytes());
        assert_eq!(int(0).sub_unnormalized(&int(5)).to_bytes(), int(-5).to_bytes());
        assert_eq!(int(-3).add_unnormalized(&int(2)).to_bytes(), int(-1).to_bytes());
        assert_eq!(int(-3).sub_unnormalized(&int(2)).to_bytes(), int(-5).to_bytes());
        assert_eq!(int(4).add_unnormalized(&int(-4)).to_bytes(), 0);
    }

    #[test]
    fn from_int_negative() {
        // ↓ of a `,+` instruction: П = -31 is -0.96875 · 2^5
        assert_eq!(Float::from_int(-31).to_bytes(), 0x0bf8000000);
        assert_eq!(Float::from_int(-32).to_bytes(), 0x0d80000000);
        assert_eq!(Float::from_int(31).to_bytes(), 0x0af8000000);
    }

    #[test]
    fn add_one_and_two() {
        let one = Float::new(1 << 31, 1);
        let two = Float::new(1 << 31, 2);
        let three = Float::new(0b11 << 30, 2);
        let mut added = one.add_unnormalized(&two);
        added.normalize();
        assert_eq!(three, added);
    }

    #[test]
    fn add_two_and_two() {
        let two = Float::new(1 << 31, 2);
        let four = Float::new(1 << 31, 3);
        let mut added = two.add_unnormalized(&two);
        added.normalize();
        assert_eq!(four, added);
    }
}

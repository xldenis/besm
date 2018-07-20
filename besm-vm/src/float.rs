use bit_field::BitField;
use num;
use std::cmp::*;

#[derive(Debug, Copy, Clone)]
pub struct Float {
  pub mant: u32, // actually only the lower 32 bits are used (u64 lets us store overflow  though...)
  pub overflow: bool,
  pub exp: i8, // actually is only 6 bits long aka (-32 - 31)
  pub sign: bool
}

macro_rules! equalize_exponents {
  ($left_mant:ident, $left_exp:ident, $right_mant:ident, $right_exp:ident) => {{
    match $left_exp.cmp(&$right_exp) {
      Ordering::Less    => {
        $left_mant = $left_mant.checked_shr(($left_exp - $right_exp) as u32).unwrap_or(0);
        $left_exp = $right_exp;
      }
      Ordering::Greater => {
        $right_mant = $right_mant.checked_shr(($right_exp - $left_exp) as u32).unwrap_or(0);
        $right_exp = $left_exp;
      }
      Ordering::Equal => { }
    }
  }};
}

impl Float {
  pub fn from_bytes(word: u64) -> Float {
    let exp = (0xFF & word.get_bits(33..38)) as i8;
    let mant = word.get_bits(0..32) as u32;
    let sign = word.get_bit(32);
    println!("{:039b}", word);
    println!("{:06b} {:?} {:032b}", exp, sign, mant);
    Float { mant: mant, exp: exp, sign: sign, overflow: false }
  }

  pub fn to_bytes(&mut self) -> u64 {
    let mut bytes : u64 = 0;
    bytes.set_bits(0..32, self.mant.into());
    bytes.set_bit(32, self.sign);
    *bytes.set_bits(33..38, self.exp as u64) // oh boy is this correct?
  }

  pub fn normalize(&mut self) {
    if self.overflow {
      self.exp += 1;
      self.mant >>= 1;
      self.mant.set_bit(31, true);
      self.overflow = false
    } else {
      let mut shift = 0;
      while self.mant.get_bit(31) == false {

        shift += 1;
        self.mant <<= 1;
      }

      self.exp  = num::clamp(self.exp - shift, -32,  31);
    }
  }

  pub fn add_unnormalized(&self, other: &Float) -> Float {
    let mut left_mant = self.mant;
    let mut right_mant = other.mant;
    let mut left_exp = self.exp;
    let mut right_exp = other.exp;

    println!("{:032b} {:032b} {:06b} {:06b}", left_mant, right_mant, left_exp, right_exp);
    equalize_exponents!(left_exp, left_mant, right_exp, right_mant);
    println!("{:032b} {:032b} {:06b} {:06b}", left_mant, right_mant, left_exp, right_exp);

    let exp = right_exp;

    let (res_mant, overflow) = left_mant.overflowing_add(right_mant);
    let res_sign = match left_mant.cmp(&right_mant) {
      Ordering::Less => other.sign,
      Ordering::Greater => self.sign,
      Ordering::Equal   => false,
    };

    Float { mant: res_mant, overflow: overflow, sign: res_sign, exp: exp }
  }

  pub fn sub_unnormalized(&self, other: &Float) -> Float {
    let mut left_mant = self.mant;
    let mut right_mant = other.mant;
    let mut left_exp = self.exp;
    let mut right_exp = other.exp;

    println!("{:032b} {:032b} {:06b} {:06b}", left_mant, right_mant, left_exp, right_exp);
    equalize_exponents!(left_exp, left_mant, right_exp, right_mant);
    println!("{:032b} {:032b} {:06b} {:06b}", left_mant, right_mant, left_exp, right_exp);

    let exp = right_exp;

    let (res_mant, overflow) = left_mant.overflowing_sub(right_mant);
    let res_sign = match overflow {
      true  => !other.sign,
      false => self.sign,
    };

    Float { mant: res_mant, overflow: false, sign: res_sign, exp: exp }

  }
}

use float::*;
use bit_field::BitField;

mod ds;
pub mod mag;
pub mod instruction;
use vm::mag::*;

pub struct VM<'a> {
  pub memory: &'a mut Memory,
  local_ic: u16,
  global_ic: u16,
  active_ic: ActiveIC,
  pub stopped: bool,
  pub mag_system: MagSystem<'a>
}

pub struct Memory {
  is: [u64; 1023],
}

impl Memory {
  pub fn new(is: [u64; 1023]) -> Memory {
    Memory { is: is}
  }
  pub fn get(&self, ix: u16) -> Result<u64, VMError> {
    if ix == 0 {
      Ok(0)
    } else if ix <= 1023 {
      Ok(self.is[(ix - 1) as usize])
    } else if ix == 1024 {
      Ok(1024)
    } else if 1024 < ix && ix <= 1408 { // The DS annoyingly starts on address 1025 not 1024 :rage:
      Ok(ds::DS[(ix - 1023 - 1 - 1) as usize]) // We want address 1025 to map to index 0
    } else {
      Err(VMError::OutOfBounds{})
    }
  }

  pub fn get_mut(&mut self, ix: u16) -> Result<&mut u64, VMError> {
    if ix <= 1023 {
      Ok(&mut self.is[(ix - 1) as usize])
    } else {
      Err(VMError::OutOfBounds{})
    }
  }

  fn set(&mut self, ix: u16, val: u64) -> Result<&mut Self, VMError> {
    if ix == 0 {
      return Err(VMError::OutOfBounds{})
    } else if ix <= 1023 {
      self.is[(ix - 1) as usize].set_bits(0..39, val.get_bits(0..39));

    } else {
      panic!("can't assign to DS")
    }
    Ok(self)
  }

  fn iter_mut(&mut self) -> slice::IterMut<u64> {
    self.is.iter_mut()
  }
}

pub struct Iter<'a> {
  mem: &'a Memory,
  index: usize,
}

use std::slice;

impl <'a>IntoIterator for &'a Memory {
  type Item = u64;
  type IntoIter = Iter<'a>;

  fn into_iter(self) -> Self::IntoIter {
    Iter { mem: self, index: 1 }
  }
}

impl < 'a>Iterator for Iter< 'a> {
  type Item = u64;

  fn next(&mut self) -> Option<u64> {
    match self.mem.get(self.index as u16) {
      Err(_)   => return None,
      Ok(item) => {
        self.index += 1;

        Some(item)
      }
    }
  }
}

enum ActiveIC { Global, Local}

#[derive(Debug)]
pub enum VMError {
  OutOfBounds { },
  BadDriveOperation,
  InvalidInstruction(u64),
  NotYetImplemented(Instruction),
  DriveErr(mag::DriveError),
}

use vm::instruction::*;
use vm::instruction::Instruction::*;

impl<'a> VM<'a> {
  pub fn new(is: &'a mut Memory, drives: &'a mut [MagDrive; 5], tapes: &'a mut [MagTape; 4], start: u16) -> VM<'a> {
    VM {
      memory: is,
      global_ic: start,
      local_ic: 1,
      active_ic: ActiveIC::Global,
      stopped: false,
      mag_system: MagSystem {
        mag_drives: drives,
        mag_tapes: tapes,
      }
    }
  }

  pub fn step(&mut self) -> Result<Instruction, VMError> {
    let opcode = self.memory.get(self.next_instr())?;
    let instr = Instruction::from_bytes(opcode)?;

    match instr {
      Ma { a, b, c } => {
        let mb = Instruction::from_bytes(self.memory.get(self.next_instr() + 1)?)?;

        match mb {
          Mb { b: b2 } => {
            self.mag_system.perform_operation(
              &DriveOperation::from_ma(a,b,c,b2).map_err(VMError::DriveErr)?,
              &mut self.memory
            ).map_err(VMError::DriveErr)?;
            self.increment_ic();
          }
          _ => { return Err(VMError::BadDriveOperation);}
        }
      }
      Mb { b: _ } => { self.increment_ic(); }
      Add  { a: l, b: r, c: res, normalize: needs_norm } => {
        let lfloat  = Float::from_bytes(self.memory.get(l)?);
        let rfloat  = Float::from_bytes(self.memory.get(r)?);
        let mut val = lfloat.add_unnormalized(&rfloat);

        warn!("ADD");
        warn!("{} + {} = {}", lfloat, rfloat, val);

        if needs_norm { val.normalize() };

        warn!("norm {}", val);
        self.memory.set(res, val.to_bytes())?;
        self.increment_ic();
      }
      Sub  { a: l, b: r, c: res, normalize: needs_norm } => {
        let lfloat  = Float::from_bytes(self.memory.get(l)?);
        let rfloat  = Float::from_bytes(self.memory.get(r)?);
        let mut val = lfloat.sub_unnormalized(&rfloat);

        if needs_norm { val.normalize() };

        self.memory.set(res, val.to_bytes())?;
        self.increment_ic();
      },
      Mult { a: l, b: r, c: res, normalize: needs_norm } => {
        let lfloat  = Float::from_bytes(self.memory.get(l)?);
        let rfloat  = Float::from_bytes(self.memory.get(r)?);
        let mut val = lfloat.mul_unnormalized(&rfloat);

        if needs_norm { val.normalize() };

        self.memory.set(res, val.to_bytes())?;
        self.increment_ic();
      }
      AddE { a: x, b: y, c: z, normalize: needs_norm } => {
        let lfloat = Float::from_bytes(self.memory.get(x)?);
        let rfloat = Float::from_bytes(self.memory.get(y)?);

        let mut result = Float::new(lfloat.mant, lfloat.exp + rfloat.exp);

        if needs_norm { result.normalize() };

        // alarm if power is > 31

        self.memory.set(z, result.to_bytes())?;
        self.increment_ic();
      }
      SubE { a: x, b: y, c: z, normalize: needs_norm } => {
        let lfloat = Float::from_bytes(self.memory.get(x)?);
        let rfloat = Float::from_bytes(self.memory.get(y)?);

        let mut result = Float::new(lfloat.mant, lfloat.exp + rfloat.exp);

        if needs_norm { result.normalize() };

        // alarm if power is > 31

        self.memory.set(z, result.to_bytes())?;
        self.increment_ic();
      }
      Ce { a: source, b: new_exp, c: target, normalize } => {
        let mut float = Float::from_bytes(self.memory.get(source)?);

        // The rule is if n >= 0 then new_exp = n, otherwise new_exp = 64 + n
        let exp = new_exp as i8;

        if exp <= 31 {
          float.exp = exp;
        } else if exp < 64 {
          float.exp = exp - 64;
        } else {
          return Err(VMError::OutOfBounds {});
        }

        if normalize { float.normalize(); }

        self.memory.set(target, float.to_bytes())?;
        self.increment_ic();
      }
      I { a, b, c } => {
        let float = Float::from_bytes(self.memory.get(a)?);

        let mut mant = Float::new(float.mant, 0);
        mant.sign = float.sign;

        let mut exp = Float::new(0, float.exp);

        self.memory.set(b, mant.to_bytes())?;
        self.memory.set(c, exp.to_bytes())?;
        self.increment_ic();
      }
      // TODO: Fix offset since right shift is _negative_
      ShiftAll { a, b, c } => {
        let val = self.memory.get(a)?;
        let shifted = if b <= 31 {
          val << b
        } else {
          val >> (b - 64)
        };

        self.memory.set(c, shifted)?;
        self.increment_ic();
      }
      Shift { a, b, c } => {
        let mut float = Float::from_bytes(self.memory.get(a)?);

        if b < 64 { // this should check <= 31 but first need to fix helpers in compile-pp
          float.mant <<= b
        } else {
          float.mant >>= b - 64
        };

        float.exp = 0;
        self.memory.set(c, float.to_bytes())?;
        self.increment_ic();
      }
      TN { a: source, c: target, normalize: needs_norm } => {
        let mut val = Float::from_bytes(self.memory.get(source)?);

        if needs_norm { val.normalize() };

        self.memory.set(target, val.to_bytes())?;
        self.increment_ic();
      }
      TExp { a: source, c: target, normalize: needs_norm } => {
        let val = Float::from_bytes(self.memory.get(source)?);
        warn!("Transferring exponent from {:?}", val);

        let mut res = Float::from_bytes(val.exp as u64);

        if needs_norm { res.normalize() };

        self.memory.set(target, res.to_bytes())?;
        self.increment_ic();

      }
      TSign { a: source, b: sign, c: target, normalize: needs_norm } => {
        let mut val = Float::from_bytes(self.memory.get(source)?);
        let sign = Float::from_bytes(self.memory.get(sign)?);

        val.sign ^= sign.sign;

        if needs_norm { val.normalize() };

        self.memory.set(target, val.to_bytes())?;
        self.increment_ic();
      }
      CCCC { b: cell, c: addr } => {
        if cell != 0 { self.memory.set(cell, self.next_instr() as u64)?; }
        self.global_ic = addr as u16;
        self.active_ic = ActiveIC::Global;
      }
      CLCC { c: addr } => {
        self.local_ic = addr as u16;
        self.active_ic = ActiveIC::Local;
      }
      JCC => {
        self.active_ic = ActiveIC::Global;
        self.increment_ic();
      }
      AICarry { a: p, b: q, c: r } => {
        let left = self.memory.get(p)?;
        let right = self.memory.get(q)?;

        let (mut result, _) = left.overflowing_add(right);
        let wrapping_carry = result.get_bits(39..40);

        result.set_bits(39..63, 0);
        result |= wrapping_carry;

        self.memory.set(r, result)?;
        self.increment_ic();
      }
      AI { a: p, b: q, c: r} => {
        let left = self.memory.get(p)?;
        let right = self.memory.get(q)?;

        let mut result = left.get_bits(0..33) + right.get_bits(0..33);
        result.set_bits(33..39, self.memory.get(p)?.get_bits(33..39));
        // info!("A1 : {} {} {}", p, q, r);

        // info!("AI {:039b}", left);
        // info!("AI {:039b}", right);
        // info!("AI {:039b}", result);

        self.memory.set(r, result)?;
        self.increment_ic();
      }
      LogMult { a, b, c } => {
        let left = self.memory.get(a)?;
        let right = self.memory.get(b)?;
        let result = left & right;

        self.memory.set(c, result)?;
        self.increment_ic();
      }
      Comp { a, b, c } => {
        use std::cmp::*;

        let left  = Float::from_bytes(self.memory.get(a)?);
        let right = Float::from_bytes(self.memory.get(b)?);

        info!("CMP {:10.5} {:10.5}", left, right);
        let branch = match left.exp.cmp(&right.exp) {
          Ordering::Greater => { left.sign }
          Ordering::Less => { !right.sign }
          Ordering::Equal => { match (left.sign, right.sign) { // want to check left mant < right mant
            (true, true) => left.mant < right.mant,
            (true, false) => false,
            (false, true) => true,
            (false, false) => right.mant > left.mant
            } }
        };

        if branch {
          info!("Branching: {} -> {}", self.next_instr(), c);
          self.set_ic(c);
        } else {
          self.increment_ic();
        }
      }

      CompMod { a, b, c } => {
        use std::cmp::*;

        let left  = Float::from_bytes(self.memory.get(a)?);
        let right = Float::from_bytes(self.memory.get(b)?);

        let branch = match left.exp.abs().cmp(&right.exp.abs()) {
          Ordering::Less => { right.mant != 0 }
          Ordering::Equal   => { left.mant < right.mant }
          _ => false

        };

        if branch {
          info!("Branching: {} -> {}", self.next_instr(), c);
          self.set_ic(c);
        } else {
          self.increment_ic();
        }
      }
      CompWord { a, b, c } => {
        let left = self.memory.get(a)?;
        let right = self.memory.get(b)?;

        info!("CMPWD {} {}", left, right);
        if left != right {
          info!("Branching {} -> {}", self.next_instr(), c);
          self.set_ic(c);
        } else {
          self.increment_ic();
        }
      }
      Stop => { self.stopped = true;  }
      Stop28 => { self.stopped = true; }
      i => {
        self.stopped = true;
        return Err(VMError::NotYetImplemented(i));
      }
    }

    Ok(instr)
  }

  pub fn next_instr(& self) -> u16 {
    match &self.active_ic {
      ActiveIC::Global => self.global_ic,
      ActiveIC::Local  => self.local_ic,
    }
  }

  fn increment_ic(&mut self) -> () {
    match &self.active_ic {
      ActiveIC::Global => self.global_ic += 1,
      ActiveIC::Local  => self.local_ic += 1,
    }
  }

  fn set_ic(&mut self, ix: u16) -> () {
    match &self.active_ic {
      ActiveIC::Global => self.global_ic = ix as u16,
      ActiveIC::Local  => self.local_ic = ix as u16,
    }
  }
}

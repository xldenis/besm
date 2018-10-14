use float::*;
use bit_field::BitField;

mod ds;
pub mod mag;
pub mod instruction;
use vm::mag::*;

pub struct VM<'a> {
  pub is: &'a mut [u64; 1023],
  ds: &'a [u64; 384],
  local_ic: u16,
  global_ic: u16,
  active_ic: ActiveIC,
  pub stopped: bool,
  pub mag_system: MagSystem<'a>
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
  pub fn new(is: &'a mut [u64; 1023], drives: &'a mut [MagDrive; 5], tapes: &'a mut [MagTape; 4], start: u16) -> VM<'a> {
    VM {
      is: is,
      ds: &ds::DS,
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
    let opcode = self.is[self.next_instr() as usize - 1];
    let instr = Instruction::from_bytes(opcode)?;

    match instr {
      Ma { a, b, c } => {
        let mb = Instruction::from_bytes(self.get_address(self.next_instr() + 1)?)?;

        match mb {
          Mb { b: b2 } => {
            self.mag_system.perform_operation(
              &DriveOperation::from_ma(a,b,c,b2).map_err(VMError::DriveErr)?,
              self.is
            ).map_err(VMError::DriveErr)?;
            self.increment_ic();
          }
          _ => { return Err(VMError::BadDriveOperation);}
        }
      }
      Mb { b: _ } => { self.increment_ic(); }
      Add  { a: l, b: r, c: res, normalize: needs_norm } => {
        let lfloat  = Float::from_bytes(self.get_address(l)?);
        let rfloat  = Float::from_bytes(self.get_address(r)?);
        let mut val = lfloat.add_unnormalized(&rfloat);

        if needs_norm { val.normalize() };

        self.set_address(res, val.to_bytes());
        self.increment_ic();

      }
      Sub  { a: l, b: r, c: res, normalize: needs_norm } => {
        let lfloat  = Float::from_bytes(self.get_address(l)?);
        let rfloat  = Float::from_bytes(self.get_address(r)?);
        let mut val = lfloat.sub_unnormalized(&rfloat);

        if needs_norm { val.normalize() };

        self.set_address(res, val.to_bytes());
        self.increment_ic();

      },
      Mult { a: l, b: r, c: res, normalize: needs_norm } => {
        let lfloat  = Float::from_bytes(self.get_address(l)?);
        let rfloat  = Float::from_bytes(self.get_address(r)?);
        let mut val = lfloat.mul_unnormalized(&rfloat);

        if needs_norm { val.normalize() };

        self.set_address(res, val.to_bytes());
        self.increment_ic();

      }
      AddE { a: x, b: y, c: z, normalize: needs_norm } => {
        let lfloat = Float::from_bytes(self.get_address(x)?);
        let rfloat = Float::from_bytes(self.get_address(y)?);

        let mut result = Float::new(lfloat.mant, lfloat.exp + rfloat.exp);

        if needs_norm { result.normalize() };

        // alarm if power is > 31

        self.set_address(z, result.to_bytes());
      }
      SubE { a: x, b: y, c: z, normalize: needs_norm } => {
        let lfloat = Float::from_bytes(self.get_address(x)?);
        let rfloat = Float::from_bytes(self.get_address(y)?);

        let mut result = Float::new(lfloat.mant, lfloat.exp + rfloat.exp);

        if needs_norm { result.normalize() };

        // alarm if power is > 31

        self.set_address(z, result.to_bytes());
      }
      Shift { a, b, c } => {
        let val = self.get_address(a)?;
        let mut shifted = if b < 64 {
          val << b
        } else {
          val >> (b - 64)
        };

        let res = shifted.set_bits(33..39, val.get_bits(33..39));

        self.set_address(c, *res);
        self.increment_ic();
      }
      TN { a: source, c: target, normalize: needs_norm } => {
        let mut val = Float::from_bytes(self.get_address(source)?);

        if needs_norm { val.normalize() };

        self.set_address(target, val.to_bytes());
        self.increment_ic();
      }
      TExp { a: source, c: target, normalize: needs_norm } => {
        let val = Float::from_bytes(self.get_address(source)?);
        warn!("Transferring exponent from {:?}", val);

        let mut res = Float::from_bytes(val.exp as u64);

        if needs_norm { res.normalize() };

        self.set_address(target, res.to_bytes());
        self.increment_ic();

      }
      CCCC { b: cell, c: addr } => {
        if cell != 0 { self.set_address(cell, self.next_instr() as u64); }
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
        let left = self.get_address(p)?;
        let right = self.get_address(q)?;

        let (mut result, _) = left.overflowing_add(right);
        let wrapping_carry = result.get_bits(39..40);

        result.set_bits(39..63, 0);
        result |= wrapping_carry;

        self.set_address(r, result);
        self.increment_ic();
      }
      AI { a: p, b: q, c: r} => {
        let left = self.get_address(p)?;
        let right = self.get_address(q)?;

        let mut result = left.get_bits(0..33) + right.get_bits(0..33);
        result.set_bits(33..39, self.get_address(p)?.get_bits(33..39));
        // info!("A1 : {} {} {}", p, q, r);

        // info!("AI {:039b}", left);
        // info!("AI {:039b}", right);
        // info!("AI {:039b}", result);

        self.set_address(r, result);
        self.increment_ic();
      }
      LogMult { a, b, c } => {
        let left = self.get_address(a)?;
        let right = self.get_address(b)?;
        let result = left & right;

        self.set_address(c, result);
        self.increment_ic();
      }
      Comp { a, b, c } => {
        use std::cmp::*;

        let left  = Float::from_bytes(self.get_address(a)?);
        let right = Float::from_bytes(self.get_address(b)?);

        info!("CMP {} {}", left, right);
        let branch = match left.exp.cmp(&right.exp) {
          Ordering::Greater => { left.sign }
          Ordering::Less => { !right.sign }
          Ordering::Equal => { left.mant < right.mant }
        };

        if branch {
          info!("Branching to {} from {}", c, self.next_instr());
          self.set_ic(c);
        } else {
          self.increment_ic();
        }
      }
      CompWord { a, b, c } => {
        let left = self.get_address(a)?;
        let right = self.get_address(b)?;

        info!("CMPWD {} {}", left, right);
        if left != right {
          info!("Branching to {} from {}", c, self.next_instr());
          self.set_ic(c);
        } else {
          self.increment_ic();
        }
      }
      Stop => { self.stopped = true; }
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

  pub fn get_address(&self, ix: u16) -> Result<u64, VMError> {
    if ix == 0 {
      Ok(0)
    } else if ix <= 1023 {
      Ok(self.is[(ix - 1) as usize])
    } else if 1024 < ix && ix <= 1408 { // The DS annoyingly starts on address 1025 not 1024 :rage:
      Ok(self.ds[(ix - 1023 - 1 - 1) as usize]) // We want address 1025 to map to index 0
    } else {
      Err(VMError::OutOfBounds{})
    }
  }

  fn set_address(& mut self, ix: u16, val: u64) -> &mut Self {
    if ix <= 1023 {
      self.is[(ix - 1) as usize] = val
    } else {
      panic!("can't assign to DS")
    }
    self
  }
}

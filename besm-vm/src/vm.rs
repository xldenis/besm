use bit_field::BitField;
use float::*;

pub struct VM<'a> {
  is: &'a mut [u64; 1023],
  ds: &'a [u64; 364],
  local_ic: u16,
  global_ic: u16,
  active_ic: ActiveIC,
  pub stopped: bool,

  mag_drives: &'a mut [MagDrive; 5],
  mag_tapes: &'a mut [MagTape; 4],
}

#[derive(Copy, Clone)]
pub struct MagDrive {
  drive: [u64; 1024]
}

#[derive(Copy, Clone)]
pub struct MagTape {
  head: u16,
  tape: [u64; 30_000]
}

enum ActiveIC { Global, Local}

static DS : [u64; 364] = [0; 364];

#[derive(Debug)]
pub enum VMError {
  OutOfBounds { },

}

impl MagDrive {
  pub fn new() -> MagDrive {
    MagDrive { drive: [0; 1024]}
  }
}

impl MagTape {
  pub fn new() -> MagTape {
    MagTape { head: 0, tape: [0; 30_000]}
  }
}

impl<'a> VM<'a> {
  pub fn new(is: &'a mut [u64; 1023], drives: &'a mut [MagDrive; 5], tapes: &'a mut [MagTape; 4], start: u16) -> VM<'a> {
    VM {
      is: is,
      ds: &DS,
      global_ic: start,
      local_ic: 1,
      active_ic: ActiveIC::Global,
      stopped: false,
      mag_drives: drives,
      mag_tapes: tapes,
    }
  }

  pub fn step(&mut self) -> Result<Instruction, VMError> {
    let opcode = self.is[self.next_instr() as usize - 1];
    let instr = Instruction::from_bytes(opcode);

    match instr {
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
      TN { a: source, c: target, normalize: needs_norm } => {
        let mut val = Float::from_bytes(self.get_address(source)?);

        if needs_norm { val.normalize() };

        self.set_address(target, val.to_bytes());
        self.increment_ic();
      }
      CCCC { b: cell, c: addr } => {
        if cell != 0 { self.is[cell as usize]; }
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
        let laddrs = self.get_address(p)?.get_bits(0..33);
        let raddrs = self.get_address(q)?.get_bits(0..33);

        let mut result = laddrs + raddrs;

        result.set_bits(33..39, self.get_address(p)?.get_bits(33..39));

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

        let branch = match left.exp.cmp(&right.exp) {
          Ordering::Greater => { left.sign }
          Ordering::Less => { !right.sign }
          Ordering::Equal => { left.mant < right.mant }
        };

        if branch {
          self.set_ic(c);
        } else {
          self.increment_ic();
        }
      }
      Stop => { self.stopped = true; }
      Stop28 => { self.stopped = true; }
      a => {
        println!("NYI {:?}", a);
        self.stopped = true;
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

  fn set_ic(&mut self, ix: u64) -> () {
    match &self.active_ic {
      ActiveIC::Global => self.global_ic = ix as u16,
      ActiveIC::Local  => self.local_ic = ix as u16,
    }
  }

  pub fn get_address(&self, ix: u64) -> Result<u64, VMError> {
    if ix == 0 {
      Err(VMError::OutOfBounds {})
    } else if ix <= 1023 {
      Ok(self.is[(ix - 1) as usize])
    } else {
      Ok(self.ds[(ix - 1023 - 1) as usize])
    }
  }

  fn set_address(& mut self, ix: u64, val: u64) -> &mut Self {
    if ix <= 1023 {
      self.is[(ix - 1) as usize] = val
    } else {
      panic!("can't assign to DS")
    }
    self
  }
}


type Address = u64;

#[derive(Debug, Copy, Clone)]
pub enum Instruction {
  Add       { a: Address, b: Address, c: Address, normalize: bool},
  Sub       { a: Address, b: Address, c: Address, normalize: bool},
  Mult      { a: Address, b: Address, c: Address, normalize: bool},
  Div       { a: Address, b: Address, c: Address, normalize: bool},
  AddE      { a: Address, b: Address, c: Address, normalize: bool},
  SubE      { a: Address, b: Address, c: Address, normalize: bool},
  Ce        { a: Address, b: Address, c: Address, normalize: bool},
  Xa        { a: Address, b: Address, c: Address, normalize: bool},
  Xb        { c: Address, normalize: bool },
  DivA      { a: Address, b: Address, c: Address, normalize: bool},
  DivB      { c: Address, normalize: bool },
  TN        { a: Address, c: Address, normalize: bool},
  PN        { a: Address },
  TMin      { a: Address,  c: Address, normalize: bool},
  TMod      { a: Address,  c: Address, normalize: bool},
  TSign     { a: Address, b: Address, c: Address, normalize: bool},
  TExp      { a: Address,  c: Address, normalize: bool},
  Shift     { a: Address, b: Address, c: Address },
  ShiftAll  { a: Address, b: Address, c: Address },
  AI        { a: Address, b: Address, c: Address },
  AICarry   { a: Address, b: Address, c: Address },
  I         { a: Address, b: Address, c: Address },
  Comp      { a: Address, b: Address, c: Address },
  CompWord  { a: Address, b: Address, c: Address },
  CompMod   { a: Address, b: Address, c: Address },
  Ma        { a: Address, b: Address, c: Address },
  Mb        { a: Address, b: Address, c: Address },
  JCC,
  CLCC      { c: Address },
  CCCC      { b: Address, c: Address},
  Stop28,
  LogMult   { a: Address, b: Address, c: Address },
  Stop
}

use Instruction::*;

pub fn first_addr(x: u64) -> u64 {
  x.get_bits(22..32)
}

pub fn second_addr(x: u64) -> u64 {
  x.get_bits(11..21)
}

pub fn third_addr(x: u64) -> u64 {
  x.get_bits(0..10)
}

impl Instruction {
  // Eventually this should be Result<(), Instruction>
  pub fn from_bytes(word: u64) -> Instruction {
    match word.get_bits(33..38) {
      0x01 => Add       { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x02 => Sub       { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x03 => Mult      { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x04 => Div       { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x05 => AddE      { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x06 => SubE      { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x07 => Ce        { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x08 => Xa        { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x09 => Xb        { normalize: !word.get_bit(38),                                            c: third_addr(word)},
      0x0A => DivA      { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x0B => DivB      { normalize: !word.get_bit(38),                                            c: third_addr(word)},
      0x0C => TN        { normalize: !word.get_bit(38), a: first_addr(word),                       c: third_addr(word)},
      0x2C => PN        {                               a: first_addr(word)},
      0x0D => TMin      { normalize: !word.get_bit(38), a: first_addr(word),                       c: third_addr(word)},
      0x0E => TMod      { normalize: !word.get_bit(38), a: first_addr(word),                       c: third_addr(word)},
      0x0F => TSign     { normalize: !word.get_bit(38), a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x10 => TExp      { normalize: !word.get_bit(38), a: first_addr(word),                       c: third_addr(word)},
      0x11 => match word.get_bit(38) {
        false => Shift  {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
        true => ShiftAll{                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      },
      0x12 => match word.get_bit(38) {
        false => AI     {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
        true => AICarry {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      },
      0x13 => I         {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x14 => Comp      {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x34 => CompWord  {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x15 => CompMod   {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x16 => Ma        {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x17 => Mb        {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x19 => JCC       { },
      0x1A => CLCC      {                                                                         c: third_addr(word)},
      0x1B => CCCC      {                                                   b: second_addr(word), c: third_addr(word)},
      0x1C => Stop28    { },
      0x1D => LogMult   {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x1F => Stop      { },
      _    => panic!("omg"),
    }
  }
}

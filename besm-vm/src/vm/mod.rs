use bit_field::BitField;
use float::*;

mod ds;
pub struct VM<'a> {
  is: &'a mut [u64; 1023],
  ds: &'a [u64; 384],
  local_ic: u16,
  global_ic: u16,
  active_ic: ActiveIC,
  pub stopped: bool,
  mag_system: MagSystem<'a>
}

#[derive(Copy, Clone)]
pub struct MagDrive {
  drive: [u64; 1024]
}
use std::collections::HashMap;

#[derive(Clone)]
pub struct MagTape {
  /*
  This may seem like a weird representation for a tape.
  However, since according to the assembly instruction reference we can write
  up to 64 blocks of arbitrary length, it appears like it had a block-marker
  channel to track the start of each data-block. Storing it as a HashMap is
  conceptually much simpler than other formats.
  */
  tape: HashMap<u8, Vec<u64>>
}

enum ActiveIC { Global, Local}

#[derive(Debug)]
pub enum VMError {
  OutOfBounds { },
  PartialMa(Instruction, DriveOperation),
  BadDriveOperation,
  InvalidInstruction(u64),
  NotYetImplemented(Instruction),
}

impl MagDrive {
  pub fn new(vals: [u64; 1024]) -> MagDrive {
    MagDrive { drive: vals }
  }
}

impl <'a> MagTape {
  pub fn new() -> MagTape {
    MagTape { tape: HashMap::new() }
  }
}

struct MagSystem<'a> {
  mag_drives: &'a mut [MagDrive; 5],
  mag_tapes: &'a mut [MagTape; 4],
  partial_operation: Option<DriveOperation>
}

impl <'a> MagSystem<'a> {
  fn op_started(&self) -> bool {
    match self.partial_operation {
      None => false,
      Some(_) => true,
    }
  }

  fn perform_operation(&mut self, n2: u16, is: &mut [u64; 1023]) -> Option<()> {
    use vm::DriveOperation::*;

    match &self.partial_operation {
      None => { panic!("return error") }

      Some(WriteMD(id, n1, c)) => {
        warn!("Writing to MD");
        let span = (n2 + 1).checked_sub(*n1)?;
        warn!("TEST");
        let drive = &mut self.mag_drives[id.to_num() as usize];

        for (place, data) in drive.drive.iter_mut().skip(*n1 as usize).zip(is.iter().skip((c - 1) as usize).take(span as usize)) {
          *place = *data;
        }
      }
      Some(ReadMD(id, n1, c)) => {

        let span = n2 + 1 - n1;
        let drive = &mut self.mag_drives[id.to_num() as usize];
        for (place, data) in is.iter_mut().skip((*c-1) as usize).zip(drive.drive.iter().skip(*n1 as usize).take(span as usize)) {
          *place = *data;
        }
      }

      Some(ReadMT(id, r, c)) => {
        let tape = &mut self.mag_tapes[id.to_num() as usize];

        if *r > 63 {
          return None;
        }

        let range = is[(*c as usize) .. ((c + n2) as usize)].to_vec();
        tape.tape.insert(*r as u8, range);
      }
      _ => { panic!("NOT DONE") }
    }

    self.partial_operation = None;

    Some(())
  }
}

#[derive(Debug, Clone)]
pub enum DrumId { Zero, One, Two, Three, Four }

#[derive(Debug, Clone)]
pub enum TapeId { One, Two, Three, Four }

impl DrumId {
  pub fn to_num(&self) -> u8 {
    match self {
      DrumId::Zero => 0,
      DrumId::One => 1,
      DrumId::Two => 2,
      DrumId::Three => 3,
      DrumId::Four => 4,
    }
  }
}

impl TapeId {
  pub fn to_num(&self) -> u8 {
    match self {
      TapeId::One => 1,
      TapeId::Two => 2,
      TapeId::Three => 3,
      TapeId::Four => 4,
    }
  }
}

#[derive(Debug, Clone)]
pub enum DriveOperation {
  WriteMD(DrumId, u16, u16),
  ReadMD(DrumId, u16, u16),
  ReadTape(u16),
  WriteMT(TapeId, u16, u16),
  ReadMT(TapeId, u16, u16),
  RewindMT(TapeId, u16),
}

fn drum_id_from_num(ix: u16) -> DrumId {
  match ix {
    0 => DrumId::Zero,
    1 => DrumId::One,
    2 => DrumId::Two,
    3 => DrumId::Three,
    4 => DrumId::Four,
    _ => panic!("bad drum id")
  }
}

fn tape_id_from_num(ix: u16) -> TapeId {
  match ix {
    1 => TapeId::One,
    2 => TapeId::Two,
    3 => TapeId::Three,
    4 => TapeId::Four,
    _ => panic!("bad tape id")
  }
}

impl DriveOperation {
  pub fn from_ma(a: u16, b: u16, c: u16) -> Result<DriveOperation, VMError> {
    use vm::DriveOperation::*;
    match a {
      0x0300 ... 0x0304 => { Ok(WriteMD(drum_id_from_num(a - 0x0300), b, c)) }
      0x0100 ... 0x0104 => { Ok(ReadMD(drum_id_from_num(a - 0x0100), b, c)) }
      0x0080            => { Ok(ReadTape(c)) }
      0x0281 ... 0x0284 => { Ok(WriteMT(tape_id_from_num(a), b, c)) }
      0x0081 ... 0x0084 => { Ok(ReadMT(tape_id_from_num(a), b, c)) }
      0x00C1 ... 0x00C4 => { Ok(RewindMT(tape_id_from_num(a), b)) }
      _ => Err(VMError::BadDriveOperation)
    }
  }
}

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
        partial_operation: None,
      }
    }
  }


  pub fn step(&mut self) -> Result<Instruction, VMError> {
    let opcode = self.is[self.next_instr() as usize - 1];
    let instr = Instruction::from_bytes(opcode)?;

    match instr {
      Mb { b } if self.mag_system.op_started() => {
        warn!("{}", instr);
        self.mag_system.perform_operation(b, &mut self.is).ok_or(VMError::BadDriveOperation)?;
        self.increment_ic();

      }
      i if self.mag_system.op_started() => {
        self.stopped = true;

        let drive_op = self.mag_system.partial_operation.clone().unwrap();
        Err(VMError::PartialMa(i, drive_op))?
      }
      Ma { a, b, c } => {
        self.mag_system.partial_operation = Some(DriveOperation::from_ma(a,b,c)?);
        self.increment_ic();
      }
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
      Shift { a, b, c } => {
        let val = self.get_address(a)?;
        if b < 64 {
          self.set_address(c, val << b);
        } else {
          self.set_address(c, val >> (b - 64));
        }
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
      Err(VMError::OutOfBounds {})
    } else if ix <= 1023 {
      Ok(self.is[(ix - 1) as usize])
    } else {
      Ok(self.ds[(ix - 1023 - 1) as usize])
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


type Address = u16;

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
  Xb        {                         c: Address, normalize: bool },
  DivA      { a: Address, b: Address, c: Address, normalize: bool},
  DivB      {                         c: Address, normalize: bool },
  TN        { a: Address,             c: Address, normalize: bool},
  PN        { a: Address },
  TMin      { a: Address,             c: Address, normalize: bool},
  TMod      { a: Address,             c: Address, normalize: bool},
  TSign     { a: Address, b: Address, c: Address, normalize: bool},
  TExp      { a: Address,             c: Address, normalize: bool},
  Shift     { a: Address, b: Address, c: Address },
  ShiftAll  { a: Address, b: Address, c: Address },
  AI        { a: Address, b: Address, c: Address },
  AICarry   { a: Address, b: Address, c: Address },
  I         { a: Address, b: Address, c: Address },
  Comp      { a: Address, b: Address, c: Address },
  CompWord  { a: Address, b: Address, c: Address },
  CompMod   { a: Address, b: Address, c: Address },
  Ma        { a: Address, b: Address, c: Address },
  Mb        {             b: Address},
  JCC,
  CLCC      {                         c: Address },
  CCCC      {             b: Address, c: Address},
  Stop28,
  LogMult   { a: Address, b: Address, c: Address },
  Stop
}

use Instruction::*;

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
      0x17 => Mb        {                                                   b: second_addr(word)},
      0x19 => JCC       { },
      0x1A => CLCC      {                                                                         c: third_addr(word)},
      0x1B => CCCC      {                                                   b: second_addr(word), c: third_addr(word)},
      0x1C => Stop28    { },
      0x1D => LogMult   {                              a: first_addr(word), b: second_addr(word), c: third_addr(word)},
      0x1F => Stop      { },
      _    => return Err(VMError::InvalidInstruction(word))
    };

    Ok(instr)
  }
}

use std::fmt;

impl fmt::Display for Instruction {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    macro_rules! denormed {
      ($normed:expr, $nm:expr) => (format!("{}{}", if $normed {""} else {","}, $nm))
    }

    match self {
      Add     {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Add")   , a, b, c),
      Sub     {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Sub")   , a, b, c),
      Mult    {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Mult")  , a, b, c),
      Div     {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Div")   , a, b, c),
      AddE    {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "AddE")  , a, b, c),
      SubE    {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "SubE")  , a, b, c),
      Ce      {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Ce")    , a, b, c),
      Xa      {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Xa")    , a, b, c),
      Xb      {c,     normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "Xb")    ,"","", c),
      DivA    {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "DivA")  , a, b, c),
      DivB    {c,     normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "DivB")  ,"","", c),
      TN      {a,c,   normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TN")    , a,"", c),
      PN      {a}                      => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "PN")    , a,"",""),
      TMin    {a,c,   normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TMin")  , a,"", c),
      TMod    {a,c,   normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TMod")  , a,"", c),
      TSign   {a,b,c, normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TSgn")  , a, b, c),
      TExp    {a,c,   normalize: norm} => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(*norm, "TExp")  , a,"", c),
      Shift   {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "Shft")  , a, b, c),
      ShiftAll{a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(false, "Shft")  , a, b, c),
      AI      {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "AI")    , a, b, c),
      AICarry {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(false, "AI")    , a, b, c),
      I       {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "I")     , a, b, c),
      Comp    {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "Cmp")   , a, b, c),
      CompWord{a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "CmpWd") , a, b, c),
      CompMod {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "CmpMd") , a, b, c),
      Ma      {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "Ma")    , a, b, c),
      Mb      {b}                      => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "Mb")    ,"", b,""),
      JCC                              => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "JCC")   ,"","",""),
      CLCC    {c}                      => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "CLCC")  ,"","", c),
      CCCC    {  b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "CCCC")  ,"", b, c),
      Stop28                           => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "Stp28") ,"","",""),
      LogMult {a,b,c}                  => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "BitMl") , a, b, c),
      Stop                             => write!(f, "{:<5} {:4} {:4} {:4}", denormed!(true,  "Stop")  ,"","",""),
    }
  }
}

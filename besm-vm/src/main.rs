#![feature(nll)]

extern crate bit_field;
#[macro_use]
extern crate structopt;
extern crate byteorder;

use std::path::PathBuf;
use structopt::StructOpt;
use bit_field::BitField;
use std::fs::File;
use std::cmp::*;

struct VM<'a> {
  is: &'a mut [u64; 1023],
  ds: &'a [u64; 364],
  local_ic: u16,
  global_ic: u16,
  active_ic: ActiveIC,
  stopped: bool,
}

enum ActiveIC { Global, Local}

static DS : [u64; 364] = [0; 364];

impl<'a> VM<'a> {
  pub fn new(is: &'a mut [u64; 1023]) -> VM {
    VM {
      is: is,
      ds: &DS,
      global_ic: 1,
      local_ic: 1,
      active_ic: ActiveIC::Global,
      stopped: false,
    }
  }

  pub fn step(&mut self) -> () {
    let opcode = self.is[self.next_instr() as usize - 1];

    match Instruction::from_bytes(opcode) {
      Add  { a: l, b: r, c: res, normalize: needs_norm } => {
        let mut lfloat = Float::from_bytes(self.get_address(l));
        let mut rfloat = Float::from_bytes(self.get_address(r));

        match lfloat.exp.cmp(&rfloat.exp) {
          Ordering::Less    => {
            lfloat.mant = lfloat.mant.checked_shr((lfloat.exp - rfloat.exp) as u32).unwrap_or(0);
            lfloat.exp = rfloat.exp;
          }
          Ordering::Greater => {
            rfloat.mant = rfloat.mant.checked_shr((rfloat.exp - lfloat.exp) as u32).unwrap_or(0);
            rfloat.exp = lfloat.exp;
          }
          Ordering::Equal => { }
        }

        let (res_mant, overflow) = lfloat.mant.overflowing_add(rfloat.mant);
        let res_sign = match lfloat.mant.cmp(&rfloat.mant) {
          Ordering::Less => rfloat.sign,
          Ordering::Greater => lfloat.sign,
          Ordering::Equal   => false,
        };

        let mut val = Float { mant: res_mant, overflow: overflow, sign: res_sign, exp: rfloat.exp };

        if needs_norm { val.normalize() };
        self.set_address(res, val.to_bytes());
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
      Stop => {}
      Stop28 => {}
      _a => { self.stopped = true; }
    }
  }

  fn next_instr(& self) -> u16 {
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

  fn get_address(& self, ix: u64) -> u64 {
    if ix <= 1023 {
      self.is[(ix - 1) as usize]
    } else {
      self.ds[(ix - 1023 - 1) as usize]
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
extern crate num;

#[derive(Debug)]
struct Float {
  mant: u32, // actually only the lower 32 bits are used (u64 lets us store overflow  though...)
  overflow: bool,
  exp: i8, // actually is only 6 bits long aka (-32 - 31)
  sign: bool
}

impl Float {
  pub fn from_bytes(word: u64) -> Float {
    let exp = (0xFF & word.get_bits(33..38)) as i8;
    let mant = word.get_bits(0..32) as u32;
    let sign = word.get_bit(32);
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
}

type Address = u64;

#[derive(Debug)]
enum Instruction {
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

fn first_addr(x: u64) -> u64 {
  x.get_bits(22..32)
}
fn second_addr(x: u64) -> u64 {
  x.get_bits(11..21)
}
fn third_addr(x: u64) -> u64 {
  x.get_bits(0..10)
}

impl Instruction {
  pub fn from_bytes(word: u64) -> Instruction {
    match word.get_bits(33..37) {
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
      0x2B => TN        { normalize: !word.get_bit(38), a: first_addr(word),                       c: third_addr(word)},
      0x0C => PN        {                               a: first_addr(word)},
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

#[derive(StructOpt, Debug)]
#[structopt(name = "omg")]
struct Opts {
  #[structopt(short = "s", long = "start-address", default_value = "0")]
  start_address: u64,
  #[structopt(name = "FILE", parse(from_os_str))]
  is_file: PathBuf,
}

use byteorder::{BigEndian, ReadBytesExt};
use std::iter;
fn main() {
    let opt = Opts::from_args();

    let mut f = File::open(opt.is_file.clone()).expect("file not found");

    let words : Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0)).take(1023).collect();
    let mut is_buf = [0u64; 1023];
    is_buf.copy_from_slice(&words[..]);

    let mut vm = VM::new(&mut is_buf);

    while !vm.stopped {
      let ins = vm.is[vm.next_instr() as usize];

      println!("{:?} {:039b} {:016x}", vm.next_instr(), ins, ins);
      vm.step();

    }

    println!("{:?}", opt);

}

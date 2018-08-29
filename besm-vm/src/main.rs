#![feature(nll)]

extern crate bit_field;
#[macro_use] extern crate structopt;
#[macro_use] extern crate log;

extern crate tui_logger;
extern crate byteorder;
extern crate num;
extern crate arraydeque;
extern crate tui;

use std::path::PathBuf;
use structopt::StructOpt;
use std::fs::File;

use vm::*;
use interface::*;
use byteorder::{BigEndian, ReadBytesExt};
use std::iter;

use tui::backend::MouseBackend;
use tui::Terminal;


use std::io;
use std::{thread, time};
use arraydeque::{ArrayDeque};
use std::sync::mpsc;

mod float;
mod vm;
mod interface;

#[derive(StructOpt, Debug)]
#[structopt(name = "besm-vm")]
struct Opts {
  #[structopt(short = "s", long = "start-address", default_value = "1")]
  start_address: u64,
  #[structopt(name = "FILE", parse(from_os_str))]
  is_file: PathBuf,

  #[structopt(long = "md0", parse(from_os_str))]
  md0: Option<PathBuf>,
  #[structopt(long = "md1", parse(from_os_str))]
  md1: Option<PathBuf>,
  #[structopt(long = "md2", parse(from_os_str))]
  md2: Option<PathBuf>,
  #[structopt(long = "md3", parse(from_os_str))]
  md3: Option<PathBuf>,
  #[structopt(long = "md4", parse(from_os_str))]
  md4: Option<PathBuf>,
}


extern crate termion;

fn md_from_file(file: Option<PathBuf>) -> MagDrive {
  let mut buf = [0; 1024];
  match file {
    None => { }
    Some(path) => {
      let mut f = File::open(path).expect("file not found");
      let     words : Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0)).take(1024).collect();

      buf.copy_from_slice(&words);
    }
  }

  MagDrive::new(buf)
}
use log::LevelFilter;
use tui_logger::*;

fn main() {
  init_logger(LevelFilter::Info).unwrap();
  set_default_level(LevelFilter::Trace);

  let     opt = Opts::from_args();
  let mut f = File::open(opt.is_file.clone()).expect("file not found");
  let     words : Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0)).take(1023).collect();

  let mut is_buf = [0u64; 1023];
  let mut terminal = Terminal::new(MouseBackend::new().unwrap()).unwrap();

  let mut interface = Interface {
    size: terminal.size().unwrap(),
    past_instrs: ArrayDeque::new(),
  };

  is_buf.copy_from_slice(&words[..]);

  let mut x = [md_from_file(opt.md0), md_from_file(opt.md1), md_from_file(opt.md2), md_from_file(opt.md3), md_from_file(opt.md4)];
  let mut y = [MagTape::new(), MagTape::new(), MagTape::new(), MagTape::new()];
  let mut vm = VM::new(&mut is_buf, &mut x, &mut y, opt.start_address as u16);

  let (tx, rx) = mpsc::channel();

  thread::spawn(move || {
    use termion::input::TermRead;
    use termion::event;

    let stdin = io::stdin();

    for e in stdin.keys() {
      let evt = e.unwrap();

      tx.send(evt).unwrap();

      if let event::Key::Char('q') = evt { break; }
    }
  });

  terminal.clear().unwrap();
  terminal.hide_cursor().unwrap();

  let quarter_sec = time::Duration::from_millis(250);

  loop {
    let size = terminal.size().unwrap();
    if size != interface.size {
        terminal.resize(size).unwrap();
        interface.size = size;
    }

    draw(&mut terminal, &vm, &interface);

    use termion::event;

    let evt = rx.try_recv();
    use std::sync::mpsc::TryRecvError::*;

    match evt {
      Ok(event::Key::Char('q')) => { break; }
      Ok(_) => {}
      Err(Disconnected) => { break; }
      Err(Empty) => { }
    }

    if !vm.stopped {
      match vm.step() {
        Err(e) => { error!("{:?}", e); vm.stopped = true;},
        Ok(i) => { interface.past_instrs.push_front(i); },
      }
    }

    thread::sleep(quarter_sec);
  }

  terminal.show_cursor().unwrap();
}

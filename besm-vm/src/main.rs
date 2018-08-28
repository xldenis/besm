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
use bit_field::BitField;

mod float;
mod vm;

use vm::*;
use byteorder::{BigEndian, ReadBytesExt};
use std::iter;

use tui::backend::MouseBackend;
use tui::Terminal;
use tui::layout::{Direction, Group, Rect, Size};
use tui::widgets::{Widget, Paragraph, Block, Borders};
use tui::style::{Alignment};

fn draw(t: &mut Terminal<MouseBackend>, vm: &VM, ui_state: &ArrayDeque<[Instruction; 10], Wrapping> , size: &Rect) {
  Group::default()
    .direction(Direction::Vertical)
    .sizes(&[Size::Fixed(3), Size::Fixed(12), Size::Percent(100)])
    .render(t, size, |t, chunks| {
      Block::default()
        .borders(Borders::ALL)
        .title("Current Instruction")
        .render(t, &chunks[0]);

      Group::default()
        .margin(1)
        .direction(Direction::Horizontal)
        .sizes(&[Size::Fixed(5), Size::Fixed(2), Size::Fixed(42), Size::Fixed(2), Size::Fixed(10), Size::Fixed(2), Size::Percent(100)])
        .render(t, &chunks[0], |t, chunks| {
          let ins = vm.get_address(vm.next_instr() as u64).unwrap();

          Paragraph::default()
            .text(&format!("{:04}", vm.next_instr()))
            .alignment(Alignment::Right)
            .render(t, &chunks[0]);

          Paragraph::default()
            .text(&format!("{:06b} {:011b} {:011b} {:011b}", ins.get_bits(33..38), first_addr(ins), second_addr(ins), third_addr(ins)))
            .render(t, &chunks[2]);

          Paragraph::default()
            .text(&format!("{:010x}", ins))
            .render(t, &chunks[4]);

          Paragraph::default()
            .text(&Instruction::from_bytes(ins).map(|s| format!("{:?}", s)).unwrap_or("ERROR".to_string()))
            .alignment(Alignment::Right)
            .wrap(true)
            .render(t, &chunks[6]);
        });

      Block::default()
        .borders(Borders::ALL)
        .title("Past Instructions")
        .render(t, &chunks[1]);

      Group::default()
        .margin(1)
        .direction(Direction::Vertical)
        .sizes(&[Size::Fixed(1); 10])
        .render(t, &chunks[1], |t, chunks| {
          for (instr, chunk) in ui_state.iter().zip(chunks.iter()) {
            Paragraph::default()
              .text(&format!("{:?}", instr))
              .alignment(Alignment::Right)
              .render(t, &chunk);
          }
        });

      TuiLoggerWidget::default()
        .block(
          Block::default()
            .title("Log")
            .borders(Borders::ALL)
        )
        .render(t, &chunks[2]);
    });
  t.draw().unwrap();
}

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

use std::io;
use std::{thread, time};
use arraydeque::{ArrayDeque};
use arraydeque::behavior::Wrapping;
use std::sync::mpsc;

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
  let mut past_instrs: ArrayDeque<[_; 10], Wrapping> = ArrayDeque::new();
  let mut terminal = Terminal::new(MouseBackend::new().unwrap()).unwrap();
  let term_size = terminal.size().unwrap();

  is_buf.copy_from_slice(&words[..]);

  let mut x = [md_from_file(opt.md0), md_from_file(opt.md1), md_from_file(opt.md2), md_from_file(opt.md3), md_from_file(opt.md4)];
  let mut y = [MagTape::new(); 4];
  let mut vm = VM::new(&mut is_buf, &mut x, &mut y, opt.start_address as u16);

  let (tx, rx) = mpsc::channel();

  thread::spawn(move || {
    use termion::input::TermRead;
    use termion::event;

    let stdin = io::stdin();

    for e in stdin.keys() {
      let evt = e.unwrap();

      tx.send(evt).unwrap();

      match evt {
        event::Key::Char('q') => { break; }
        _ => { }
      }
    }
  });

  terminal.clear().unwrap();
  terminal.hide_cursor().unwrap();

  let quarter_sec = time::Duration::from_millis(250);

  loop {
    draw(&mut terminal, &vm, &past_instrs, &term_size);

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
        Ok(i) => { past_instrs.push_front(i); },
      }
    }

    thread::sleep(quarter_sec);
  }

  terminal.show_cursor().unwrap();
}

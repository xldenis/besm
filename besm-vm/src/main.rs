#![feature(nll)]

extern crate bit_field;
#[macro_use] extern crate structopt;
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

      let empty_space = 0.max(chunks[0].width - (5 + 42 + 10) - 4);
      let spacer = empty_space / 2;

      Group::default()
        .margin(1)
        .direction(Direction::Horizontal)
        .sizes(&[Size::Fixed(5), Size::Fixed(spacer), Size::Fixed(42), Size::Fixed(spacer), Size::Fixed(10)])
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

    });
  t.draw();
}

#[derive(StructOpt, Debug)]
#[structopt(name = "omg")]
struct Opts {
  #[structopt(short = "s", long = "start-address", default_value = "0")]
  start_address: u64,
  #[structopt(name = "FILE", parse(from_os_str))]
  is_file: PathBuf,
}

use std::io;
use std::{thread, time};
use arraydeque::{ArrayDeque};
use arraydeque::behavior::Wrapping;
use std::sync::mpsc;

extern crate termion;

fn main() {
  let     opt = Opts::from_args();
  let mut f = File::open(opt.is_file.clone()).expect("file not found");
  let     words : Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0)).take(1023).collect();

  let mut is_buf = [0u64; 1023];
  let mut t = MouseBackend::new().unwrap();
  let mut past_instrs: ArrayDeque<[_; 10], Wrapping> = ArrayDeque::new();
  let mut terminal = Terminal::new(MouseBackend::new().unwrap()).unwrap();
  let mut term_size = terminal.size().unwrap();

  is_buf.copy_from_slice(&words[..]);
  let mut vm = VM::new(&mut is_buf);

  let (tx, rx) = mpsc::channel();

  thread::spawn(move || {
    use termion::input::TermRead;
    use termion::event;

    let stdin = io::stdin();

    for e in stdin.keys() {
      let evt = e.unwrap();

      tx.send(evt);

      match evt {
        event::Key::Char('q') => { break; }
        _ => { }
      }
    }
  });

  terminal.clear().unwrap();
  terminal.hide_cursor().unwrap();

  let quarter_sec = time::Duration::from_millis(250);
  let mut ui_stop = false;

  while (!vm.stopped && !ui_stop) {
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

    match vm.step() {
      Err(e) => { ui_stop = true; },
      Ok(i) => { past_instrs.push_front(i); },
    };

    thread::sleep(quarter_sec);
  }

  terminal.show_cursor().unwrap();
}

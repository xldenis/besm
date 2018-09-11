#![feature(nll)]

extern crate bit_field;
#[macro_use]
extern crate structopt;
#[macro_use]
extern crate log;

extern crate arraydeque;
extern crate byteorder;
extern crate num;
extern crate tui;
extern crate tui_logger;

use std::fs::File;
use std::path::PathBuf;
use structopt::StructOpt;

use byteorder::{BigEndian, ReadBytesExt};
use interface::*;
use std::iter;
use vm::*;

use tui::backend::MouseBackend;
use tui::Terminal;

use arraydeque::ArrayDeque;
use std::io;
use std::sync::mpsc;
use std::{thread, time};

mod float;
mod interface;
mod vm;

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
use vm::mag::MagDrive;

fn md_from_file(file: Option<PathBuf>) -> MagDrive {
    let mut buf = [0; 1024];
    match file {
        None => {}
        Some(path) => {
            let mut f = File::open(path).expect("file not found");
            let words: Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0))
            .take(1024)
            .collect();

            buf.copy_from_slice(&words);
        }
    }

    MagDrive::new(buf)
}
use log::LevelFilter;
use tui_logger::*;

use termion::event;

#[derive(Debug)]
enum Event {
    Key(event::Key),
    Tick,
    Goto(usize),
}

fn main() {
    init_logger(LevelFilter::Info).unwrap();
    set_default_level(LevelFilter::Trace);

    let opt = Opts::from_args();

    let mut terminal = Terminal::new(MouseBackend::new().unwrap()).unwrap();
    let mut interface = Interface {
        size: terminal.size().unwrap(),
        past_instrs: ArrayDeque::new(),
        step_mode: interface::StepMode::Step,
        tabs: interface::TabInfo::default(),
    };

    let mut f = File::open(opt.is_file.clone()).expect("file not found");
    let words: Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0))
        .take(1023)
        .collect();

    let mut is_buf = [0u64; 1023];
    is_buf.copy_from_slice(&words[..]);

    let mut x = [
        md_from_file(opt.md0),
        md_from_file(opt.md1),
        md_from_file(opt.md2),
        md_from_file(opt.md3),
        md_from_file(opt.md4),
    ];

    use vm::mag::MagTape;
    let mut y = [
        MagTape::new(),
        MagTape::new(),
        MagTape::new(),
        MagTape::new(),
    ];

    let mut vm = VM::new(&mut is_buf, &mut x, &mut y, opt.start_address as u16);

    let (tx, rx) = mpsc::channel();

    let tx2 = tx.clone();
    thread::spawn(move || {
        let speed = time::Duration::from_millis(50);
        loop {
            tx2.send(Event::Tick).unwrap();
            thread::sleep(speed);
        }
    });

    thread::spawn(move || {
        use termion::input::TermRead;
        use termion::event::Key::*;

        let stdin = io::stdin();

        for e in stdin.keys() {
            let evt = e.unwrap();
            match evt {
                Char('g') => {
                    let key_evs : String = io::stdin().keys().take_while(|ev| {
                        match ev {
                            Ok(Char('\n')) => false,
                            Ok(Char(_)) => true,
                            _           => false,
                        }
                    }).map(|ev| {
                        match ev.unwrap() {
                            Char(c) => c,
                            _ => panic!("found non-char in series of chars"),
                        }
                    }).collect();

                    if let Ok(off) = key_evs.parse::<usize>() {
                        tx.send(Event::Goto(off)).unwrap();
                    };
                },
                e => { tx.send(Event::Key(e)).unwrap() }
            }
        }
    });

    terminal.clear().unwrap();
    terminal.hide_cursor().unwrap();
    draw(&mut terminal, &vm, &interface);

    loop {
        let size = terminal.size().unwrap();
        if size != interface.size {
            terminal.resize(size).unwrap();
            interface.size = size;
            draw(&mut terminal, &vm, &interface);
        }


        use termion::event::Key::*;
        use interface::StepMode::*;
        use Event::*;

        let evt = match rx.recv() {
            Err(_) => break,
            Ok(e) => e,
        };

        match evt {
            Key(Char('q')) => {
                break;
            }
            Key(Char(' ')) => {
                interface.toggle_step();
            }
            Key(Char('<'))| Key(Char(',')) => {
                if interface.tabs.prev_tab() {
                    interface.pause();
                    terminal.resize(interface.size).unwrap();
                }
            }
            Key(Char('>')) | Key(Char('.')) => {
                if interface.tabs.next_tab() {
                    interface.pause();
                    // termion appears to have a bug which causes the UI to glitch out until it's resized
                    // so I just fake a resize to cause it to fix itself.
                    terminal.resize(interface.size).unwrap();
                }
            }
            Key(Down) => {
                interface.tabs.offsets[interface.tabs.selection] += 1;
            }
            Key(Up) => {
                if interface.tabs.offsets[interface.tabs.selection] > 0 {
                    interface.tabs.offsets[interface.tabs.selection] -= 1;
                }
            }
            Goto(off) => {
                interface.tabs.offsets[interface.tabs.selection] = off;
            }
            Tick if interface.step_mode == Run => {
                step_vm(&mut vm, &mut interface);
            }

            Key(Char('s')) if interface.step_mode == Step => {
                step_vm(&mut vm, &mut interface);
            }
            _ => { continue }
        }
        draw(&mut terminal, &vm, &interface);
    }

    terminal.show_cursor().unwrap();
}

fn step_vm(vm: &mut VM, app: &mut Interface) {
    match vm.step() {
        Err(e) => {
            error!("{:?}", e);
            vm.stopped = true; // vm should handle this internally
        }
        Ok(i) => {
            app.past_instrs.push_front(i);
        }
    }

    if vm.stopped { app.halt(); }
}

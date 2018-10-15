#![feature(nll)]

extern crate bit_field;
#[macro_use]
extern crate structopt;
#[macro_use]
extern crate clap;
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

mod float;
mod interface;
mod vm;

arg_enum! {
    #[derive(StructOpt, Debug)]
    enum Command {
        Run,
        Trace
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "besm-vm")]
struct Opts {
    #[structopt(name = "COMMAND", raw(possible_values = "&Command::variants()", case_insensitive = "true"))]
    command: Command,

    #[structopt(name = "FILE", parse(from_os_str))]
    is_file: PathBuf,

    #[structopt(short = "s", long = "start-address", default_value = "1")]
    start_address: u64,

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

fn runner_from_command(command: Command) -> impl for <'a> FnOnce(VM<'a>) -> () {
    match command {
        Command::Run   => run_with_interface,
        Command::Trace => trace_execution
    }
}

fn run_with_interface(mut vm: VM) {
    let mut terminal = Terminal::new(MouseBackend::new().unwrap()).unwrap();
    let mut interface = Interface {
        size: terminal.size().unwrap(),
        past_instrs: ArrayDeque::new(),
        step_mode: interface::StepMode::Step,
        tabs: interface::TabInfo::default(),
        breakpoint: None,
    };

    interface.run(&mut terminal, &mut vm);
}

fn trace_execution(mut vm: VM) {
    let mut previous_operator = 0;
    loop {

        use bit_field::BitField;
        let current_operator = vm.memory.get(vm.next_instr()).unwrap().get_bits(48..64);

        if previous_operator != current_operator {
            previous_operator = current_operator;
            let procedure = current_operator.get_bits(12..16);
            let operator = current_operator.get_bits(0..12);
            println!("{} {:3}", procedure, operator);
        }

        vm.step().unwrap();
    }
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

fn main() {
    init_logger(LevelFilter::Info).unwrap();
    set_default_level(LevelFilter::Trace);

    let opt = Opts::from_args();


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

    let mut mem = Memory::new(is_buf);
    let vm = VM::new(&mut mem, &mut x, &mut y, opt.start_address as u16);

    runner_from_command(opt.command)(vm);
}


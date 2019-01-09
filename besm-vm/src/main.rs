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

use std::path::Path;

use interface::*;
use vm::*;
use structopt::StructOpt;

use tui::backend::MouseBackend;
use tui::Terminal;

mod float;
mod interface;
mod vm;
mod opt;

fn runner_from_command(command: Command) -> impl for <'a> FnOnce(&mut VM<'a>) -> () {
    match command {
        Command::Run   => run_with_interface,
        Command::Trace => trace_execution
    }
}

fn run_with_interface(vm: &mut VM) {
    let mut terminal = Terminal::new(MouseBackend::new().unwrap()).unwrap();
    let mut interface = Interface::default();

    interface.run(&mut terminal, vm);
}

fn trace_execution(vm: &mut VM) {
    let mut previous_operator = 0;
    loop {
        if vm.stopped { break; }
        use bit_field::BitField;
        let current_operator = vm.memory.get(vm.next_instr()).unwrap().get_bits(48..64);

        if previous_operator != current_operator {
            previous_operator = current_operator;
            let procedure = current_operator.get_bits(12..16);
            let operator = current_operator.get_bits(0..12);
            println!("{} {:3} {}", procedure, operator, vm.next_instr());
        }

        vm.step().unwrap();
    }
}


extern crate termion;

use log::LevelFilter;
use tui_logger::*;
use opt::*;

fn main() {
    init_logger(LevelFilter::Info).unwrap();
    set_default_level(LevelFilter::Trace);

    let opt = Opts::from_args();

    let f = Path::new(&opt.is_file);
    let is_buf = is_from_file(f);

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
    let mut vm = VM::new(&mut mem, &mut x, &mut y, opt.start_address as u16);

    runner_from_command(opt.command)(&mut vm);

    file_from_md(opt.md0_out, vm.mag_system.mag_drives[0]);
    file_from_md(opt.md1_out, vm.mag_system.mag_drives[1]);
    file_from_md(opt.md2_out, vm.mag_system.mag_drives[2]);
    file_from_md(opt.md3_out, vm.mag_system.mag_drives[3]);
    file_from_md(opt.md4_out, vm.mag_system.mag_drives[4]);
}


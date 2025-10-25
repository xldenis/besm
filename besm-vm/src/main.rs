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

use crate::interface::*;
use structopt::StructOpt;
use crate::vm::*;

use std::io;
use termion::{input::MouseTerminal, raw::IntoRawMode, screen::AlternateScreen};
use tui::backend::TermionBackend;

use tui::Terminal;

mod float;
mod interface;
mod opt;
mod vm;

fn runner_from_command(command: Command) -> impl for<'a> FnOnce(&mut VM<'a>) -> () {
    match command {
        Command::Run => run_with_interface,
        Command::Trace => trace_execution,
    }
}

fn run_with_interface(vm: &mut VM) {
    let stdout = io::stdout().into_raw_mode().unwrap();
    let stdout = MouseTerminal::from(stdout);
    let stdout = AlternateScreen::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend).unwrap();

    let mut interface = Interface::default();

    interface.run(&mut terminal, vm);
}

fn trace_execution(vm: &mut VM) {
    let mut previous_operator = 0;
    loop {
        if vm.stopped {
            break;
        }
        use bit_field::BitField;
        let current_operator = vm.memory.get(vm.next_instr()).unwrap().get_bits(48..64);

        if previous_operator != current_operator {
            previous_operator = current_operator;
            let pass = current_operator.get_bits(14..16);
            let procedure = current_operator.get_bits(10..14);
            let operator = current_operator.get_bits(0..10);
            println!("PP{} {} {:3} {}", pass, procedure, operator, vm.next_instr());
        }

        vm.step().unwrap();
    }
}

extern crate termion;

use log::LevelFilter;
use crate::opt::*;
use tui_logger::*;

fn main() {
    init_logger(LevelFilter::Info).unwrap();
    set_default_level(LevelFilter::Trace);

    let opt = Opts::from_args();

    let mut is_buf = match &opt.is_file {
        Some(file) => is_from_file(file),
        None => [0u64; 1023],
    };

    let mut bootloader: [u64; 9] = [0u64; 9];
    if let Some(boot) = opt.bootloader {
        let words: Vec<u64> = read_file(&boot).take(9).collect();
        bootloader.copy_from_slice(&words[..])
    }

    let mut x = [
        md_from_file(opt.md0),
        md_from_file(opt.md1),
        md_from_file(opt.md2),
        md_from_file(opt.md3),
        md_from_file(opt.md4),
    ];

    use crate::vm::mag::MagTape;
    let mut y = [MagTape::new(), MagTape::new(), MagTape::new(), MagTape::new()];

    let mut mem = Memory::new_with_bootloader(&mut is_buf, &bootloader);
    let mut vm = VM::new(&mut mem, &mut x, &mut y, opt.start_address as u16);

    runner_from_command(opt.command)(&mut vm);

    file_from_md(opt.md0_out, vm.mag_system.mag_drives[0]);
    file_from_md(opt.md1_out, vm.mag_system.mag_drives[1]);
    file_from_md(opt.md2_out, vm.mag_system.mag_drives[2]);
    file_from_md(opt.md3_out, vm.mag_system.mag_drives[3]);
    file_from_md(opt.md4_out, vm.mag_system.mag_drives[4]);
}

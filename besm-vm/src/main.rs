use crate::vm::*;
use clap::Parser;
use interface::{Interface, OpBreakpoint};

use ratatui::backend::TermionBackend;
use std::io;
use termion::{input::MouseTerminal, raw::IntoRawMode, screen::AlternateScreen};

use ratatui::Terminal;

mod float;
mod interface;
mod opt;
mod vm;

fn run_with_interface(
    vm: &mut VM,
    breakpoint: Option<u16>,
    mem_breakpoint: Option<u16>,
    op_breakpoint: Option<OpBreakpoint>,
) {
    let stdout = io::stdout().into_raw_mode().unwrap();
    let stdout = MouseTerminal::from(stdout);
    let stdout = AlternateScreen::from(stdout);
    let backend = TermionBackend::new(stdout);
    let mut terminal = Terminal::new(backend).unwrap();

    let mut interface = Interface::with_breakpoints(breakpoint, mem_breakpoint, op_breakpoint);

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

use crate::opt::*;
use log::LevelFilter;
use tui_logger::*;

fn main() {
    init_logger(LevelFilter::Info).unwrap();
    set_default_level(LevelFilter::Trace);

    let opt = Opts::parse();

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

    // Convert CLI op_breakpoint tuple to OpBreakpoint struct
    let op_breakpoint = opt.op_breakpoint.map(|(pass, procedure, operator)| OpBreakpoint {
        pass,
        procedure,
        operator,
    });

    match opt.command {
        Command::Run => run_with_interface(&mut vm, opt.breakpoint, opt.mem_breakpoint, op_breakpoint),
        Command::Trace => trace_execution(&mut vm),
    }

    file_from_md(opt.md0_out, vm.mag_system.mag_drives[0]);
    file_from_md(opt.md1_out, vm.mag_system.mag_drives[1]);
    file_from_md(opt.md2_out, vm.mag_system.mag_drives[2]);
    file_from_md(opt.md3_out, vm.mag_system.mag_drives[3]);
    file_from_md(opt.md4_out, vm.mag_system.mag_drives[4]);
}

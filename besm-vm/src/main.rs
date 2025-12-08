use crate::vm::*;
use clap::Parser;
use interface::{Interface, OpBreakpoint, OpMetadata, extract_op_metadata};

use bit_field::BitField;
use ratatui::backend::TermionBackend;
use std::{collections::HashMap, io};
use termion::{input::MouseTerminal, raw::IntoRawMode, screen::AlternateScreen};
use vm::instruction::Instruction;

use ratatui::Terminal;

use crate::float::Float;
use crate::opt::MemoryRange;

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

fn print_memory_cell(vm: &VM, addr: u16) {
    if let Ok(word) = vm.memory.get(addr) {
        let float = Float::from_bytes(word);
        let active_bits = word.get_bits(0..39);
        let instr_str =
            Instruction::from_bytes(word).map(|i| format!("{}", i)).unwrap_or_else(|_| {
                format!(
                    "{} {} {} {}",
                    active_bits.get_bits(33..39),
                    active_bits.get_bits(22..33),
                    active_bits.get_bits(11..22),
                    active_bits.get_bits(0..11)
                )
            });

        println!(
            "{:4}  {:<20}  {:>12}  {:010x}  {:039b}",
            addr,
            instr_str,
            &format!("{float:>012}")[..12],
            active_bits,
            active_bits
        );
    }
}

fn print_memory_ranges(vm: &VM, ranges: &[MemoryRange]) {
    if ranges.is_empty() {
        return;
    }

    for (i, range) in ranges.iter().enumerate() {
        if i > 0 {
            println!();
        }
        for addr in range.start..=range.end {
            print_memory_cell(vm, addr);
        }
    }
}

fn trace_execution(
    vm: &mut VM,
    print_memory: &[MemoryRange],
    breakpoint: Option<u16>,
    mem_breakpoint: Option<u16>,
    op_breakpoint: Option<OpBreakpoint>,
    no_trace: bool,
) {
    let mut back_jumps = HashMap::new();

    let mut previous_operator = OpMetadata::default();
    let mut current_instr = 0;
    loop {
        if vm.stopped {
            break;
        }
        let instr_word = vm.memory.get(vm.next_instr()).unwrap();
        let current_operator = extract_op_metadata(instr_word);

        if previous_operator != current_operator {
            previous_operator = current_operator;
            if !no_trace {
                println!(
                    "PP{} {} {:3} {}",
                    current_operator.pass,
                    current_operator.procedure,
                    current_operator.operator,
                    vm.next_instr()
                );
            }
        }

        // Check operator breakpoint before stepping
        if let Some(op_bp) = &op_breakpoint {
            if current_operator.pass == op_bp.pass
                && current_operator.procedure == op_bp.procedure
                && current_operator.operator == op_bp.operator
            {
                eprintln!(
                    "Operator breakpoint triggered - Pass: {}, Procedure: {}, Operator: {}",
                    current_operator.pass, current_operator.procedure, current_operator.operator
                );
                print_memory_ranges(vm, print_memory);
                return;
            }
        }

        match vm.step() {
            Ok(step_result) => {
                // Check memory write breakpoint
                if let Some(mem_bp) = mem_breakpoint {
                    if let Some(ref write) = step_result.write {
                        if write.address == mem_bp {
                            eprintln!(
                                "Memory write breakpoint triggered at address {} (wrote {:016x})",
                                mem_bp, write.new_value
                            );
                            print_memory_ranges(vm, print_memory);
                            return;
                        }
                    }
                }

                // Check instruction address breakpoint
                if let Some(bp) = breakpoint {
                    if bp == vm.next_instr() {
                        eprintln!("Instruction breakpoint triggered at address {}", bp);
                        print_memory_ranges(vm, print_memory);
                        return;
                    }
                }

                if vm.next_instr() < current_instr {
                    let hash = vm.memory.hash();

                    if back_jumps.get(&vm.next_instr()) == Some(&hash) {
                        eprintln!("InfiniteLoop");
                        print_memory_ranges(vm, print_memory);
                        return;
                    }

                    back_jumps.insert(vm.next_instr(), hash);
                }

                current_instr = vm.next_instr();
            }
            Err(e) => {
                eprintln!("{e:?}");
                if let Ok(next) = vm.memory.get(vm.next_instr()) {
                    match Instruction::from_bytes(next) {
                        Ok(instr) => eprintln!("{}", instr),
                        Err(_) => eprintln!("(invalid instruction: {:016x})", next),
                    }
                }
                print_memory_ranges(vm, print_memory);
                return;
            }
        }
    }
    print_memory_ranges(vm, print_memory);
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
        Command::Run => {
            run_with_interface(&mut vm, opt.breakpoint, opt.mem_breakpoint, op_breakpoint)
        }
        Command::Trace => trace_execution(
            &mut vm,
            &opt.print_memory,
            opt.breakpoint,
            opt.mem_breakpoint,
            op_breakpoint,
            opt.no_trace,
        ),
    }

    file_from_md(opt.md0_out, vm.mag_system.mag_drives[0]);
    file_from_md(opt.md1_out, vm.mag_system.mag_drives[1]);
    file_from_md(opt.md2_out, vm.mag_system.mag_drives[2]);
    file_from_md(opt.md3_out, vm.mag_system.mag_drives[3]);
    file_from_md(opt.md4_out, vm.mag_system.mag_drives[4]);
}

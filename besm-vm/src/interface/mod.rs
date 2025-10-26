pub mod input;
pub mod tui;

use crate::interface::input::*;
pub use crate::interface::tui::*;

use crate::vm::{instruction::Instruction, VM};
use ::ratatui::{layout::Rect, Terminal};
use arraydeque::{behavior::Wrapping, *};
use log::error;
use ratatui::layout::Size;

use ::ratatui::backend::Backend;
use std::{collections::HashMap, sync::mpsc::*};

const WRITE_DECAY_STEPS: u32 = 5;

// This should be split into two structures, so that input can fully happen in a separate thread.
pub struct Interface {
    pub size: Size,
    pub past_instrs: ArrayDeque<[Instruction; 100], Wrapping>,
    pub step_mode: StepMode,
    pub tabs: TabInfo,
    pub breakpoint: Option<u16>,
    pub recent_writes: HashMap<u16, (u32, u64)>, // address -> (age, old_value)
    pub printer_output: Vec<String>,
    exiting: bool,
    mem_breakpoint: Option<u16>,
}

impl Interface {
    pub fn default() -> Interface {
        Interface {
            size: Size::default(),
            past_instrs: ArrayDeque::new(),
            step_mode: StepMode::Step,
            tabs: TabInfo::default(),
            breakpoint: None,
            recent_writes: HashMap::new(),
            printer_output: Vec::new(),
            exiting: false,
            mem_breakpoint: None,
        }
    }

    pub fn toggle_step(&mut self) {
        use StepMode::*;
        self.step_mode = match self.step_mode {
            Run => Step,
            Step => Run,
            Stop => Stop,
        };
    }

    pub fn halt(&mut self) {
        self.step_mode = StepMode::Stop;
    }

    pub fn pause(&mut self) {
        self.step_mode = StepMode::Step;
    }

    pub fn run<T: Backend>(&mut self, terminal: &mut Terminal<T>, vm: &mut VM) {
        let rx = setup_input_stream();

        terminal.clear().unwrap();
        terminal.hide_cursor().unwrap();
        draw(terminal, &vm, &self);

        while !self.exiting {
            let size = terminal.size().unwrap();

            if size != self.size {
                terminal.resize(Rect::new(0, 0, size.width, size.height)).unwrap();
                self.size = size;
                draw(terminal, &vm, &self);
            }

            let need_draw = self.handle_input(vm, &rx);

            if need_draw {
                draw(terminal, &vm, &self);
            }
        }

        terminal.show_cursor().unwrap();
    }

    fn handle_input(&mut self, vm: &mut VM, rx: &Receiver<Event>) -> bool {
        use self::StepMode::*;
        use termion::event::Key::*;

        let evt = match rx.recv() {
            Err(_) => {
                self.exiting = true;
                return false;
            }
            Ok(e) => e,
        };

        match evt {
            Key(Char('q')) => {
                self.exiting = true;
            }
            Key(Char(' ')) if self.step_mode == Step => {
                self.toggle_step();
                step_vm(vm, self);
            }
            Key(Char(' ')) => self.toggle_step(),
            Key(Char('<')) | Key(Char(',')) => {
                if self.tabs.prev_tab() {
                    self.pause();
                }
            }
            Key(Char('>')) | Key(Char('.')) => {
                if self.tabs.next_tab() {
                    self.pause();
                }
            }
            Key(Down) => {
                self.tabs.offsets[self.tabs.selection] += 1;
            }
            Key(Up) => {
                if self.tabs.offsets[self.tabs.selection] > 0 {
                    self.tabs.offsets[self.tabs.selection] -= 1;
                }
            }
            Command('g', off) => {
                self.tabs.offsets[self.tabs.selection] = off;
            }
            Command('b', off) => {
                if off == 0 {
                    self.breakpoint = None;
                } else {
                    self.breakpoint = Some(off as u16);
                }
            }
            Command('m', off) => {
                if off == 0 {
                    self.mem_breakpoint = None;
                } else {
                    self.mem_breakpoint = Some(off as u16);
                }
            }
            Tick if self.step_mode == Run => {
                step_vm(vm, self);
            }

            Key(Char('s')) if self.step_mode == Step => {
                step_vm(vm, self);
            }
            Key(Char('r')) => {
                for _ in 0..1000 {
                    step_vm(vm, self);
                    if vm.stopped {
                        break;
                    }
                }
            }
            _ => return false,
        }

        return true;
    }
}

fn step_vm(vm: &mut VM, app: &mut Interface) {
    let step_result = match vm.step() {
        Ok(res) => res,
        Err(e) => {
            error!("{:?}", e);
            vm.stopped = true; // vm should handle this internally
            return;
        }
    };

    app.past_instrs.push_front(step_result.instruction);

    // Update write tracking
    if let Some(write) = step_result.write {
        app.recent_writes.insert(write.address, (0, write.old_value));
    }

    // Capture printer output
    if let Some(output) = step_result.printer_output {
        app.printer_output.push(output);
    }

    // Age existing writes and remove old ones
    app.recent_writes.retain(|_, (age, _)| {
        *age += 1;
        *age <= WRITE_DECAY_STEPS
    });

    if let Some(b) = app.mem_breakpoint {
        if let Some(out) = step_result.write
            && out.address == b
        {
            app.pause();
        }
    }

    if let Some(b) = app.breakpoint {
        if b == vm.next_instr() {
            app.pause();
        }
    }

    if vm.stopped {
        app.halt();
    }
}

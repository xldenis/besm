pub mod tui;
pub mod input;

pub use interface::tui::*;
use interface::input::*;

use arraydeque::behavior::Wrapping;
use arraydeque::*;
use vm::instruction::Instruction;
use tui::layout::Rect;
use vm::VM;
use tui::terminal::Terminal;

use std::sync::mpsc::*;
use tui::backend::Backend;

// This should be split into two structures, so that input can fully happen in a separate thread.
pub struct Interface {
    pub size: Rect,
    pub past_instrs: ArrayDeque<[Instruction; 100], Wrapping>,
    pub step_mode: StepMode,
    pub tabs: TabInfo,
    pub breakpoint: Option<u16>,
    exiting: bool,
}

impl Interface {
    pub fn default() -> Interface {
        Interface {
            size: Rect::default(),
            past_instrs: ArrayDeque::new(),
            step_mode: StepMode::Step,
            tabs: TabInfo::default(),
            breakpoint: None,
            exiting: false,
        }
    }

    pub fn toggle_step(&mut self) {
        use StepMode::*;
        self.step_mode = match self.step_mode {
            Run  => Step,
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

    pub fn run<T : Backend >(&mut self, terminal: &mut Terminal<T>, vm: &mut VM) {
        let rx = setup_input_stream();

        terminal.clear().unwrap();
        terminal.hide_cursor().unwrap();

        draw(terminal, &vm, &self);

        while !self.exiting {
            let size = terminal.size().unwrap();

            if size != self.size {
                terminal.resize(size).unwrap();
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
        use termion::event::Key::*;
        use self::StepMode::*;

        let evt = match rx.recv() {
            Err(_) => { self.exiting = true; return false },
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
            Key(Char(' ')) => { self.toggle_step() }
            Key(Char('<')) | Key(Char(',')) => {
                if self.tabs.prev_tab() {
                    self.pause();
                    self.size = Rect::default();
                }
            }
            Key(Char('>')) | Key(Char('.')) => {
                if self.tabs.next_tab() {
                    self.pause();
                    self.size = Rect::default();
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
            _ => { return false }
        }

        return true
    }
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

    if let Some(b) = app.breakpoint {
        if b == vm.next_instr() {
            app.pause();
        }
    }

    if vm.stopped { app.halt(); }
}



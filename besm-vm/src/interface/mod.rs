pub mod tui;

pub use interface::tui::*;

use arraydeque::behavior::Wrapping;
use arraydeque::*;
use vm::instruction::Instruction;
use tui::layout::Rect;
use vm::VM;
use tui::terminal::Terminal;

use std::sync::mpsc::*;
use tui::backend::Backend;

pub struct Interface {
    pub size: Rect,
    pub past_instrs: ArrayDeque<[Instruction; 50], Wrapping>,
    pub step_mode: StepMode,
    pub tabs: TabInfo,
}

use termion::event;

#[derive(Debug)]
enum Event {
    Key(event::Key),
    Tick,
    Goto(usize),
}

use self::Event::*;

impl Interface {
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

        loop {
            let size = terminal.size().unwrap();
            if size != self.size {
                terminal.resize(size).unwrap();
                self.size = size;
                draw(terminal, &vm, &self);
            }

            use termion::event::Key::*;
            use self::StepMode::*;

            let evt = match rx.recv() {
                Err(_) => break,
                Ok(e) => e,
            };

            match evt {
                Key(Char('q')) => {
                    break;
                }
                Key(Char(' ')) => {
                    self.toggle_step();
                }
                Key(Char('<'))| Key(Char(',')) => {
                    if self.tabs.prev_tab() {
                        self.pause();
                        terminal.resize(self.size).unwrap();
                    }
                }
                Key(Char('>')) | Key(Char('.')) => {
                    if self.tabs.next_tab() {
                        self.pause();
                        // termion appears to have a bug which causes the UI to glitch out until it's resized
                        // so I just fake a resize to cause it to fix itself.
                        terminal.resize(self.size).unwrap();
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
                Goto(off) => {
                    self.tabs.offsets[self.tabs.selection] = off;
                }
                Tick if self.step_mode == Run => {
                    step_vm(vm, self);
                }

                Key(Char('s')) if self.step_mode == Step => {
                    step_vm(vm, self);
                }
                _ => { continue }
            }
            draw(terminal, &vm, &self);
        }

        terminal.show_cursor().unwrap();
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

    if vm.stopped { app.halt(); }
}

fn setup_input_stream() -> Receiver<Event> {
    use std::{thread, time};
    use std::io;

    let (tx, rx) = channel();

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
    return rx;
}

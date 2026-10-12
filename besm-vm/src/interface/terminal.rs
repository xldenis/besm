use std::{
    io,
    sync::mpsc::{Receiver, RecvTimeoutError, channel},
    thread,
    time::{Duration, Instant},
};

use ratatui::{Terminal, backend::Backend, layout::Rect};
use termion::{event, input::TermRead};

use crate::interface::{Interface, Key, draw};
use crate::vm::VM;

const TICK: Duration = Duration::from_millis(25);

fn convert_key(key: event::Key) -> Option<Key> {
    use termion::event::Key::*;

    match key {
        Char('\n') | Char('\r') => Some(Key::Enter),
        Char(c) => Some(Key::Char(c)),
        Esc => Some(Key::Esc),
        Backspace => Some(Key::Backspace),
        Up => Some(Key::Up),
        Down => Some(Key::Down),
        _ => None,
    }
}

// Reading stdin blocks, so it runs on its own thread.
fn spawn_key_reader() -> Receiver<Key> {
    let (tx, rx) = channel();

    thread::spawn(move || {
        for key in io::stdin().keys() {
            let Some(key) = key.ok().and_then(convert_key) else { continue };
            if tx.send(key).is_err() {
                break;
            }
        }
    });

    rx
}

impl Interface {
    pub fn run<T: Backend>(&mut self, terminal: &mut Terminal<T>, vm: &mut VM) {
        let keys = spawn_key_reader();

        terminal.clear().unwrap();
        terminal.hide_cursor().unwrap();
        draw(terminal, vm, self);

        let mut next_tick = Instant::now() + TICK;

        while !self.exiting() {
            let size = terminal.size().unwrap();

            if size != self.size {
                terminal.resize(Rect::new(0, 0, size.width, size.height)).unwrap();
                self.size = size;
                draw(terminal, vm, self);
            }

            let need_draw =
                match keys.recv_timeout(next_tick.saturating_duration_since(Instant::now())) {
                    Ok(key) => match self.map_key(key) {
                        Some(action) => {
                            self.update(vm, action);
                            true
                        }
                        None => false,
                    },
                    Err(RecvTimeoutError::Timeout) => {
                        next_tick = Instant::now() + TICK;
                        self.tick(vm)
                    }
                    Err(RecvTimeoutError::Disconnected) => break,
                };

            if need_draw {
                draw(terminal, vm, self);
            }
        }

        terminal.show_cursor().unwrap();
    }
}

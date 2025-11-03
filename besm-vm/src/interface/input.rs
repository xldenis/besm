use std::sync::mpsc::*;
use termion::event;

#[derive(Debug)]
pub enum Event {
    Key(event::Key),
    Tick,
    Command(char, usize),
    OpCommand(u16, u16, u16), // (pass, procedure, operator)
}

pub use self::Event::*;

pub fn setup_input_stream() -> Receiver<Event> {
    use std::{io, thread, time};

    let (tx, rx) = channel();

    let tx2 = tx.clone();
    thread::spawn(move || {
        let speed = time::Duration::from_millis(25);
        loop {
            tx2.send(Event::Tick).unwrap();
            thread::sleep(speed);
        }
    });

    thread::spawn(move || {
        use termion::{event::Key::*, input::TermRead};

        let stdin = io::stdin();

        for e in stdin.keys() {
            let evt = e.unwrap();
            match evt {
                Char(x @ 'g') | Char(x @ 'b') | Char(x @ 'm') => {
                    if let Ok(off) = read_address() {
                        tx.send(Event::Command(x, off as usize)).unwrap();
                    };
                }
                Char('o') => {
                    if let Ok((pass, proc, op)) = read_operator_breakpoint() {
                        tx.send(Event::OpCommand(pass, proc, op)).unwrap();
                    };
                }
                e => tx.send(Event::Key(e)).unwrap(),
            }
        }
    });
    return rx;
}

use std::num::ParseIntError;

fn read_address() -> Result<u16, ParseIntError> {
    use std::io;
    use termion::{event::Key::*, input::TermRead};

    let key_evs: String = io::stdin()
        .keys()
        .take_while(|ev| match ev {
            Ok(Char('\n')) => false,
            Ok(Char(_)) => true,
            _ => false,
        })
        .map(|ev| match ev.unwrap() {
            Char(c) => c,
            _ => panic!("found non-char in series of chars"),
        })
        .collect();
    key_evs.parse::<u16>()
}

fn read_operator_breakpoint() -> Result<(u16, u16, u16), ParseIntError> {
    use std::io;
    use termion::{event::Key::*, input::TermRead};

    let key_evs: String = io::stdin()
        .keys()
        .take_while(|ev| match ev {
            Ok(Char('\n')) => false,
            Ok(Char(_)) => true,
            _ => false,
        })
        .map(|ev| match ev.unwrap() {
            Char(c) => c,
            _ => panic!("found non-char in series of chars"),
        })
        .collect();

    // Parse format: "pass-procedure-operator" or "pass procedure operator"
    let parts: Vec<&str> = if key_evs.contains('-') {
        key_evs.split('-').collect()
    } else {
        key_evs.split_whitespace().collect()
    };

    if parts.len() == 3 {
        let pass = parts[0].parse::<u16>()?;
        let procedure = parts[1].parse::<u16>()?;
        let operator = parts[2].parse::<u16>()?;
        Ok((pass, procedure, operator))
    } else if parts.len() == 1 && parts[0] == "0" {
        Ok((0, 0, 0))
    } else {
        "invalid".parse::<u16>().map(|_| (0, 0, 0))
    }
}

use termion::event;
use std::sync::mpsc::*;


#[derive(Debug)]
pub enum Event {
    Key(event::Key),
    Tick,
    Command(char, usize)
}

pub use self::Event::*;


pub fn setup_input_stream() -> Receiver<Event> {
    use std::{thread, time};
    use std::io;

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
        use termion::input::TermRead;
        use termion::event::Key::*;

        let stdin = io::stdin();

        for e in stdin.keys() {
            let evt = e.unwrap();
            match evt {
                Char(x@'g') | Char(x@'b') => {
                    if let Ok(off) = read_address() {
                        tx.send(Event::Command(x, off as usize)).unwrap();
                    };
                },
                e => { tx.send(Event::Key(e)).unwrap() }
            }
        }
    });
    return rx;
}

use std::num::ParseIntError;

fn read_address() -> Result<u16, ParseIntError> {
    use termion::event::Key::*;
    use termion::input::TermRead;
    use std::io;

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
    key_evs.parse::<u16>()
}

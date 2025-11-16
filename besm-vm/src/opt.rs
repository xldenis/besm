use std::{fs::File, path::PathBuf};
// use std::iter;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use clap::{Parser, ValueEnum};

#[derive(ValueEnum, Debug, Clone)]
pub enum Command {
    Run,
    Trace,
}

#[derive(Parser, Debug)]
pub struct Opts {
    #[arg(name = "COMMAND")]
    pub command: Command,

    #[arg(name = "FILE")]
    pub is_file: Option<PathBuf>,

    #[arg(short = 's', long = "start-address", default_value = "1")]
    pub start_address: u64,

    #[arg(long = "bootloader")]
    pub bootloader: Option<PathBuf>,

    #[arg(long = "md0")]
    pub md0: Option<PathBuf>,
    #[arg(long = "md1")]
    pub md1: Option<PathBuf>,
    #[arg(long = "md2")]
    pub md2: Option<PathBuf>,
    #[arg(long = "md3")]
    pub md3: Option<PathBuf>,
    #[arg(long = "md4")]
    pub md4: Option<PathBuf>,

    #[arg(long = "md0-out")]
    pub md0_out: Option<PathBuf>,
    #[arg(long = "md1-out")]
    pub md1_out: Option<PathBuf>,
    #[arg(long = "md2-out")]
    pub md2_out: Option<PathBuf>,
    #[arg(long = "md3-out")]
    pub md3_out: Option<PathBuf>,
    #[arg(long = "md4-out")]
    pub md4_out: Option<PathBuf>,

    #[arg(short = 'b', long = "breakpoint", help = "Set instruction address breakpoint")]
    pub breakpoint: Option<u16>,

    #[arg(long = "mem-breakpoint", help = "Set memory write breakpoint at address")]
    pub mem_breakpoint: Option<u16>,

    #[arg(long = "op-breakpoint", value_parser = parse_op_breakpoint, value_name = "PASS:PROCEDURE:OPERATOR", help = "Set operator breakpoint (format: PASS:PROCEDURE:OPERATOR)")]
    pub op_breakpoint: Option<(u16, u16, u16)>,
}

use crate::vm::mag::MagDrive;

pub fn md_from_file(file: Option<PathBuf>) -> MagDrive {
    let mut buf = [0; 1024];

    match file {
        None => {}
        Some(f) => {
            let words: Vec<u64> = read_file(&f).take(1024).collect();
            buf.copy_from_slice(&words[..])
        }
    }

    MagDrive::new(buf)
}

pub fn file_from_md(file: Option<PathBuf>, md: MagDrive) -> () {
    if let Some(path) = file {
        File::create(path)
            .map(|mut f| {
                for x in md.drive.iter() {
                    f.write_u64::<BigEndian>(*x).unwrap();
                }
            })
            .unwrap();
    }
}

use std::{ffi::OsStr, path::Path};

struct IntReader<R> {
    src: R,
}
use std::{self, io::Read};

impl<R: Read> Iterator for IntReader<R> {
    type Item = std::io::Result<u64>;

    fn next(&mut self) -> Option<std::io::Result<u64>> {
        match self.src.read_u64::<BigEndian>() {
            Ok(v) => Some(Ok(v)),
            Err(ref e) if e.kind() == std::io::ErrorKind::UnexpectedEof => None,
            e @ Err(_) => Some(e),
        }
    }
}
pub fn is_from_file(path: &Path) -> [u64; 1023] {
    let mut is_buf = [0u64; 1023];

    let words: Vec<u64> = read_file(path).take(1023).collect();
    is_buf.copy_from_slice(&words[..]);
    is_buf
}

pub fn read_file(path: &Path) -> Box<dyn Iterator<Item = u64>> {
    let file = Box::new(File::open(path).expect("file not found"));
    use std::io::{BufRead, BufReader};
    let buf = BufReader::new(*file);

    match path.extension().and_then(OsStr::to_str) {
        Some("txt") => {
            let words = buf
                .lines()
                .map(|line| {
                    line.as_ref().map(|l| u64::from_str_radix(l, 16).unwrap_or(0)).unwrap_or(0)
                })
                .chain(std::iter::repeat(0));

            return Box::new(words);
        }
        Some("bin") => {
            let words = IntReader { src: buf }.map(|r| r.unwrap_or(0)).chain(std::iter::repeat(0));
            return Box::new(words);
        }
        _ => {
            panic!("unsupported is file type.");
        }
    }
}

fn parse_op_breakpoint(s: &str) -> Result<(u16, u16, u16), String> {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() != 3 {
        return Err(format!("Expected format PASS:PROCEDURE:OPERATOR, got '{}'", s));
    }

    let pass =
        parts[0].parse::<u16>().map_err(|_| format!("Invalid pass value: '{}'", parts[0]))?;
    let procedure =
        parts[1].parse::<u16>().map_err(|_| format!("Invalid procedure value: '{}'", parts[1]))?;
    let operator =
        parts[2].parse::<u16>().map_err(|_| format!("Invalid operator value: '{}'", parts[2]))?;

    Ok((pass, procedure, operator))
}

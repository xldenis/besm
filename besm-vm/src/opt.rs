use std::{fs::File, path::PathBuf};
// use std::iter;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};

arg_enum! {
    #[derive(StructOpt, Debug)]
    pub enum Command {
        Run,
        Trace
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "besm-vm")]
pub struct Opts {
    #[structopt(
        name = "COMMAND",
        raw(possible_values = "&Command::variants()", case_insensitive = "true")
    )]
    pub command: Command,

    #[structopt(name = "FILE", parse(from_os_str))]
    pub is_file: Option<PathBuf>,

    #[structopt(short = "s", long = "start-address", default_value = "1")]
    pub start_address: u64,

    #[structopt(long = "bootloader", parse(from_os_str))]
    pub bootloader: Option<PathBuf>,

    #[structopt(long = "md0", parse(from_os_str))]
    pub md0: Option<PathBuf>,
    #[structopt(long = "md1", parse(from_os_str))]
    pub md1: Option<PathBuf>,
    #[structopt(long = "md2", parse(from_os_str))]
    pub md2: Option<PathBuf>,
    #[structopt(long = "md3", parse(from_os_str))]
    pub md3: Option<PathBuf>,
    #[structopt(long = "md4", parse(from_os_str))]
    pub md4: Option<PathBuf>,

    #[structopt(long = "md0-out", parse(from_os_str))]
    pub md0_out: Option<PathBuf>,
    #[structopt(long = "md1-out", parse(from_os_str))]
    pub md1_out: Option<PathBuf>,
    #[structopt(long = "md2-out", parse(from_os_str))]
    pub md2_out: Option<PathBuf>,
    #[structopt(long = "md3-out", parse(from_os_str))]
    pub md3_out: Option<PathBuf>,
    #[structopt(long = "md4-out", parse(from_os_str))]
    pub md4_out: Option<PathBuf>,
}

use vm::mag::MagDrive;

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

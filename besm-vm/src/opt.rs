use std::fs::File;
use std::path::PathBuf;
use std::iter;

use byteorder::{BigEndian, ReadBytesExt};

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
    #[structopt(name = "COMMAND", raw(possible_values = "&Command::variants()", case_insensitive = "true"))]
    pub command: Command,

    #[structopt(name = "FILE", parse(from_os_str))]
    pub is_file: PathBuf,

    #[structopt(short = "s", long = "start-address", default_value = "1")]
    pub start_address: u64,

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
}

use vm::mag::MagDrive;

pub fn md_from_file(file: Option<PathBuf>) -> MagDrive {
    let mut buf = [0; 1024];
    match file {
        None => {}
        Some(path) => {
            let mut f = File::open(path).expect("file not found");
            let words: Vec<u64> = iter::repeat_with(|| f.read_u64::<BigEndian>().unwrap_or(0))
            .take(1024)
            .collect();

            buf.copy_from_slice(&words);
        }
    }

    MagDrive::new(buf)
}

use std::path::Path;
use std::ffi::OsStr;

pub fn is_from_file(path: &Path) -> [u64; 1023] {
    let mut file = File::open(path).expect("file not found");
    let mut is_buf = [0u64; 1023];

    match path.extension().and_then(OsStr::to_str) {
        Some("txt") => {
            use std::io::BufReader;
            use std::io::BufRead;
            let buf = BufReader::new(&file);

            let words: Vec<u64> = buf.lines()
            .map(|line|
                line.as_ref().map(|l|
                    u64::from_str_radix(l, 16).unwrap_or(0)
                ).unwrap_or(0)
            ).take(1023)
            .collect();

            is_buf.copy_from_slice(&words[..]);
        }
        Some("bin") => {
           let words: Vec<u64> = iter::repeat_with(|| file.read_u64::<BigEndian>().unwrap_or(0))
           .take(1023)
           .collect();

           is_buf.copy_from_slice(&words[..]);
       }
       _ => { panic!("unsupported is file type.");}
   }
   is_buf
}

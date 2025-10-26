#[derive(Debug)]
pub enum DriveError {
    TapeGroupTooLarge,
    InvalidDriveSpan,
    InvalidDriveOperation,
    OperationNotSupported,
}

#[derive(Copy, Clone)]
pub struct MagDrive {
    pub drive: [u64; 1024],
}
use std::collections::HashMap;

#[derive(Clone)]
pub struct MagTape {
    /*
    This may seem like a weird representation for a tape.
    However, since according to the assembly instruction reference we can write
    up to 64 blocks of arbitrary length, it appears like it had a block-marker
    channel to track the start of each data-block. Storing it as a HashMap is
    conceptually much simpler than other formats.
    */
    tape: HashMap<u8, Vec<u64>>,
}

impl MagDrive {
    pub fn new(vals: [u64; 1024]) -> MagDrive {
        MagDrive { drive: vals }
    }
}

pub struct Iter {
    drive: MagDrive,
    index: usize,
}

impl IntoIterator for MagDrive {
    type Item = u64;
    type IntoIter = Iter;

    fn into_iter(self) -> Self::IntoIter {
        Iter { drive: self, index: 0 }
    }
}

impl Iterator for Iter {
    type Item = u64;

    fn next(&mut self) -> Option<u64> {
        let result = match self.drive.drive.get(self.index) {
            Some(o) => Some(*o),
            None => None,
        };

        self.index += 1;

        result
    }
}

impl<'a> MagTape {
    pub fn new() -> MagTape {
        MagTape { tape: HashMap::new() }
    }
}

pub struct MagSystem<'a> {
    pub mag_drives: &'a mut [MagDrive; 5],
    pub mag_tapes: &'a mut [MagTape; 4],
}

use log::info;

use crate::vm::Memory;

impl<'a> MagSystem<'a> {
    pub fn perform_operation(
        &mut self,
        op: &DriveOperation,
        memory: &mut Memory,
    ) -> Result<(), DriveError> {
        use crate::vm::DriveOperation::*;

        match op {
            WriteMD(id, n1, c, n2) => {
                let span = (n2 + 1).checked_sub(*n1).ok_or(DriveError::InvalidDriveSpan)?;
                let drive = &mut self.mag_drives[id.to_num() as usize];

                let dest = drive.drive.iter_mut().skip(*n1 as usize);
                let source = memory.into_iter().skip((c - 1) as usize);

                for (place, data) in dest.zip(source).take(span as usize) {
                    *place = data;
                }
                info!("Wrote cells {}-{} (len {}) to MD-{:?} from IS {}", n1, n2, span, id, c);
            }
            ReadMD(id, n1, c, n2) => {
                let span = (n2 + 1).checked_sub(*n1).ok_or(DriveError::InvalidDriveSpan)?;

                let drive = self.mag_drives[id.to_num() as usize];

                let source = drive.into_iter().skip(*n1 as usize);
                let dest = memory.iter_mut().skip((*c - 1) as usize);

                for (place, data) in dest.zip(source).take(span as usize) {
                    *place = data;
                }
                info!("Read cells {}-{} (len {}) from MD-{:?} to IS {}", n1, n2, span, id, c);
            }

            ReadMT(id, r, c, n2) => {
                let tape = &mut self.mag_tapes[id.to_num() as usize];

                if *r > 63 {
                    return Err(DriveError::TapeGroupTooLarge);
                }

                let range = memory.into_iter().skip(*c as usize).take(*n2 as usize).collect(); //[(*c as usize) .. ((c + n2) as usize)].to_vec();
                tape.tape.insert(*r as u8, range);
            }
            _ => return Err(DriveError::OperationNotSupported),
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum DrumId {
    Zero,
    One,
    Two,
    Three,
    Four,
}

#[derive(Debug, Clone)]
pub enum TapeId {
    One,
    Two,
    Three,
    Four,
}

impl DrumId {
    pub fn to_num(&self) -> u8 {
        match self {
            DrumId::Zero => 0,
            DrumId::One => 1,
            DrumId::Two => 2,
            DrumId::Three => 3,
            DrumId::Four => 4,
        }
    }
}

impl TapeId {
    pub fn to_num(&self) -> u8 {
        match self {
            TapeId::One => 1,
            TapeId::Two => 2,
            TapeId::Three => 3,
            TapeId::Four => 4,
        }
    }
}

fn drum_id_from_num(ix: u16) -> DrumId {
    match ix {
        0 => DrumId::Zero,
        1 => DrumId::One,
        2 => DrumId::Two,
        3 => DrumId::Three,
        4 => DrumId::Four,
        _ => panic!("bad drum id"),
    }
}

fn tape_id_from_num(ix: u16) -> TapeId {
    match ix {
        1 => TapeId::One,
        2 => TapeId::Two,
        3 => TapeId::Three,
        4 => TapeId::Four,
        _ => panic!("bad tape id"),
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum DriveOperation {
    WriteMD(DrumId, u16, u16, u16),
    ReadMD(DrumId, u16, u16, u16),
    ReadTape(u16),
    WriteMT(TapeId, u16, u16, u16),
    ReadMT(TapeId, u16, u16, u16),
    RewindMT(TapeId, u16),
}

impl DriveOperation {
    pub fn from_ma(a: u16, b: u16, c: u16, b2: u16) -> Result<DriveOperation, DriveError> {
        use crate::vm::DriveOperation::*;
        match a {
            0x0300..=0x0304 => Ok(WriteMD(drum_id_from_num(a - 0x0300), b, c, b2)),
            0x0100..=0x0104 => Ok(ReadMD(drum_id_from_num(a - 0x0100), b, c, b2)),
            0x0080 => Ok(ReadTape(c)),
            0x0281..=0x0284 => Ok(WriteMT(tape_id_from_num(a), b, c, b2)),
            0x0081..=0x0084 => Ok(ReadMT(tape_id_from_num(a), b, c, b2)),
            0x00C1..=0x00C4 => Ok(RewindMT(tape_id_from_num(a), b)),
            _ => Err(DriveError::InvalidDriveOperation),
        }
    }
}

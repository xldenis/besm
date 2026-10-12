use std::collections::{HashMap, VecDeque};

use crate::float::Float;
use crate::interface::OpMetadata;
use crate::vm::{ActiveIC, StepResult, VMError, mag::DriveOperation};
use bit_field::BitField;

const TRACE_CAPACITY: usize = 20_000;

#[derive(Debug, Clone, Copy)]
pub struct Patch {
    pub step: u64,
    pub writer: u16,
    pub old_value: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum EntryKind {
    Normal,
    Patched,
    Halt,
    Error,
}

pub struct TraceEntry {
    pub step: u64,
    pub addr: u16,
    pub local: bool,
    pub op: OpMetadata,
    // Ignores detours through DS routines.
    pub op_changed: bool,
    pub instr: String,
    pub effect: String,
    pub kind: EntryKind,
    haystack: String,
}

impl TraceEntry {
    fn new(
        step: u64,
        addr: u16,
        local: bool,
        op: OpMetadata,
        op_changed: bool,
        instr: String,
        effect: String,
        kind: EntryKind,
    ) -> TraceEntry {
        let haystack = format!(
            "{step} {addr:04} {} {} {instr} {effect}",
            if local { "L" } else { "G" },
            op_label(op)
        )
        .to_lowercase();
        TraceEntry { step, addr, local, op, op_changed, instr, effect, kind, haystack }
    }
}

pub fn op_label(op: OpMetadata) -> String {
    format!("{}-{}-{:03}", op.pass, op.procedure, op.operator)
}

pub struct PendingStep {
    addr: u16,
    ic: ActiveIC,
    op: OpMetadata,
    word: u64,
}

pub struct Trace {
    pub entries: VecDeque<TraceEntry>,
    pub steps: u64,
    patches: HashMap<u16, Patch>,
    last_op: OpMetadata,
}

impl Trace {
    pub fn new() -> Trace {
        Trace {
            entries: VecDeque::with_capacity(TRACE_CAPACITY),
            steps: 0,
            patches: HashMap::new(),
            last_op: OpMetadata::default(),
        }
    }

    pub fn begin(&self, addr: u16, ic: ActiveIC, word: u64) -> PendingStep {
        PendingStep { addr, ic, op: crate::interface::extract_op_metadata(word), word }
    }

    pub fn finish(
        &mut self,
        pending: PendingStep,
        result: &Result<StepResult, VMError>,
        next_addr: u16,
        next_ic: ActiveIC,
        stopped: bool,
    ) {
        self.steps += 1;
        let PendingStep { addr, ic, op, word } = pending;

        let mut kind = EntryKind::Normal;
        let mut effects = Vec::new();

        // Only the first execution after a patch is flagged.
        if let Some(patch) = self.patches.remove(&addr)
            && patch.old_value.get_bits(0..39) != word.get_bits(0..39)
        {
            kind = EntryKind::Patched;
            effects.push(format!(
                "PATCHED by {:04} @{} (was {})",
                patch.writer,
                patch.step,
                fmt_instr(patch.old_value)
            ));
        }

        let instr = match result {
            Ok(res) => res.instruction.to_string(),
            Err(_) => fmt_instr(word),
        };

        match result {
            Ok(res) => {
                if let Some(w) = res.write {
                    effects.push(format!(
                        "{:04} ← {} [{:010x}]",
                        w.address,
                        fmt_number(Float::from_bytes(w.new_value).to_float()),
                        w.new_value.get_bits(0..39)
                    ));
                    let old_value =
                        self.patches.get(&w.address).map_or(w.old_value, |p| p.old_value);
                    self.patches
                        .insert(w.address, Patch { step: self.steps, writer: addr, old_value });
                }

                if let Some(op) = &res.drum_transfer {
                    effects.push(self.drum_effect(op));
                }

                if let Some(out) = &res.printer_output {
                    effects.push(format!("PRINT {out}"));
                }

                if stopped {
                    kind = EntryKind::Halt;
                    effects.push("STOP".into());
                } else if next_ic != ic {
                    let tag = if next_ic == ActiveIC::Local { "L" } else { "G" };
                    effects.push(format!("→ {tag}{next_addr:04}"));
                } else if next_addr != addr.wrapping_add(1) {
                    effects.push(format!("→ {next_addr:04}"));
                }
            }
            Err(e) => {
                kind = EntryKind::Error;
                effects.push(format!("ERROR {e:?}"));
            }
        }

        let op_changed = op != OpMetadata::default() && op != self.last_op;
        if op_changed {
            self.last_op = op;
        }
        let entry = TraceEntry::new(
            self.steps,
            addr,
            ic == ActiveIC::Local,
            op,
            op_changed,
            instr,
            effects.join("  "),
            kind,
        );

        if self.entries.len() == TRACE_CAPACITY {
            self.entries.pop_front();
        }
        self.entries.push_back(entry);
    }

    // Cells reloaded from a drum are no longer patched.
    fn drum_effect(&mut self, op: &DriveOperation) -> String {
        use DriveOperation::*;
        match op {
            ReadMD(id, n1, c, n2) => {
                let end = c + n2.saturating_sub(*n1);
                self.patches.retain(|a, _| !(*c..=end).contains(a));
                format!("IS {c:04}-{end:04} ← MD-{} {n1:04}-{n2:04}", id.to_num())
            }
            WriteMD(id, n1, c, n2) => {
                let end = c + n2.saturating_sub(*n1);
                format!("MD-{} {n1:04}-{n2:04} ← IS {c:04}-{end:04}", id.to_num())
            }
            other => format!("{other:?}"),
        }
    }
}

fn fmt_number(v: f64) -> String {
    if v == 0.0 {
        "0".into()
    } else if (1e-4..1e9).contains(&v.abs()) {
        let s = format!("{v:.10}");
        s.trim_end_matches('0').trim_end_matches('.').into()
    } else {
        format!("{v:.6e}")
    }
}

fn fmt_instr(word: u64) -> String {
    crate::vm::instruction::Instruction::from_bytes(word)
        .map(|i| i.to_string())
        .unwrap_or_else(|_| format!("?? {:010x}", word.get_bits(0..39)))
}

/// Whitespace-separated terms that must all appear in the row, ignoring case.
pub struct Filter(Vec<String>);

impl Filter {
    pub fn parse(s: &str) -> Option<Filter> {
        let terms: Vec<String> = s.split_whitespace().map(str::to_lowercase).collect();
        if terms.is_empty() { None } else { Some(Filter(terms)) }
    }

    pub fn matches(&self, entry: &TraceEntry) -> bool {
        self.0.iter().all(|t| entry.haystack.contains(t.as_str()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(instr: &str, effect: &str) -> TraceEntry {
        TraceEntry::new(
            1,
            412,
            false,
            OpMetadata::default(),
            false,
            instr.into(),
            effect.into(),
            EntryKind::Normal,
        )
    }

    #[test]
    fn filter_terms() {
        let tn = entry("TN    0301      0305", "0305 ← 1 [0000000000]");
        let unnormed_tn = entry(",TN   0301      0305", "");
        let cmp = entry("Cmp   0305 1031 0420", "→ 0420");

        let f = Filter::parse("tn").unwrap();
        assert!(f.matches(&tn) && f.matches(&unnormed_tn) && !f.matches(&cmp));

        let f = Filter::parse("TN  0305").unwrap();
        assert!(f.matches(&tn) && f.matches(&unnormed_tn) && !f.matches(&cmp));

        let f = Filter::parse("0305 ←").unwrap();
        assert!(f.matches(&tn) && !f.matches(&unnormed_tn) && !f.matches(&cmp));

        assert!(Filter::parse("  ").is_none());
    }
}

use std::collections::{HashMap, VecDeque};

use log::{info, warn};

use crate::interface::trace::{TRACE_CAPACITY, TraceCheckpoint};
use crate::interface::{Interface, OpMetadata, StepMode, advance};
use crate::vm::{VM, VMState};

const KEYFRAME_INTERVAL: u64 = 500;
const KEYFRAMES: usize = TRACE_CAPACITY / KEYFRAME_INTERVAL as usize + 1;

struct Keyframe {
    vm: VMState,
    trace: TraceCheckpoint,
    printed: usize,
    recent_writes: HashMap<u16, (u32, u64)>,
    previous_op: OpMetadata,
}

pub struct History {
    frames: VecDeque<Keyframe>,
}

impl History {
    pub fn new() -> History {
        History { frames: VecDeque::with_capacity(KEYFRAMES) }
    }

    pub fn earliest(&self) -> Option<u64> {
        self.frames.front().map(|k| k.trace.steps)
    }
}

impl Interface {
    pub(crate) fn checkpoint(&mut self, vm: &VM) {
        let steps = self.trace.steps;
        let frames = &mut self.history.frames;
        if steps % KEYFRAME_INTERVAL != 0 || frames.back().is_some_and(|k| k.trace.steps >= steps) {
            return;
        }
        if frames.len() == KEYFRAMES {
            frames.pop_front();
        }
        frames.push_back(Keyframe {
            vm: vm.snapshot(),
            trace: self.trace.checkpoint(),
            printed: self.printer_output.len(),
            recent_writes: self.recent_writes.clone(),
            previous_op: self.previous_op,
        });
    }

    // Execution is deterministic, so replaying from a snapshot retraces the run.
    pub fn seek(&mut self, vm: &mut VM, target: u64) {
        let rewinding = target < self.trace.steps;
        if rewinding {
            let Some(frame) = self.history.frames.iter().rev().find(|k| k.trace.steps <= target)
            else {
                warn!(
                    "Can't rewind to step {target}: history starts at step {}",
                    self.history.earliest().unwrap_or(self.trace.steps)
                );
                return;
            };

            vm.restore(&frame.vm);
            self.trace.rewind(&frame.trace);
            self.printer_output.truncate(frame.printed);
            self.recent_writes = frame.recent_writes.clone();
            self.previous_op = frame.previous_op;

            let steps = self.trace.steps;
            self.history.frames.retain(|k| k.trace.steps <= steps);
        }

        // Steps taken after a halt were recorded too, so a rewind replays them.
        while self.trace.steps < target && (rewinding || !vm.stopped) {
            if advance(vm, self).is_err() {
                vm.stopped = true;
            }
        }

        self.tabs.offsets[0] = 0;
        self.step_mode = if vm.stopped { StepMode::Stop } else { StepMode::Step };
    }

    pub fn reverse_continue(&mut self, vm: &mut VM) {
        if self.breakpoint.is_none()
            && self.mem_breakpoint.is_none()
            && self.op_breakpoint.is_none()
        {
            warn!("Reverse continue needs a breakpoint");
            return;
        }

        let now = self.trace.steps;
        let entries = &self.trace.entries;

        let hit = entries.iter().enumerate().rev().find_map(|(i, e)| {
            let instr = self.breakpoint == Some(e.addr);
            let mem =
                self.mem_breakpoint.is_some() && e.write == self.mem_breakpoint && e.step < now;
            let op = self.op_breakpoint.is_some_and(|bp| {
                let entering = i == 0 || entries[i - 1].op != e.op;
                entering
                    && e.step < now
                    && (e.op.pass, e.op.procedure, e.op.operator)
                        == (bp.pass, bp.procedure, bp.operator)
            });

            // Instruction breakpoints stop before `e` runs, the others after.
            if mem || op {
                Some(e.step)
            } else if instr {
                Some(e.step - 1)
            } else {
                None
            }
        });

        match hit {
            Some(target) => {
                info!("Rewound to step {target}");
                self.seek(vm, target);
            }
            None => warn!("No earlier breakpoint hit in the last {} steps", entries.len()),
        }
    }
}

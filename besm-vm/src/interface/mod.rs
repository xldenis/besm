pub mod history;
pub mod input;
pub mod terminal;
pub mod trace;
pub mod tui;

pub use crate::interface::input::*;
pub use crate::interface::tui::*;

use crate::interface::{history::History, trace::Trace};
use crate::vm::{StepResult, VM, VMError};
use log::{error, warn};
use ratatui::layout::Size;

use std::collections::HashMap;

const WRITE_DECAY_STEPS: u32 = 5;
const PAGE: usize = 20;

// Operator breakpoint: (pass, procedure, operator)
#[derive(Debug, Clone, Copy)]
pub struct OpBreakpoint {
    pub pass: u16,
    pub procedure: u16,
    pub operator: u16,
}

// Operator metadata extracted from instruction
#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct OpMetadata {
    pub pass: u16,
    pub procedure: u16,
    pub operator: u16,
}

/// Extract operator metadata (pass, subprogram, operator) from an instruction word
pub fn extract_op_metadata(instr_word: u64) -> OpMetadata {
    use bit_field::BitField;
    let op_metadata = instr_word.get_bits(48..64);
    OpMetadata {
        pass: op_metadata.get_bits(14..16) as u16,
        procedure: op_metadata.get_bits(10..14) as u16,
        operator: op_metadata.get_bits(0..10) as u16,
    }
}

// Key events are turned into actions by `map_key` and applied by `update`.
pub struct Interface {
    pub size: Size,
    pub trace: Trace,
    history: History,
    pub trace_filter: String,
    pub step_mode: StepMode,
    pub input_mode: InputMode,
    pub tabs: TabInfo,
    pub breakpoint: Option<u16>,
    pub recent_writes: HashMap<u16, (u32, u64)>, // address -> (age, old_value)
    pub printer_output: Vec<String>,
    exiting: bool,
    mem_breakpoint: Option<u16>,
    op_breakpoint: Option<OpBreakpoint>,
    previous_op: OpMetadata,
}

impl Interface {
    pub fn with_breakpoints(
        breakpoint: Option<u16>,
        mem_breakpoint: Option<u16>,
        op_breakpoint: Option<OpBreakpoint>,
    ) -> Interface {
        Interface {
            size: Size::default(),
            trace: Trace::new(),
            history: History::new(),
            trace_filter: String::new(),
            step_mode: StepMode::Step,
            input_mode: InputMode::Normal,
            tabs: TabInfo::default(),
            breakpoint,
            recent_writes: HashMap::new(),
            printer_output: Vec::new(),
            exiting: false,
            mem_breakpoint,
            op_breakpoint,
            previous_op: OpMetadata::default(),
        }
    }

    pub fn toggle_step(&mut self) {
        use StepMode::*;
        self.step_mode = match self.step_mode {
            Run => Step,
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

    pub fn exiting(&self) -> bool {
        self.exiting
    }

    pub fn active_filter(&self) -> &str {
        match &self.input_mode {
            InputMode::Prompt { kind: PromptKind::Filter, buf } => buf,
            _ => &self.trace_filter,
        }
    }

    // The trace scrolls back from its newest entry.
    fn scroll(&mut self, older: bool, by: usize) {
        let trace = self.tabs.selection == 0;
        let offset = &mut self.tabs.offsets[self.tabs.selection];
        *offset =
            if older == trace { offset.saturating_add(by) } else { offset.saturating_sub(by) };
        if trace {
            *offset = (*offset).min(self.trace.entries.len().saturating_sub(1));
        }
    }

    /// Advance the VM by one step if running. Returns whether a redraw is needed.
    pub fn tick(&mut self, vm: &mut VM) -> bool {
        if self.step_mode == StepMode::Run {
            step_vm(vm, self);
            true
        } else {
            false
        }
    }

    pub fn update(&mut self, vm: &mut VM, action: Action) {
        use self::Action::*;

        match action {
            Quit => self.exiting = true,
            ToggleRun if self.step_mode == StepMode::Step => {
                self.toggle_step();
                step_vm(vm, self);
            }
            ToggleRun => self.toggle_step(),
            Step if self.step_mode == StepMode::Step => step_vm(vm, self),
            Step => {}
            RunBurst => {
                let old_mode = std::mem::replace(&mut self.step_mode, StepMode::Run);
                for _ in 0..1000 {
                    step_vm(vm, self);
                    if vm.stopped || self.step_mode == StepMode::Step {
                        return;
                    }
                }
                self.step_mode = old_mode;
            }
            PrevTab => {
                if self.tabs.prev_tab() {
                    self.pause();
                }
            }
            NextTab => {
                if self.tabs.next_tab() {
                    self.pause();
                }
            }
            StepBack => {
                let target = self.trace.steps.saturating_sub(1);
                self.seek(vm, target);
            }
            ReverseContinue => self.reverse_continue(vm),
            ScrollDown => self.scroll(false, 1),
            ScrollUp => self.scroll(true, 1),
            PageDown => self.scroll(false, PAGE),
            PageUp => self.scroll(true, PAGE),
            Goto(addr) => self.tabs.offsets[self.tabs.selection] = addr as usize,
            SetBreakpoint(b) => self.breakpoint = b,
            SetMemBreakpoint(b) => self.mem_breakpoint = b,
            SetOpBreakpoint(b) => self.op_breakpoint = b,
            OpenPrompt(PromptKind::Filter) => {
                let buf = self.trace_filter.clone();
                self.input_mode = InputMode::Prompt { kind: PromptKind::Filter, buf };
            }
            OpenPrompt(kind) => self.input_mode = InputMode::Prompt { kind, buf: String::new() },
            PromptInput(c) => {
                if let InputMode::Prompt { buf, .. } = &mut self.input_mode {
                    buf.push(c);
                }
            }
            PromptBackspace => {
                if let InputMode::Prompt { buf, .. } = &mut self.input_mode {
                    buf.pop();
                }
            }
            PromptCancel => self.input_mode = InputMode::Normal,
            PromptSubmit => {
                if let InputMode::Prompt { kind, buf } =
                    std::mem::replace(&mut self.input_mode, InputMode::Normal)
                {
                    if kind == PromptKind::Filter {
                        self.trace_filter = buf;
                        self.tabs.offsets[0] = 0;
                        return;
                    }
                    if kind == PromptKind::Seek {
                        match buf.trim().parse() {
                            Ok(target) => self.seek(vm, target),
                            Err(_) => warn!("Invalid {} input: {:?}", kind.label(), buf),
                        }
                        return;
                    }
                    match parse_prompt(kind, &buf) {
                        Some(action) => self.update(vm, action),
                        None => warn!("Invalid {} input: {:?}", kind.label(), buf),
                    }
                }
            }
        }
    }
}

fn step_vm(vm: &mut VM, app: &mut Interface) {
    // Check operator breakpoint: fire only on transition into the matching operator
    if let Some(op_bp) = app.op_breakpoint {
        if let Ok(instr_word) = vm.memory.get(vm.next_instr()) {
            let metadata = extract_op_metadata(instr_word);

            if metadata != app.previous_op
                && metadata.pass == op_bp.pass
                && metadata.procedure == op_bp.procedure
                && metadata.operator == op_bp.operator
            {
                log::info!(
                    "Operator breakpoint triggered - Pass: {}, Subprogram: {}, Operator: {}",
                    metadata.pass,
                    metadata.procedure,
                    metadata.operator
                );
                app.previous_op = metadata;
                app.pause();
            }
        }
    }

    let step_result = match advance(vm, app) {
        Ok(res) => res,
        Err(e) => {
            error!("{:?}", e);
            vm.stopped = true; // vm should handle this internally
            app.pause();
            return;
        }
    };

    if let Some(b) = app.mem_breakpoint {
        if let Some(out) = step_result.write
            && out.address == b
        {
            if let Ok(instr_word) = vm.memory.get(vm.next_instr()) {
                let metadata = extract_op_metadata(instr_word);
                log::info!(
                    "Memory write breakpoint triggered at address {} - Pass: {}, Subprogram: {}, Operator: {}",
                    b,
                    metadata.pass,
                    metadata.procedure,
                    metadata.operator
                );
            }
            app.pause();
        }
    }

    if let Some(b) = app.breakpoint {
        if b == vm.next_instr() {
            if let Ok(instr_word) = vm.memory.get(vm.next_instr()) {
                let metadata = extract_op_metadata(instr_word);
                log::info!(
                    "Instruction breakpoint triggered at address {} - Pass: {}, Subprogram: {}, Operator: {}",
                    b,
                    metadata.pass,
                    metadata.procedure,
                    metadata.operator
                );
            }
            app.pause();
        }
    }

    if vm.stopped {
        app.halt();
    }
}

// Runs and records one step without checking breakpoints.
pub(crate) fn advance(vm: &mut VM, app: &mut Interface) -> Result<StepResult, VMError> {
    app.checkpoint(vm);

    // Track operator transitions for breakpoint edge detection
    if let Ok(instr_word) = vm.memory.get(vm.next_instr()) {
        app.previous_op = extract_op_metadata(instr_word);
    }

    let addr = vm.next_instr();
    let pending = vm.memory.get(addr).ok().map(|word| app.trace.begin(addr, vm.active_ic, word));
    let result = vm.step();
    if let Some(pending) = pending {
        app.trace.finish(pending, &result, vm.next_instr(), vm.active_ic, vm.stopped);
    }

    if let Ok(step_result) = &result {
        // Update write tracking
        if let Some(write) = step_result.write {
            app.recent_writes.insert(write.address, (0, write.old_value));
        }

        // Capture printer output
        if let Some(output) = &step_result.printer_output {
            app.printer_output.push(output.clone());
        }
    }

    // Age existing writes and remove old ones
    app.recent_writes.retain(|_, (age, _)| {
        *age += 1;
        *age <= WRITE_DECAY_STEPS
    });

    result
}

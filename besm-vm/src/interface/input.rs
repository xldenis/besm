use crate::interface::{Interface, OpBreakpoint};

// Drivers convert their native key events into this.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Key {
    Char(char),
    Enter,
    Esc,
    Backspace,
    Up,
    Down,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PromptKind {
    Goto,
    Breakpoint,
    MemBreakpoint,
    OpBreakpoint,
}

impl PromptKind {
    pub fn label(&self) -> &'static str {
        match self {
            PromptKind::Goto => "GOTO",
            PromptKind::Breakpoint => "BREAK",
            PromptKind::MemBreakpoint => "MEM BREAK",
            PromptKind::OpBreakpoint => "OP BREAK (p-s-o)",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InputMode {
    Normal,
    Prompt { kind: PromptKind, buf: String },
}

#[derive(Debug, Clone, Copy)]
pub enum Action {
    Quit,
    ToggleRun,
    Step,
    RunBurst,
    PrevTab,
    NextTab,
    ScrollUp,
    ScrollDown,
    Goto(u16),
    SetBreakpoint(Option<u16>),
    SetMemBreakpoint(Option<u16>),
    SetOpBreakpoint(Option<OpBreakpoint>),
    OpenPrompt(PromptKind),
    PromptInput(char),
    PromptBackspace,
    PromptSubmit,
    PromptCancel,
}

impl Interface {
    pub fn map_key(&self, key: Key) -> Option<Action> {
        use self::{Action::*, Key::*};

        match self.input_mode {
            InputMode::Prompt { .. } => match key {
                Enter => Some(PromptSubmit),
                Esc => Some(PromptCancel),
                Backspace => Some(PromptBackspace),
                Char(c) => Some(PromptInput(c)),
                _ => None,
            },
            InputMode::Normal => match key {
                Char('q') => Some(Quit),
                Char(' ') => Some(ToggleRun),
                Char('s') => Some(Step),
                Char('r') => Some(RunBurst),
                Char('<') | Char(',') => Some(PrevTab),
                Char('>') | Char('.') => Some(NextTab),
                Up => Some(ScrollUp),
                Down => Some(ScrollDown),
                Char('g') => Some(OpenPrompt(PromptKind::Goto)),
                Char('b') => Some(OpenPrompt(PromptKind::Breakpoint)),
                Char('m') => Some(OpenPrompt(PromptKind::MemBreakpoint)),
                Char('o') => Some(OpenPrompt(PromptKind::OpBreakpoint)),
                _ => None,
            },
        }
    }
}

/// Parse a submitted prompt into the action it requests. An address of 0 clears a breakpoint.
pub fn parse_prompt(kind: PromptKind, buf: &str) -> Option<Action> {
    let buf = buf.trim();
    let addr_or_none = |s: &str| s.parse::<u16>().ok().map(|a| if a == 0 { None } else { Some(a) });

    match kind {
        PromptKind::Goto => buf.parse::<u16>().ok().map(Action::Goto),
        PromptKind::Breakpoint => addr_or_none(buf).map(Action::SetBreakpoint),
        PromptKind::MemBreakpoint => addr_or_none(buf).map(Action::SetMemBreakpoint),
        PromptKind::OpBreakpoint => parse_op_breakpoint(buf).map(Action::SetOpBreakpoint),
    }
}

// Format: "pass-procedure-operator" or "pass procedure operator". "0" clears the breakpoint.
fn parse_op_breakpoint(buf: &str) -> Option<Option<OpBreakpoint>> {
    let parts: Vec<&str> =
        if buf.contains('-') { buf.split('-').collect() } else { buf.split_whitespace().collect() };

    match parts[..] {
        [pass, procedure, operator] => {
            let pass = pass.trim().parse().ok()?;
            let procedure = procedure.trim().parse().ok()?;
            let operator = operator.trim().parse().ok()?;
            if pass == 0 && procedure == 0 && operator == 0 {
                Some(None)
            } else {
                Some(Some(OpBreakpoint { pass, procedure, operator }))
            }
        }
        ["0"] => Some(None),
        _ => None,
    }
}

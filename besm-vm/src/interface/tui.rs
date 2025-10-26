use ratatui::{
    backend::*,
    buffer::Buffer,
    layout::{Alignment, Constraint::*, Direction::*, Layout, Rect},
    style::{Color, Modifier, Style, Styled as _, Stylize},
    text::{Line, Span, Text},
    widgets::{Block, Borders, Cell, Paragraph, Table, Tabs, Widget, Wrap},
    Terminal,
};

use crate::vm::{
    instruction::{first_addr, get_instruction_parts, second_addr, third_addr, Instruction},
    VM,
};
use bit_field::BitField;

use tui_logger::*;

#[derive(Eq, PartialEq)]
pub enum StepMode {
    Run,
    Stop,
    Step,
}

use crate::interface::Interface;

pub struct TabInfo {
    titles: Vec<&'static str>,
    pub selection: usize,
    pub offsets: Vec<usize>,
}

impl TabInfo {
    pub fn default() -> TabInfo {
        TabInfo {
            titles: vec!["MAIN", "IS", "MD-0", "MD-1", "MD-2", "MD-3", "MD-4"],
            offsets: vec![0, 0, 0, 0, 0, 0, 0],
            selection: 0,
        }
    }

    pub fn prev_tab(&mut self) -> bool {
        if self.selection > 0 {
            self.selection -= 1;
            true
        } else {
            false
        }
    }

    pub fn next_tab(&mut self) -> bool {
        if self.selection < self.titles.len() - 1 {
            self.selection += 1;
            true
        } else {
            false
        }
    }
}

pub fn draw<T: Backend>(t: &mut Terminal<T>, vm: &VM, app: &Interface) {
    t.draw(|f| {
        let chunks = Layout::default()
            .direction(Vertical)
            .constraints(vec![Min(23), Length(3)])
            .split(Rect::new(0, 0, app.size.width, app.size.height));

        match app.tabs.selection {
            0 => render_main_panel(f.buffer_mut(), app, vm, chunks[0]),
            1 => render_memory_panel(f.buffer_mut(), app, vm, chunks[0]),
            2 => render_memory_panel(f.buffer_mut(), app, vm, chunks[0]),
            3 => render_memory_panel(f.buffer_mut(), app, vm, chunks[0]),
            4 => render_memory_panel(f.buffer_mut(), app, vm, chunks[0]),
            5 => render_memory_panel(f.buffer_mut(), app, vm, chunks[0]),
            6 => render_memory_panel(f.buffer_mut(), app, vm, chunks[0]),
            _ => {}
        }
        render_status_line(f.buffer_mut(), app, &vm.active_ic, chunks[1]);
    })
    .unwrap();
}

fn render_main_panel(t: &mut Buffer, app: &Interface, vm: &VM, rect: Rect) {
    let chunks =
        Layout::default().direction(Vertical).constraints(vec![Length(3), Min(20)]).split(rect);

    render_current_instruction_box(
        t,
        vm.next_instr(),
        vm.memory.get(vm.next_instr()).unwrap(),
        chunks[0],
    );

    let inner_chunks = Layout::default()
        .constraints(vec![Min(20), Length(24)])
        .direction(Horizontal)
        .split(chunks[1]);

    TuiLoggerWidget::default()
        .block(Block::default().title("Log").borders(Borders::ALL))
        .render(inner_chunks[0], t);

    render_past_instructions(t, app.past_instrs.iter(), inner_chunks[1]);
}

// Highlights the diff between two instructions
fn build_instruction_cell(
    new_instr: &Instruction,
    old_instr: Option<&Instruction>,
    highlight_style: Style,
) -> Line<'static> {
    let normal_style = Style::default();

    let Some(old) = old_instr else {
        return Line::from(format!("{}", new_instr));
    };

    let (new_op, new_a, new_b, new_c) = get_instruction_parts(new_instr);
    let (old_op, old_a, old_b, old_c) = get_instruction_parts(old);

    let mut spans = Vec::new();

    let opcode_changed = new_op != old_op;
    let opcode_text = format!("{:<5}", new_op);
    spans.push(Span::styled(
        opcode_text,
        if opcode_changed { highlight_style } else { normal_style },
    ));

    let add_operand = |spans: &mut Vec<Span<'_>>, new_val, old_val| {
        let text = match new_val {
            Some(v) => format!(" {:4}", v),
            None => " ".repeat(5),
        };
        let changed = new_val != old_val;
        spans.push(Span::styled(text, if changed { highlight_style } else { normal_style }));
    };

    add_operand(&mut spans, new_a, old_a);
    add_operand(&mut spans, new_b, old_b);
    add_operand(&mut spans, new_c, old_c);

    Line::from(spans)
}

fn build_binary_cell(new_value: u64, old_value: u64, highlight_style: Style) -> Line<'static> {
    let normal_style = Style::default();
    let mut spans = Vec::new();

    let new_opcode = new_value.get_bits(33..39);
    let old_opcode = old_value.get_bits(33..39);

    let new_a = new_value.get_bits(22..33);
    let old_a = old_value.get_bits(22..33);

    let new_b = new_value.get_bits(11..22);
    let old_b = old_value.get_bits(11..22);

    let new_c = new_value.get_bits(0..11);
    let old_c = old_value.get_bits(0..11);

    spans.push(Span::styled(
        format!("{:06b}", new_opcode),
        if new_opcode != old_opcode { highlight_style } else { normal_style },
    ));

    spans.push(Span::styled(
        format!("{:011b}", new_a),
        if new_a != old_a { highlight_style } else { normal_style },
    ));

    spans.push(Span::styled(
        format!("{:011b}", new_b),
        if new_b != old_b { highlight_style } else { normal_style },
    ));

    spans.push(Span::styled(
        format!("{:011b}", new_c),
        if new_c != old_c { highlight_style } else { normal_style },
    ));

    Line::from(spans)
}

fn render_memory_panel(t: &mut Buffer, app: &Interface, vm: &VM, rect: Rect) {
    use ratatui::widgets::Row;

    let tabs = &app.tabs;
    let (mem_vec, addr_offset): (Box<dyn Iterator<Item = u64>>, usize) = match tabs.selection {
        1 => (Box::new(vm.memory.into_iter()) as Box<_>, 1),
        i => (Box::new(vm.mag_system.mag_drives[i - 2].into_iter()) as Box<_>, 0),
    };
    let tab_offset = tabs.offsets[tabs.selection].saturating_sub(addr_offset);

    let rows = mem_vec.enumerate().skip(tab_offset).map(|(addr, instr)| {
        use crate::float::Float;

        let current_addr = addr as u16 + addr_offset as u16;
        let float = Float::from_bytes(instr);
        let active_bits = instr.get_bits(0..39);

        let write_info = if tabs.selection == 1 {
            app.recent_writes.get(&current_addr).map(|(age, old_value)| (*age, *old_value))
        } else {
            None
        };

        let row_style = if vm.next_instr() - 1 == addr as u16 && tabs.selection == 1 {
            Style::default().add_modifier(Modifier::BOLD).fg(Color::Yellow)
        } else {
            Style::default()
        };

        let (instr_cell, float, hex, bits) = if let Some((age, old_value)) = write_info {
            let color = match age {
                0 => Color::Rgb(255, 0, 0),     // Bright red
                1 => Color::Rgb(255, 100, 100), // Medium red
                2 => Color::Rgb(255, 150, 150), // Light red/pink
                3 => Color::Rgb(200, 180, 180), // Very light pink
                _ => Color::Reset,              // Light gray (age 4)
            };

            let style = row_style.fg(color).bold();

            let new_instr = Instruction::from_bytes(instr);
            let is_instr = new_instr.is_ok();
            let instr_cell = match (new_instr, Instruction::from_bytes(old_value).ok()) {
                (Ok(new_instr), old_instr) => {
                    build_instruction_cell(&new_instr, old_instr.as_ref(), style)
                }
                (Err(_), _) => Line::from("ERROR"),
            };

            let other_highlights = if is_instr { Style::default() } else { style };
            let bits_cell = build_binary_cell(active_bits, old_value.get_bits(0..39), style);
            (
                Cell::from(instr_cell),
                Cell::from(format!("{}", float).set_style(other_highlights)),
                Cell::from(format!("{:010x}", active_bits).set_style(other_highlights)),
                Cell::from(bits_cell),
            )
        } else {
            let instr_cell = Line::from(
                Instruction::from_bytes(instr)
                    .map(|s| format!("{} ", s))
                    .unwrap_or_else(|_| "ERROR".to_string()),
            );
            (
                Cell::from(instr_cell),
                Cell::from(format!("{}", float)),
                Cell::from(format!("{:010x}", active_bits)),
                Cell::from(format!("{:039b}", active_bits)),
            )
        };

        Row::new(vec![Cell::from(format!("{:04}", current_addr)), instr_cell, float, hex, bits])
            .style(row_style)
    });

    Table::new(rows, [4, 20, 10, 10, 39])
        .header(Row::new(["Addr", "Instruction", "Number", "Hex", "Raw"]).fg(Color::Yellow))
        .column_spacing(2)
        .block(Block::default().title(tabs.titles[tabs.selection]).borders(Borders::ALL))
        .render(rect, t)
}

use crate::vm::ActiveIC;
fn render_status_line(t: &mut Buffer, app: &Interface, active_ic: &ActiveIC, rect: Rect) {
    Block::default().borders(Borders::ALL).render(rect, t);

    let chunks = Layout::default()
        .margin(1)
        .direction(Horizontal)
        .constraints(vec![Length(6), Length(8), Min(0), Length(11)])
        .split(rect);

    let (style, text) = match &app.step_mode {
        StepMode::Run => (Style::default().fg(Color::Green), "RUN"),
        StepMode::Step => (Style::default().fg(Color::Yellow), "STEP"),
        StepMode::Stop => (Style::default().fg(Color::Red), "STOP"),
    };

    Paragraph::new(Text::styled(format!("{:4}", text), style))
        .alignment(Alignment::Center)
        .render(chunks[0], t);

    let ic = match active_ic {
        ActiveIC::Global => "GLOBAL",
        ActiveIC::Local => "LOCAL",
    };

    Paragraph::new(Text::raw(format!("{:6}", ic)))
        .alignment(Alignment::Center)
        .render(chunks[1], t);

    Tabs::default()
        .titles(app.tabs.titles.clone())
        .select(app.tabs.selection)
        .highlight_style(Style::default().fg(Color::Yellow))
        .render(chunks[2], t);

    if let Some(b) = app.breakpoint {
        Paragraph::new(Text::styled(
            format!("BREAK {:04} ", b),
            Style::default().fg(Color::Yellow),
        ))
        .render(chunks[3], t);
    }
}

fn render_past_instructions<'a, I>(t: &mut Buffer, app: I, rect: Rect)
where
    I: Iterator<Item = &'a Instruction>,
{
    Block::default().borders(Borders::ALL).title("Past Instructions").render(rect, t);

    let chunks = Layout::default()
        .margin(1)
        .direction(Vertical)
        .constraints(vec![Length(1); rect.height as usize])
        .split(rect);

    for (instr, chunk) in app.zip(chunks.iter()) {
        Paragraph::new(Text::raw(format!("{} ", instr)))
            .alignment(Alignment::Center)
            .render(*chunk, t);
    }
}

fn render_current_instruction_box(t: &mut Buffer, ix: u16, instr_bytes: u64, rect: Rect) {
    Block::default().borders(Borders::ALL).title("Current Instruction").render(rect, t);

    let chunks = Layout::default()
        .margin(1)
        .direction(Horizontal)
        .constraints(vec![
            Length(5),
            Length(2),
            Length(42),
            Length(2),
            Length(10),
            Length(2),
            Percentage(100),
        ])
        .split(rect);

    Paragraph::new(Text::raw(format!("{:04}", ix)))
        .alignment(Alignment::Right)
        .render(chunks[0], t);

    Paragraph::new(Text::raw(format!(
        "{:06b} {:011b} {:011b} {:011b}",
        instr_bytes.get_bits(33..38),
        first_addr(instr_bytes),
        second_addr(instr_bytes),
        third_addr(instr_bytes)
    )))
    .render(chunks[2], t);

    Paragraph::new(Text::raw(format!("{:010x}", instr_bytes))).render(chunks[4], t);

    Paragraph::new(Text::raw(
        Instruction::from_bytes(instr_bytes)
            .map(|s| format!("{} ", s))
            .unwrap_or_else(|_| "ERROR".to_string()),
    ))
    .alignment(Alignment::Right)
    .wrap(Wrap { trim: false })
    .render(chunks[6], t);
}

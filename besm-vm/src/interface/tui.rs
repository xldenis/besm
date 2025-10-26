use ratatui::{
    backend::*,
    buffer::Buffer,
    layout::{Alignment, Constraint::*, Direction::*, Layout, Rect},
    style::{Color, Style},
    text::Text,
    widgets::{Block, Borders, Paragraph, Table, Tabs, Widget, Wrap},
    Terminal,
};

use crate::vm::{
    instruction::{first_addr, second_addr, third_addr, Instruction},
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
            1 => render_memory_panel(f.buffer_mut(), &app.tabs, vm, chunks[0]),
            2 => render_memory_panel(f.buffer_mut(), &app.tabs, vm, chunks[0]),
            3 => render_memory_panel(f.buffer_mut(), &app.tabs, vm, chunks[0]),
            4 => render_memory_panel(f.buffer_mut(), &app.tabs, vm, chunks[0]),
            5 => render_memory_panel(f.buffer_mut(), &app.tabs, vm, chunks[0]),
            6 => render_memory_panel(f.buffer_mut(), &app.tabs, vm, chunks[0]),
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

fn render_memory_panel(t: &mut Buffer, tabs: &TabInfo, vm: &VM, rect: Rect) {
    use ratatui::widgets::Row;

    let (mem_vec, addr_offset): (Box<dyn Iterator<Item = u64>>, usize) = match tabs.selection {
        1 => (Box::new(vm.memory.into_iter()) as Box<_>, 1),
        i => (Box::new(vm.mag_system.mag_drives[i - 2].into_iter()) as Box<_>, 0),
    };
    let tab_offset = tabs.offsets[tabs.selection].saturating_sub(addr_offset);

    let rows = mem_vec.enumerate().skip(tab_offset).map(|(addr, instr)| {
        let instr_string = Instruction::from_bytes(instr)
            .map(|s| format!("{} ", s))
            .unwrap_or_else(|_| "ERROR".to_string());

        use crate::float::Float;
        let float = Float::from_bytes(instr);
        let active_bits = instr.get_bits(0..39);
        let style = if vm.next_instr() - 1 == addr as u16 && tabs.selection == 1 {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default()
        };

        Row::new(vec![
            format!("{:04}", addr + addr_offset),
            instr_string,
            format!("{}", float),
            format!("{:010x}", active_bits),
            format!("{:039b}", active_bits),
        ])
        .style(style)
    });

    Table::new(rows, [4, 20, 10, 10, 39])
        // .header_style(Style::default().fg(Color::Yellow))
        .header(Row::new(["Addr", "Instruction", "Number", "Hex", "Raw"]))
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

use tui::{
    backend::*,
    layout::{Alignment, Layout, Rect},
    style::{Color, Style},
    widgets::{Block, Borders, Paragraph, Table, Tabs, Text, Widget},
    Frame, Terminal,
};

use bit_field::BitField;
use vm::{
    instruction::{first_addr, second_addr, third_addr, Instruction},
    VM,
};

use tui_logger::*;

#[derive(Eq, PartialEq)]
pub enum StepMode {
    Run,
    Stop,
    Step,
}

use interface::Interface;

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

use tui::layout::{Constraint::*, Direction::*};

pub fn draw<T: Backend>(t: &mut Terminal<T>, vm: &VM, app: &Interface) {
    t.draw(|mut f| {
        let chunks = Layout::default()
            .direction(Vertical)
            .constraints(vec![Min(23), Length(3)])
            .split(app.size);

        match app.tabs.selection {
            0 => render_main_panel(&mut f, app, vm, chunks[0]),
            1 => render_memory_panel(&mut f, &app.tabs, vm, chunks[0]),
            2 => render_memory_panel(&mut f, &app.tabs, vm, chunks[0]),
            3 => render_memory_panel(&mut f, &app.tabs, vm, chunks[0]),
            4 => render_memory_panel(&mut f, &app.tabs, vm, chunks[0]),
            5 => render_memory_panel(&mut f, &app.tabs, vm, chunks[0]),
            6 => render_memory_panel(&mut f, &app.tabs, vm, chunks[0]),
            _ => {}
        }
        render_status_line(&mut f, app, &vm.active_ic, chunks[1]);
    })
    .unwrap();
}

fn render_main_panel<T: Backend>(t: &mut Frame<T>, app: &Interface, vm: &VM, rect: Rect) {
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
        .render(t, inner_chunks[0]);

    render_past_instructions(t, app.past_instrs.iter(), inner_chunks[1]);
}

fn render_memory_panel<T: Backend>(t: &mut Frame<T>, tabs: &TabInfo, vm: &VM, rect: Rect) {
    use tui::widgets::Row;

    let (mem_vec, addr_offset): (Box<dyn Iterator<Item = u64>>, usize) = match tabs.selection {
        1 => (Box::new(vm.memory.into_iter()) as Box<_>, 1),
        i => (Box::new(vm.mag_system.mag_drives[i - 2].into_iter()) as Box<_>, 0),
    };
    let tab_offset = tabs.offsets[tabs.selection].saturating_sub(addr_offset);

    let rows = mem_vec.enumerate().skip(tab_offset).map(|(addr, instr)| {
        let instr_string = Instruction::from_bytes(instr)
            .map(|s| format!("{} ", s))
            .unwrap_or_else(|_| "ERROR".to_string());

        use float::Float;
        let float = Float::from_bytes(instr);
        let active_bits = instr.get_bits(0..39);
        let style = if vm.next_instr() - 1 == addr as u16 && tabs.selection == 1 {
            Style::default().fg(Color::Yellow)
        } else {
            Style::default()
        };

        Row::StyledData(
            vec![
                format!("{:04}", addr + addr_offset),
                instr_string,
                format!("{}", float),
                format!("{:010x}", active_bits),
                format!("{:039b}", active_bits),
            ]
            .into_iter(),
            style,
        )
    });

    Table::new(["Addr", "Instruction", "Number", "Hex", "Raw"].iter(), rows)
        .widths(&[4, 20, 10, 10, 39])
        .column_spacing(2)
        .block(Block::default().title(tabs.titles[tabs.selection]).borders(Borders::ALL))
        .header_style(Style::default().fg(Color::Yellow))
        .render(t, rect)
}

use vm::ActiveIC;
fn render_status_line<T: Backend>(
    t: &mut Frame<T>,
    app: &Interface,
    active_ic: &ActiveIC,
    rect: Rect,
) {
    Block::default().borders(Borders::ALL).render(t, rect);

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

    Paragraph::new([Text::styled(format!("{:4}", text), style)].iter())
        .alignment(Alignment::Center)
        .render(t, chunks[0]);

    let ic = match active_ic {
        ActiveIC::Global => "GLOBAL",
        ActiveIC::Local => "LOCAL",
    };

    Paragraph::new([Text::raw(format!("{:6}", ic))].iter())
        .alignment(Alignment::Center)
        .render(t, chunks[1]);

    Tabs::default()
        .titles(&app.tabs.titles)
        .select(app.tabs.selection)
        .highlight_style(Style::default().fg(Color::Yellow))
        .render(t, chunks[2]);

    if let Some(b) = app.breakpoint {
        Paragraph::new(
            [Text::styled(format!("BREAK {:04} ", b), Style::default().fg(Color::Yellow))].iter(),
        )
        .render(t, chunks[3]);
    }
}

fn render_past_instructions<'a, T, I>(t: &mut Frame<T>, app: I, rect: Rect)
where
    T: Backend,
    I: Iterator<Item = &'a Instruction>,
{
    Block::default().borders(Borders::ALL).title("Past Instructions").render(t, rect);

    let chunks = Layout::default()
        .margin(1)
        .direction(Vertical)
        .constraints(vec![Length(1); rect.height as usize])
        .split(rect);

    for (instr, chunk) in app.zip(chunks.iter()) {
        Paragraph::new([Text::raw(format!("{} ", instr))].iter())
            .alignment(Alignment::Center)
            .render(t, *chunk);
    }
}

fn render_current_instruction_box<T: Backend>(
    t: &mut Frame<T>,
    ix: u16,
    instr_bytes: u64,
    rect: Rect,
) {
    Block::default().borders(Borders::ALL).title("Current Instruction").render(t, rect);

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

    Paragraph::new([Text::raw(format!("{:04}", ix))].iter())
        .alignment(Alignment::Right)
        .render(t, chunks[0]);

    Paragraph::new(
        [Text::raw(format!(
            "{:06b} {:011b} {:011b} {:011b}",
            instr_bytes.get_bits(33..38),
            first_addr(instr_bytes),
            second_addr(instr_bytes),
            third_addr(instr_bytes)
        ))]
        .iter(),
    )
    .render(t, chunks[2]);

    Paragraph::new([Text::raw(format!("{:010x}", instr_bytes))].iter()).render(t, chunks[4]);

    Paragraph::new(
        [Text::raw(
            Instruction::from_bytes(instr_bytes)
                .map(|s| format!("{} ", s))
                .unwrap_or_else(|_| "ERROR".to_string()),
        )]
        .iter(),
    )
    .alignment(Alignment::Right)
    .wrap(true)
    .render(t, chunks[6]);
}

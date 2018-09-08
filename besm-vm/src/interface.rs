use tui::backend::*;
use tui::layout::{Group, Rect, Size};
use tui::style::{Alignment, Style, Color};
use tui::widgets::{Block, Borders, Paragraph, Widget, Tabs, Table};
use tui::Terminal;

use vm::{first_addr, second_addr, third_addr, Instruction, VM};

use bit_field::BitField;

use arraydeque::*;
use tui_logger::*;

use arraydeque::behavior::Wrapping;

#[derive(Eq, PartialEq)]
pub enum StepMode { Run, Stop, Step }

pub struct Interface {
    pub size: Rect,
    pub past_instrs: ArrayDeque<[Instruction; 50], Wrapping>,
    pub step_mode: StepMode,
    pub tabs: TabInfo,
}

pub struct TabInfo {
    titles: Vec<&'static str>,
    pub selection: usize,
    pub offsets: Vec<usize>,
}

impl TabInfo {
    pub fn default() -> TabInfo {
        TabInfo {
            titles: vec!["MAIN","IS", "MD-0", "MD-1", "MD-2", "MD-3", "MD-4"],
            offsets: vec![0    , 0  , 0     , 0     , 0     , 0     , 0],
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

impl Interface {
    pub fn toggle_step(&mut self) {
        use StepMode::*;
        self.step_mode = match self.step_mode {
            Run  => Step,
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
}

use tui::layout::Size::*;
use tui::layout::Direction::*;

pub fn draw(t: &mut Terminal<MouseBackend>, vm: &VM, app: &Interface) {
    let chunks: &[Size] = &[Min(23), Fixed(3)];

    Group::default()
        .direction(Vertical)
        .sizes(&chunks)
        .render(t, &app.size, |t, chunks| {
            match app.tabs.selection {
                0 => { render_main_panel(t, app, vm, chunks[0]) }
                1 => { render_memory_panel(t, &app.tabs, vm, chunks[0]) }
                2 => { render_memory_panel(t, &app.tabs, vm, chunks[0]) }
                3 => { render_memory_panel(t, &app.tabs, vm, chunks[0]) }
                4 => { render_memory_panel(t, &app.tabs, vm, chunks[0]) }
                5 => { render_memory_panel(t, &app.tabs, vm, chunks[0]) }
                6 => { render_memory_panel(t, &app.tabs, vm, chunks[0]) }
                _ => {}
            }
            render_status_line(t, app, chunks[1]);

        });

    t.draw().unwrap();
}

fn render_main_panel<T: Backend>(t: &mut Terminal<T>, app: &Interface, vm: &VM, rect: Rect) {
    Group::default()
        .direction(Vertical)
        .sizes(&[Fixed(3), Min(20)])
        .render(t, &rect, |t, chunks| {
            render_current_instruction_box(t, vm, chunks[0]);

            Group::default()
                .sizes(&[Min(20), Fixed(24)])
                .direction(Horizontal)
                .render(t, &chunks[1], |t, chunks| {
                    TuiLoggerWidget::default()
                        .block(Block::default().title("Log").borders(Borders::ALL))
                        .render(t, &chunks[0]);

                    render_past_instructions(t, app, chunks[1]);

                });
        })
}


fn render_memory_panel<T: Backend>(t: &mut Terminal<T>, tabs: &TabInfo, vm: &VM, rect: Rect) {
    use tui::widgets::Row;

    let (mem_vec, addr_offset) = match tabs.selection {
        1 => {
          (vm.is.to_vec(), 1)
        }
        i => {
           (vm.mag_system.mag_drives[i - 2].drive.to_vec(), 0)
        }
    };
    let tab_offset = tabs.offsets[tabs.selection].saturating_sub(addr_offset);
    let rows = mem_vec.iter().enumerate().skip(tab_offset).map(|(addr, instr)| {
        let instr_string = Instruction::from_bytes(*instr)
            .map(|s| format!("{} ", s))
            .unwrap_or_else(|_| "ERROR".to_string());

        use float::Float;
        let float = Float::from_bytes(*instr);
        Row::Data(
            vec![format!("{:04}", addr + addr_offset), instr_string, format!("{}", float), format!("{:010x}", instr), format!("{:039b}", instr)].into_iter()
        )
    });

    Table::new(
            ["Addr", "Instruction", "Number", "Hex", "Raw"].into_iter(),
            rows
        )
        .widths(&[4, 20, 10, 10, 39])
        .column_spacing(2)
        .block(Block::default().title(tabs.titles[tabs.selection]).borders(Borders::ALL))
        .header_style(Style::default().fg(Color::Yellow))
        .render(t, &rect)
}

fn render_status_line<T: Backend>(t: &mut Terminal<T>, app: &Interface, rect: Rect) {
    Block::default()
        .borders(Borders::ALL)
        .render(t, &rect);

    Group::default()
        .margin(1)
        .direction(Horizontal)
        .sizes(&[Fixed(6), Percent(100)])
        .render(t, &rect, |t, chunks| {
            let (style, text) = match &app.step_mode {
                StepMode::Run => (Style::default().fg(Color::Green),   "RUN"),
                StepMode::Step => (Style::default().fg(Color::Yellow), "STEP"),
                StepMode::Stop => (Style::default().fg(Color::Red),  "STOP"),
            };

            Paragraph::default()
                .alignment(Alignment::Center)
                .style(style)
                .text(&format!("{:4}", text))
                .render(t, &chunks[0]);

            Tabs::default()
                .titles(&app.tabs.titles)
                .select(app.tabs.selection)
                .highlight_style(Style::default().fg(Color::Yellow))
                .render(t, &chunks[1])
        });
}

fn render_past_instructions<T: Backend>(t: &mut Terminal<T>, app: &Interface, rect: Rect) {
    Block::default()
        .borders(Borders::ALL)
        .title("Past Instructions")
        .render(t, &rect);

    Group::default()
        .margin(1)
        .direction(Vertical)
        .sizes(&vec![Fixed(1); rect.height as usize])
        .render(t, &rect, |t, chunks| {
            for (instr, chunk) in app.past_instrs.iter().zip(chunks.iter()) {
                Paragraph::default()
                    .text(&format!("{} ", instr))
                    .alignment(Alignment::Center)
                    .render(t, &chunk);
            }
        });
}

fn render_current_instruction_box<T: Backend>(t: &mut Terminal<T>, vm: &VM, rect: Rect) {
    Block::default()
        .borders(Borders::ALL)
        .title("Current Instruction")
        .render(t, &rect);

    Group::default()
        .margin(1)
        .direction(Horizontal)
        .sizes(&[Fixed(5), Fixed(2), Fixed(42), Fixed(2), Fixed(10), Fixed(2), Percent(100)])
        .render(t, &rect, |t, chunks| {
            let ins = vm.get_address(vm.next_instr()).unwrap();

            Paragraph::default()
                .text(&format!("{:04}", vm.next_instr()))
                .alignment(Alignment::Right)
                .render(t, &chunks[0]);

            Paragraph::default()
                .text(&format!(
                    "{:06b} {:011b} {:011b} {:011b}",
                    ins.get_bits(33..38),
                    first_addr(ins),
                    second_addr(ins),
                    third_addr(ins)
                ))
                .render(t, &chunks[2]);

            Paragraph::default()
                .text(&format!("{:010x}", ins))
                .render(t, &chunks[4]);

            Paragraph::default()
                .text(
                    &Instruction::from_bytes(ins)
                        .map(|s| format!("{} ", s))
                        .unwrap_or_else(|_| "ERROR".to_string()),
                )
                .alignment(Alignment::Right)
                .wrap(true)
                .render(t, &chunks[6]);
        });
}

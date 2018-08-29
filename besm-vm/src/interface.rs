use tui::layout::{Direction, Group, Rect, Size};
use tui::widgets::{Widget, Paragraph, Block, Borders};
use tui::style::{Alignment};
use tui::backend::*;
use tui::Terminal;

use vm::{first_addr, second_addr, third_addr, VM, Instruction};

use bit_field::BitField;

use arraydeque::*;
use tui_logger::*;

use arraydeque::behavior::Wrapping;

pub struct Interface {
  pub size: Rect,
  pub past_instrs: ArrayDeque<[Instruction; 20], Wrapping>,
}

pub fn draw(t: &mut Terminal<MouseBackend>, vm: &VM, app: &Interface) {
  let chunks : &[Size] = if app.size.height > 30 {
    &[Size::Fixed(3), Size::Fixed(20), Size::Percent(100)]
  } else {
    &[Size::Fixed(3), Size::Percent(100)]
  };

  Group::default()
    .direction(Direction::Vertical)
    .sizes(&chunks)
    .render(t, &app.size, |t, chunks| {
      render_current_instruction_box(t, vm, chunks[0]);
      render_past_instructions(t, app, chunks[1]);

      // Only render the log if we have enough space
      if chunks.len() > 2 {
        TuiLoggerWidget::default()
          .block(
            Block::default()
              .title("Log")
              .borders(Borders::ALL)
          )
          .render(t, &chunks[2]);
      }

    });

  t.draw().unwrap();
}

fn render_past_instructions<T : Backend>(t: &mut Terminal<T>, app: &Interface, rect: Rect) {
  Block::default()
        .borders(Borders::ALL)
        .title("Past Instructions")
        .render(t, &rect);

  Group::default()
    .margin(1)
    .direction(Direction::Vertical)
    .sizes(&[Size::Fixed(1); 20])
    .render(t, &rect, |t, chunks| {
      for (instr, chunk) in app.past_instrs.iter().zip(chunks.iter()) {
        Paragraph::default()
          .text(&format!("{}", instr))
          .alignment(Alignment::Right)
          .render(t, &chunk);
      }
    });
}

fn render_current_instruction_box(t: &mut Terminal<MouseBackend>, vm: &VM, rect: Rect) {
  Block::default()
    .borders(Borders::ALL)
    .title("Current Instruction")
    .render(t, &rect);

  Group::default()
    .margin(1)
    .direction(Direction::Horizontal)
    .sizes(&[Size::Fixed(5), Size::Fixed(2), Size::Fixed(42), Size::Fixed(2), Size::Fixed(10), Size::Fixed(2), Size::Percent(100)])
    .render(t, &rect, |t, chunks| {
      let ins = vm.get_address(u64::from(vm.next_instr())).unwrap();

      Paragraph::default()
        .text(&format!("{:04}", vm.next_instr()))
        .alignment(Alignment::Right)
        .render(t, &chunks[0]);

      Paragraph::default()
        .text(&format!("{:06b} {:011b} {:011b} {:011b}", ins.get_bits(33..38), first_addr(ins), second_addr(ins), third_addr(ins)))
        .render(t, &chunks[2]);

      Paragraph::default()
        .text(&format!("{:010x}", ins))
        .render(t, &chunks[4]);

      Paragraph::default()
        .text(&Instruction::from_bytes(ins).map(|s| format!("{}", s)).unwrap_or_else(|_| "ERROR".to_string()))
        .alignment(Alignment::Right)
        .wrap(true)
        .render(t, &chunks[6]);
    });
}

use std::io::Write;

use ratatui::{style::Style, widgets::Widget};

use crate::rope::Rope;

pub struct SyntaxHighlightedText<'a> {
    text: &'a Rope,
    line_numbers: bool,
}

impl<'a> SyntaxHighlightedText<'a> {
    pub fn new(text: &'a Rope, line_numbers: bool) -> SyntaxHighlightedText<'a> {
        SyntaxHighlightedText { text, line_numbers }
    }
}

impl<'a> Widget for SyntaxHighlightedText<'a> {
    fn render(self, area: ratatui::prelude::Rect, buf: &mut ratatui::prelude::Buffer) {
        let highlights = self.text.highlights();
        let find_style = |b: usize| {
            highlights
                .iter()
                .find(|h| h.range.contains(&b))
                .map(|h| h.style)
                .unwrap_or_else(Style::new)
        };
        let mut line_number_text = Vec::new();
        let mut line_text_iter = self.text.lines();
        let rope_lines = self.text.line_len();
        for (line_idx, y) in (0..rope_lines).zip(area.y..area.bottom()) {
            let mut byte_index = self.text.byte_of_line(line_idx);
            let line = match line_text_iter.next() {
                Some(l) => l,
                None => break,
            };
            let mut x = area.x;
            let mut width = area.width;
            if self.line_numbers {
                line_number_text.clear();
                write!(
                    &mut line_number_text,
                    "{line_number: >3} ",
                    line_number = line_idx + 1
                )
                .unwrap();
                for ch in std::str::from_utf8(line_number_text.as_slice())
                    .unwrap()
                    .chars()
                {
                    if width == 0 {
                        break;
                    }
                    let cell = buf.get_mut(x, y);
                    cell.set_char(ch);
                    x += 1;
                    width -= 1;
                }
            }
            for ch in line.chars() {
                if width == 0 {
                    break;
                }
                let cell = buf.get_mut(x, y);
                cell.set_char(ch);
                cell.set_style(find_style(byte_index));
                byte_index += ch.len_utf8();
                x += 1;
                width -= 1;
            }
        }
    }
}

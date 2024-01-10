use std::io::Write;

use ratatui::{style::Color, widgets::Widget};

use crate::rope::Rope;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum CursorPosition {
    /// Do not render a cursor.
    None,
    /// The byte position of the cursor.
    Byte(usize),
}

pub struct SyntaxHighlightedText<'a> {
    pub text: &'a Rope,
    pub line_numbers: bool,
    pub cursor: CursorPosition,
}

impl<'a> Widget for SyntaxHighlightedText<'a> {
    fn render(self, area: ratatui::prelude::Rect, buf: &mut ratatui::prelude::Buffer) {
        let highlights = self.text.highlights();
        let find_style = |b: usize| {
            for h in highlights.iter() {
                if h.range.contains(&b) {
                    return Some(h.style);
                }
                if h.range.start > b {
                    return None;
                }
            }
            None
        };
        let mut line_number_text = Vec::new();
        let rope_lines = self.text.len_lines();
        let mut line_text_iter = self.text.lines();
        for (line_idx, y) in (0..rope_lines).zip(area.y..area.bottom()) {
            let mut char_index = self.text.line_to_char(line_idx);
            let mut byte_index = self.text.line_to_byte(line_idx);
            let mut x = area.x;
            let mut width = area.width;
            // 1. Render line numbers.
            if self.line_numbers {
                line_number_text.clear();
                if line_idx < rope_lines {
                    write!(
                        &mut line_number_text,
                        "{line_number: >3} ",
                        line_number = line_idx + 1
                    )
                    .unwrap();
                } else {
                    write!(&mut line_number_text, "    ").unwrap();
                }
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
            // 2. Render the line of text.
            if let Some(line) = line_text_iter.next() {
                for ch in line.chars() {
                    if width == 0 {
                        break;
                    }
                    let cell = buf.get_mut(x, y);
                    let next_char_index = char_index + 1;
                    let next_byte_index = byte_index + ch.len_utf8();
                    if ch != '\n' {
                        cell.set_char(ch);
                    }
                    if let Some(s) = find_style(byte_index) {
                        cell.set_style(s);
                    }
                    if let CursorPosition::Byte(p) = self.cursor {
                        if (char_index..next_char_index).contains(&p) {
                            cell.set_bg(Color::White);
                        }
                    }
                    char_index = next_char_index;
                    byte_index = next_byte_index;
                    x += 1;
                    width -= 1;
                }
            }
        }
    }
}

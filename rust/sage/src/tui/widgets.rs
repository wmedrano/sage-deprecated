use std::io::Write;

use ratatui::{
    style::{Color, Style},
    widgets::Widget,
};

use crate::rope::Rope;

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum CursorPosition {
    /// Do not render a cursor.
    None,
    /// Render a cursor at the end of the text.
    EndOfText,
    /// Render a cursor at the end of the line with the given index.
    EndOfLine(usize),
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
            highlights
                .iter()
                .find(|h| h.range.contains(&b))
                .map(|h| h.style)
                .unwrap_or_else(Style::new)
        };
        let mut line_number_text = Vec::new();
        let has_trailing_newline = self
            .text
            .chunks()
            .next_back()
            .map(|s| s.ends_with('\n'))
            .unwrap_or(false);
        let rope_lines = self.text.line_len() + if has_trailing_newline { 1 } else { 0 };
        let mut line_text_iter = self.text.lines();
        for (line_idx, y) in (0..rope_lines).zip(area.y..area.bottom()) {
            let mut byte_index = self.text.byte_of_line(line_idx);
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
                    cell.set_char(ch);
                    cell.set_style(find_style(byte_index));
                    byte_index += ch.len_utf8();
                    x += 1;
                    width -= 1;
                }
            }
            // 3. Render the cursor.
            match self.cursor {
                CursorPosition::None => (),
                CursorPosition::EndOfText => {
                    if line_idx + 1 == rope_lines && width > 0 {
                        let cell = buf.get_mut(x, y);
                        cell.set_bg(Color::White);
                    }
                }
                CursorPosition::EndOfLine(idx) => {
                    if line_idx == idx && width > 0 {
                        let cell = buf.get_mut(x, y);
                        cell.set_bg(Color::White);
                    }
                }
            }
        }
    }
}

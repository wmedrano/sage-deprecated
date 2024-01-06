use ratatui::{style::Style, widgets::Widget};

use crate::rope::Rope;

pub struct SyntaxHighlightedText<'a> {
    text: &'a Rope,
}

impl<'a> SyntaxHighlightedText<'a> {
    pub fn new(text: &'a Rope) -> SyntaxHighlightedText<'a> {
        SyntaxHighlightedText { text }
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
        let rope_lines = self.text.line_len();
        for (idx, y) in (0..rope_lines).zip(area.y..area.bottom()) {
            let mut byte_index = self.text.byte_of_line(idx);
            let line = self.text.line(idx);
            let mut x = area.x;
            let mut width = area.width;
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

use ratatui::{
    prelude::Rect,
    style::{Style, Stylize},
    widgets::{Block, BorderType, Borders, Clear, Widget},
};

use crate::buffer_content::BufferContent;

use super::theme::ONEDARK_THEME;

pub struct BufferWidget<'a> {
<<<<<<< HEAD
    pub buffer: &'a BufferContent,
    pub line_numbers: bool,
    pub highlight_line: bool,
    pub cursor: bool,
    pub border: bool,
=======
    buffer: &'a BufferContent,
    render_line_numbers: bool,
}

impl<'a> BufferWidget<'a> {
    pub fn new(buffer: &'a BufferContent, render_line_numbers: bool) -> BufferWidget<'a> {
        BufferWidget {
            buffer,
            render_line_numbers,
        }
    }
>>>>>>> main
}

impl<'a> Widget for BufferWidget<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        Clear.render(area, buf);
        let area = if self.border {
            let b = Block::new()
                .borders(Borders::ALL)
                .border_type(BorderType::Plain)
                .bg(ONEDARK_THEME.black1);
            let a = b.inner(area);
            b.render(area, buf);
            a
        } else {
            area
        };
        let line_idx = 0..area.height as usize;
        let selected_line = self.buffer.iter_lines().count();
        let mut cursor_pos = None;
        for (idx, line) in line_idx.zip(self.buffer.iter_lines()) {
            let mut area = Rect {
                x: area.x,
                y: area.y + idx as u16,
                width: area.width,
                height: 1,
            };
            // Line background.
            if self.highlight_line && selected_line == idx + 1 {
                Block::new().bg(ONEDARK_THEME.black3).render(area, buf);
            } else {
                Block::new().bg(ONEDARK_THEME.black1).render(area, buf);
            }
            // Line number.
            if self.line_numbers && area.width >= 3 {
                buf.set_stringn(
                    area.x,
                    area.y,
                    format!("{n: >3}", n = idx + 1),
                    area.width as usize,
                    Style::new().fg(ONEDARK_THEME.white1),
                );
                area.x += 4;
                area.width -= 4;
            }
            // Text: TODO, also set the width.
            (area.x, area.y) = buf.set_stringn(
                area.x,
                area.y,
                line,
                area.width as usize,
                Style::new().fg(ONEDARK_THEME.white3),
            );
            if self.cursor && selected_line == idx + 1 {
                cursor_pos = Some((area.x, area.y));
            }
        }
        if self.buffer.iter_lines().count() < area.height as usize {
            Block::new().bg(ONEDARK_THEME.black1).render(
                Rect {
                    x: area.x,
                    y: area.y + self.buffer.iter_lines().count() as u16,
                    width: area.width,
                    height: area.height - self.buffer.iter_lines().count() as u16,
                },
                buf,
            );
        }
        if let Some((x, y)) = cursor_pos {
            if x < area.right() {
                buf.set_stringn(x, y, " ", 1, Style::new().bg(ONEDARK_THEME.white3));
            }
        }
    }
}

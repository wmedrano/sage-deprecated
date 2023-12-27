use ratatui::{
    prelude::Rect,
    style::{Style, Stylize},
    widgets::{Block, Widget},
};

use crate::buffer_content::BufferContent;

use super::theme::ONEDARK_THEME;

const LINE_NUMBER_WIDTH: u16 = 3;

pub struct BufferWidget<'a> {
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
}

impl<'a> Widget for BufferWidget<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        let lines = self.buffer.iter_lines();
        let line_count = self.buffer.iter_lines().count();
        let should_render_line_number =
            self.render_line_numbers && area.width > 2 * LINE_NUMBER_WIDTH && line_count > 1;
        let positions = area.y..(area.y + area.height);
        let line_number_x = area.x;
        let text_x = if should_render_line_number {
            area.x + LINE_NUMBER_WIDTH + 1
        } else {
            area.x
        };
        let text_width = if should_render_line_number {
            area.width - LINE_NUMBER_WIDTH - 1
        } else {
            area.width
        };
        let selected_line = if line_count == 0 { 0 } else { line_count - 1 };
        for (idx, (line, y)) in lines.zip(positions).enumerate() {
            let bg = if selected_line == idx {
                ONEDARK_THEME.black3
            } else {
                ONEDARK_THEME.black1
            };
            let line_area = Rect {
                x: area.x,
                y,
                width: area.width,
                height: 1,
            };
            // Background.
            Block::default().bg(bg).render(line_area, buf);
            // Line number.
            if should_render_line_number {
                let line_text = format!("{line_number: >3}", line_number = idx + 1);
                buf.set_stringn(
                    line_number_x,
                    y,
                    line_text,
                    LINE_NUMBER_WIDTH as usize,
                    Style::new().fg(ONEDARK_THEME.white1),
                );
            }
            // Line text.
            let (cursor_x, _) = buf.set_stringn(
                text_x,
                y,
                line,
                text_width as usize,
                Style::new().fg(ONEDARK_THEME.white3),
            );
            // Cursor.
            if should_render_line_number && idx == selected_line && cursor_x < area.right() {
                buf.set_stringn(cursor_x, y, " ", 1, Style::new().bg(ONEDARK_THEME.white1));
            }
        }
    }
}

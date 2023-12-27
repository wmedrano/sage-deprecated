use ratatui::{
    prelude::Rect,
    style::{Color, Style, Stylize},
    widgets::{Block, Widget},
};

use crate::buffer::Buffer;

const LINE_NUMBER_WIDTH: u16 = 3;

#[derive(Copy, Clone)]
pub struct ColorScheme {
    pub black1: Color,
    pub black2: Color,
    pub black3: Color,
    pub red: Color,
    pub green: Color,
    pub yellow: Color,
    pub blue: Color,
    pub purple: Color,
    pub cyan: Color,
    pub white1: Color,
    pub white2: Color,
    pub white3: Color,
}

pub const COLOR_SCHEME: ColorScheme = ColorScheme {
    black1: Color::Rgb(33, 37, 43),
    black2: Color::Rgb(40, 44, 52),
    black3: Color::Rgb(50, 56, 66),
    red: Color::Rgb(224, 108, 117),
    green: Color::Rgb(152, 195, 121),
    yellow: Color::Rgb(229, 192, 123),
    blue: Color::Rgb(97, 175, 239),
    purple: Color::Rgb(198, 120, 221),
    cyan: Color::Rgb(86, 182, 194),
    white1: Color::Rgb(171, 178, 191),
    white2: Color::Rgb(212, 216, 223),
    white3: Color::Rgb(246, 247, 249),
};

pub struct BufferWidget<'a> {
    buffer: &'a Buffer,
    render_line_numbers: bool,
}

impl<'a> BufferWidget<'a> {
    pub fn new(buffer: &'a Buffer, render_line_numbers: bool) -> BufferWidget<'a> {
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
                COLOR_SCHEME.black3
            } else {
                COLOR_SCHEME.black1
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
                    Style::new().fg(COLOR_SCHEME.white1),
                );
            }
            // Line text.
            let (cursor_x, _) = buf.set_stringn(
                text_x,
                y,
                line,
                text_width as usize,
                Style::new().fg(COLOR_SCHEME.white3),
            );
            // Cursor.
            if should_render_line_number && idx == selected_line && cursor_x < area.right() {
                buf.set_stringn(cursor_x, y, " ", 1, Style::new().bg(COLOR_SCHEME.white1));
            }
        }
    }
}

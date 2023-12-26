use ratatui::{style::Style, widgets::Widget};

use crate::buffer::Buffer;

pub struct BufferWidget<'a> {
    buffer: &'a Buffer,
}

impl<'a> BufferWidget<'a> {
    pub fn new(buffer: &'a Buffer) -> BufferWidget<'a> {
        BufferWidget { buffer }
    }
}

impl<'a> Widget for BufferWidget<'a> {
    fn render(self, area: ratatui::prelude::Rect, buf: &mut ratatui::prelude::Buffer) {
        let lines = self.buffer.iter_lines();
        let positions = area.y..(area.y + area.height);
        for (line, y) in lines.zip(positions) {
            buf.set_stringn(area.x, y, line, area.width as usize, Style::new());
        }
    }
}

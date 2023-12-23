use ratatui::{prelude::Rect, style::Style, text::Line, widgets::Widget};

use crate::buffer::Buffer;

/// Render a buffer.
pub struct BufferWidget<'a> {
    buffer: &'a Buffer,
}

impl<'a> BufferWidget<'a> {
    /// Create a new buffer widget.
    pub fn new(buffer: &'a Buffer) -> BufferWidget<'a> {
        BufferWidget { buffer }
    }
}

impl<'a> Widget for BufferWidget<'a> {
    /// Render the buffer to the area.
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        let line_ys = area.y..area.bottom();
        let mut cursor = (0u16, 0u16);
        for (line_y, text) in line_ys.zip(self.buffer.iter_lines()) {
            cursor = buf.set_line(area.x, line_y, &Line::raw(text), area.width);
        }
        let cursor_area = Rect {
            x: cursor.0,
            y: cursor.1,
            width: 1,
            height: 1,
        };
        if area.intersects(cursor_area) {
            buf.set_string(cursor_area.x, cursor_area.y, "_", Style::default());
        }
    }
}

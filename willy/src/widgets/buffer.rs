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
        let line_count = self.buffer.iter_lines().count().clamp(1, usize::MAX);
        let cursor_area = Rect {
            x: cursor.0,
            y: line_count as u16 - 1,
            width: 1,
            height: 1,
        };
        if area.intersects(cursor_area) {
            buf.set_string(cursor_area.x, cursor_area.y, "_", Style::default());
        }
    }
}

#[cfg(test)]
mod tests {
    use ratatui::assert_buffer_eq;

    use super::*;

    #[test]
    fn empty_buffer_is_only_cursor() {
        let area = ratatui::prelude::Rect {
            x: 0,
            y: 0,
            width: 16,
            height: 8,
        };
        let mut buf = ratatui::prelude::Buffer::empty(area);
        BufferWidget::new(&Buffer::new()).render(area, &mut buf);
        assert_buffer_eq!(
            buf,
            ratatui::prelude::Buffer::with_lines(vec![
                "_               ",
                "                ",
                "                ",
                "                ",
                "                ",
                "                ",
                "                ",
                "                ",
            ])
        )
    }

    #[test]
    fn buffer_with_text_renders_text_and_cursor() {
        let area = ratatui::prelude::Rect {
            x: 0,
            y: 0,
            width: 16,
            height: 8,
        };
        let mut buf = ratatui::prelude::Buffer::empty(area);
        BufferWidget::new(&Buffer::with_text("my text... ")).render(area, &mut buf);
        assert_buffer_eq!(
            buf,
            ratatui::prelude::Buffer::with_lines(vec![
                "my text... _    ",
                "                ",
                "                ",
                "                ",
                "                ",
                "                ",
                "                ",
                "                ",
            ])
        )
    }

    #[test]
    fn text_line_that_is_out_of_range_is_not_rendered() {
        let area = ratatui::prelude::Rect {
            x: 0,
            y: 0,
            width: 16,
            height: 3,
        };
        let mut buf = ratatui::prelude::Buffer::empty(area);
        BufferWidget::new(&Buffer::with_text(
            "very long text sequence that will not fit",
        ))
        .render(area, &mut buf);
        assert_buffer_eq!(
            buf,
            ratatui::prelude::Buffer::with_lines(vec![
                "very long text s",
                "                ",
                "                ",
            ])
        )
    }

    #[test]
    fn text_that_is_too_tall_is_not_rendered() {
        let area = ratatui::prelude::Rect {
            x: 0,
            y: 0,
            width: 16,
            height: 3,
        };
        let mut buf = ratatui::prelude::Buffer::empty(area);
        BufferWidget::new(&Buffer::with_text("1\n2\n3\n4\n5\n6\n")).render(area, &mut buf);
        assert_buffer_eq!(
            buf,
            ratatui::prelude::Buffer::with_lines(vec![
                "1               ",
                "2               ",
                "3               ",
            ])
        )
    }
}

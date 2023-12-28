use ratatui::{
    prelude::Rect,
    style::Stylize,
    widgets::{Clear, Paragraph, Widget},
};

use crate::buffer_content::BufferContent;

use super::theme::ONEDARK_THEME;

pub struct BufferWidget<'a> {
    pub buffer: &'a BufferContent,
}

impl<'a> BufferWidget<'a> {
    fn render_impl(self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        Paragraph::new(self.buffer.to_string())
            .fg(ONEDARK_THEME.white3)
            .bg(ONEDARK_THEME.black1)
            .render(area, buf);
    }
}

impl<'a> Widget for BufferWidget<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        Clear.render(area, buf);
        self.render_impl(area, buf);
    }
}

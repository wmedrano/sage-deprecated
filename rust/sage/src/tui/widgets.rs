use ratatui::{style::Style, widgets::Widget};

/// A widget for rendering a single line.
pub struct LineWidget<S> {
    /// The contents of the line. Usually an iterator that produces `&str`.
    contents: S,
}

impl<S> LineWidget<S> {
    pub fn new(contents: S) -> LineWidget<S> {
        LineWidget { contents }
    }
}

impl<'a, S: Iterator<Item = &'a str>> Widget for LineWidget<S> {
    fn render(self, area: ratatui::prelude::Rect, buf: &mut ratatui::prelude::Buffer) {
        let mut x = area.x;
        let mut width = area.width as usize;
        for s in self.contents {
            if width == 0 {
                break;
            }
            let (new_x, _) = buf.set_stringn(x, area.y, s, width, Style::new());
            let delta = new_x - x;
            x = new_x;
            width = width.saturating_sub(delta as usize);
        }
    }
}

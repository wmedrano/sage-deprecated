use ratatui::{
    prelude::{Alignment::Right, Rect},
    style::{Color, Stylize},
    widgets::{Block, BorderType, Borders, Clear, Widget},
};

use crate::rope::Rope;

use super::widgets::{CursorPosition, SyntaxHighlightedText};

/// Defines a window on the screen.
pub struct Window<'a> {
    /// The text contents.
    pub rope: &'a Rope,
    /// The area of the window.
    pub area: Rect,
    /// The title of the window.
    pub title: Option<String>,
    /// True if a border should be rendered.
    pub border: bool,
    /// True if line numbers should be rendered.
    pub line_numbers: bool,
    /// Where the cursor should be rendered.
    pub cursor: CursorPosition,
}

impl<'a> Widget for Window<'a> {
    fn render(self, area: Rect, buf: &mut ratatui::prelude::Buffer) {
        let area = area.intersection(self.area);
        // 1. Clear the area. If this does not happen, the the stuff underneath can shine through.
        Clear.render(area, buf);
        // 2. Render the block which may or may not have a border.
        let block = {
            let mut block = Block::new().bg(Color::Reset);
            if self.border {
                block = block.borders(Borders::ALL).border_type(BorderType::Rounded);
            }
            if let Some(title) = self.title.as_ref() {
                block = block.title(title.as_str()).title_alignment(Right);
            }
            block
        };
        let inner_area = block.inner(area);
        block.render(area, buf);
        // 3. Render the text.
        SyntaxHighlightedText {
            text: self.rope,
            line_numbers: self.line_numbers,
            cursor: self.cursor,
        }
        .render(inner_area, buf);
    }
}

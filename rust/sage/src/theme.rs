use std::ops::Range;

use ratatui::style::{Color, Style};
use tree_sitter::{Node, Query, QueryCursor, TextProvider, Tree};

pub struct SyntaxTheme {
    query: Query,
    cursor: QueryCursor,
    capture_to_color: Vec<Style>,
}

impl SyntaxTheme {
    /// Create a new syntax theme for the given language and highlight query.
    pub fn new(language: tree_sitter::Language, highlight_query: &str) -> SyntaxTheme {
        let query = tree_sitter::Query::new(language, highlight_query).unwrap();
        let all_colors = [
            Style::new().fg(Color::Green),
            Style::new().fg(Color::Blue),
            Style::new().fg(Color::Magenta),
            Style::new().fg(Color::Yellow),
            Style::new().fg(Color::Cyan),
            Style::new().fg(Color::LightRed),
            Style::new().fg(Color::LightBlue),
            Style::new().fg(Color::LightCyan),
            Style::new().fg(Color::LightGreen),
            Style::new().fg(Color::LightYellow),
            Style::new().fg(Color::LightMagenta),
        ];
        let capture_to_color: Vec<Style> = query
            .capture_names()
            .iter()
            .zip(all_colors.into_iter().cycle())
            .map(|(_, c)| c)
            .collect();
        SyntaxTheme {
            query,
            cursor: QueryCursor::new(),
            capture_to_color,
        }
    }

    /// Create new highlights for the given tree.
    pub fn highlight<'a, 'b: 'a>(
        &'a mut self,
        tree: &'b Tree,
        text: impl 'a + TextProvider<'a>,
    ) -> impl 'a + Iterator<Item = HighlightRange> {
        let root_node: Node<'b> = tree.root_node();
        let matches = self.cursor.matches(&self.query, root_node, text);
        matches
            .flat_map(|m| m.captures.iter())
            .map(|c| HighlightRange {
                range: c.node.byte_range(),
                style: self.capture_to_color[c.index as usize],
            })
    }
}

pub struct HighlightRange {
    pub range: Range<usize>,
    pub style: Style,
}

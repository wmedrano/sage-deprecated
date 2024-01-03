use flashkick::foreign_object::ForeignObjectType;
use tree_sitter::{InputEdit, Language, Parser, Point, Tree};

/// Buffer holds editable an editable string.
#[derive(Default)]
pub struct BufferContent {
    lines: Vec<String>,
    parser: Option<Parser>,
    tree: Option<Tree>,
}

impl ForeignObjectType for BufferContent {
    const NAME: &'static str = "willy-buffer-content";
}

/// An empty buffer.
pub static EMPTY_BUFFER_CONTENT: BufferContent = BufferContent {
    lines: Vec::new(),
    parser: None,
    tree: None,
};

impl BufferContent {
    /// Create a new blank buffer.
    pub fn new<L: Into<Option<Language>>>(language: L) -> BufferContent {
        let mut parser = language.into().map(|l| {
            let mut parser = Parser::new();
            // TODO: Log the result.
            let _ = parser.set_language(l);
            parser
        });
        let tree = parser
            .as_mut()
            .map(|p| p.parse(&[], None).expect("unexpected parse cancellation"));
        BufferContent {
            lines: Vec::new(),
            parser,
            tree,
        }
    }

    /// Create a new buffer from a string.
    #[cfg(test)]
    pub fn with_str<L: Into<Option<Language>>>(language: L, s: &str) -> BufferContent {
        let mut bc = BufferContent::new(language.into());
        bc.push_chars(s.chars());
        bc
    }

    /// Iterate through all the lines.
    pub fn iter_lines(&self) -> impl Iterator<Item = &str> {
        self.lines
            .iter()
            .map(|s| s.as_str())
            .filter(|s| !s.is_empty())
    }

    /// Return the syntax tree.
    #[cfg(test)]
    pub fn tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }

    /// Push several characters onto the buffer.
    pub fn push_chars(&mut self, chs: impl IntoIterator<Item = char>) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }
        let start_byte: usize = self.lines.iter().map(|l| l.as_bytes().len()).sum();
        let start_point = Point {
            row: self.lines.len() - 1,
            column: self.lines.last().unwrap().chars().count(),
        };
        let byte_len: usize = chs
            .into_iter()
            .inspect(|ch| {
                self.lines.last_mut().unwrap().push(*ch);
                if *ch == '\n' {
                    self.lines.push(String::new())
                }
            })
            .map(|c| c.len_utf8())
            .sum();
        let end_byte = start_byte + byte_len;
        let end_point = Point {
            row: self.lines.len() - 1,
            column: self.lines.last().unwrap().chars().count(),
        };
        if let Some(tree) = self.tree.as_mut() {
            tree.edit(&InputEdit {
                start_byte,
                old_end_byte: start_byte,
                new_end_byte: end_byte,
                start_position: start_point,
                old_end_position: start_point,
                new_end_position: end_point,
            });
        }
        if let Some(parser) = self.parser.as_mut() {
            self.tree = parser.parse_with(
                &mut |_, point| match self.lines.get(point.row) {
                    None => "",
                    Some(line) => &line[point.column..],
                },
                self.tree.as_ref(),
            )
        }
    }

    /// Pop a character from the buffer or `None` if the buffer is empty.
    pub fn pop_char(&mut self) -> Option<char> {
        let ch = {
            while !self.lines.is_empty() && self.lines.last().unwrap().is_empty() {
                self.lines.pop();
            }
            if self.lines.is_empty() {
                return None;
            }
            let end_byte: usize = self.lines.iter().map(|l| l.as_str().len()).sum();
            let end_point = Point {
                row: self.lines.len() - 1,
                column: self.lines.last().unwrap().chars().count(),
            };
            let line = self.lines.iter_mut().last()?;
            let ch = line.pop()?;
            let start_byte = end_byte - ch.len_utf8();
            let start_point = Point {
                row: end_point.row,
                column: end_point.column - 1,
            };
            if let Some(tree) = self.tree.as_mut() {
                tree.edit(&InputEdit {
                    start_byte,
                    old_end_byte: end_byte,
                    new_end_byte: start_byte,
                    start_position: start_point,
                    old_end_position: end_point,
                    new_end_position: start_point,
                });
            }
            ch
        };
        if let Some(parser) = self.parser.as_mut() {
            self.tree = parser.parse_with(
                &mut |_, point| match self.lines.get(point.row) {
                    None => "",
                    Some(line) => &line[point.column..],
                },
                self.tree.as_ref(),
            )
        }
        while !self.lines.is_empty() && self.lines.last().unwrap().is_empty() {
            self.lines.pop();
        }
        Some(ch)
    }

    /// Clear the contents of the buffer.
    pub fn clear(&mut self) {
        if self.lines.is_empty() {
            return;
        }
        self.tree = self
            .parser
            .as_mut()
            .map(|parser| -> Option<Tree> {
                parser.reset();
                parser.parse(&[], None)
            })
            .flatten();
        self.lines.clear();
    }
}

impl std::fmt::Display for BufferContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.iter_lines() {
            f.write_str(line)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use tree_sitter_rust::language;

    use super::*;

    #[test]
    fn push_chars_adds_new_char() {
        let mut buffer = BufferContent::new(language());
        assert_eq!(buffer.to_string(), "");
        buffer.push_chars(std::iter::once('a'));
        assert_eq!(buffer.to_string(), "a");
    }

    #[test]
    fn push_chars_adds_new_chars() {
        let mut buffer = BufferContent::new(language());
        buffer.push_chars(['a', 'b', 'c', 'd', '\n', 'e', '\n']);
        assert_eq!(buffer.to_string(), "abcd\ne\n");
    }

    #[test]
    fn push_chars_to_buffer_with_string_appends() {
        let mut buffer = BufferContent::with_str(language(), "name: ");
        assert_eq!(buffer.to_string(), "name: ");

        buffer.push_chars(['w', 'i', 'l', 'l', 'y']);
        assert_eq!(buffer.to_string(), "name: willy");

        buffer.push_chars("\nfunction: editor".chars());
        assert_eq!(buffer.to_string(), "name: willy\nfunction: editor");
    }

    #[test]
    fn pop_char_on_empty_buffer_is_none() {
        let mut buffer = BufferContent::new(language());
        assert_eq!(buffer.pop_char(), None);
    }

    #[test]
    fn pop_char_removes_last_char() {
        let mut buffer = BufferContent::with_str(language(), "typoo");
        assert_eq!(buffer.to_string(), "typoo");
        assert_eq!(buffer.pop_char(), Some('o'));
        assert_eq!(buffer.to_string(), "typo");
    }

    #[test]
    fn pop_char_can_remove_line() {
        let mut buffer = BufferContent::with_str(language(), "1\n2\n3\n\n");
        assert_eq!(buffer.to_string(), "1\n2\n3\n\n");

        assert_eq!(buffer.pop_char(), Some('\n'));
        assert_eq!(buffer.to_string(), "1\n2\n3\n");

        assert_eq!(buffer.pop_char(), Some('\n'));
        assert_eq!(buffer.to_string(), "1\n2\n3");

        assert_eq!(buffer.pop_char(), Some('3'));
        assert_eq!(buffer.to_string(), "1\n2\n");

        assert_eq!(buffer.pop_char(), Some('\n'));
        assert_eq!(buffer.to_string(), "1\n2");
    }

    #[test]
    fn clear_creates_empty_buffer() {
        let mut buffer = BufferContent::with_str(language(), "let x = 12;");
        assert_eq!(buffer.to_string(), "let x = 12;");

        buffer.clear();
        assert_eq!(buffer.to_string(), "");
    }

    #[test]
    fn can_parse_tree() {
        let mut buffer = BufferContent::new(language());
        assert_eq!(
            buffer.tree().unwrap().root_node().to_sexp(),
            "(source_file)"
        );

        buffer.push_chars("fn my_fn()t".chars());
        assert_eq!(
            buffer.tree().unwrap().root_node().to_sexp(),
            "(source_file (ERROR (identifier) (parameters) (identifier)))"
        );

        assert_eq!(buffer.pop_char(), Some('t'));
        assert_eq!(
            buffer.tree().unwrap().root_node().to_sexp(),
            "(source_file (function_signature_item name: (identifier) parameters: (parameters) (MISSING \";\")))",
        );

        buffer.push_chars("-> i32 {\n    0\n}\n".chars());
        assert_eq!(
            buffer.tree().unwrap().root_node().to_sexp(),
            "(source_file (function_item name: (identifier) parameters: (parameters) return_type: (primitive_type) body: (block (integer_literal))))"
        );
    }
}

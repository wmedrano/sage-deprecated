use flashkick::foreign_object::ForeignObjectType;

/// Buffer holds editable an editable string.
#[derive(Default)]
pub struct BufferContent {
    lines: Vec<String>,
}

impl ForeignObjectType for BufferContent {
    const NAME: &'static str = "willy-buffer-content";
}

/// An empty buffer.
pub static EMPTY_BUFFER_CONTENT: BufferContent = BufferContent::new();

impl BufferContent {
    /// Create a new blank buffer.
    pub const fn new() -> BufferContent {
        BufferContent { lines: Vec::new() }
    }

    /// Create a new buffer from a string.
    pub fn with_str(s: &str) -> BufferContent {
        BufferContent {
            lines: s.split('\n').map(str::to_string).collect(),
        }
    }

    /// Iterate through all the lines.
    pub fn iter_lines(&self) -> impl ExactSizeIterator + Iterator<Item = &str> {
        self.lines.iter().map(|s| s.as_str())
    }

    /// Add a new character to the buffer.
    pub fn push_char(&mut self, ch: char) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }
        match ch {
            '\n' => self.lines.push(String::new()),
            ch => self.lines.last_mut().unwrap().push(ch),
        }
    }

    /// Push several characters onto the buffer.
    pub fn push_chars(&mut self, chs: impl IntoIterator<Item = char>) {
        for ch in chs.into_iter() {
            self.push_char(ch);
        }
    }

    /// Pop a character from the buffer or `None` if the buffer is empty.
    pub fn pop_char(&mut self) -> Option<char> {
        let line = self.lines.iter_mut().last()?;
        line.pop()
    }

    /// Clear the contents of the buffer.
    pub fn clear(&mut self) {
        self.lines.clear();
    }
}

impl std::fmt::Display for BufferContent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lines = self.iter_lines();
        if let Some(line) = lines.next() {
            write!(f, "{line}")?;
        }
        for line in lines {
            write!(f, "\n{line}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_char_adds_new_char() {
        let mut buffer = BufferContent::new();
        assert_eq!(buffer.to_string(), "");
        buffer.push_char('a');
        assert_eq!(buffer.to_string(), "a");
    }

    #[test]
    fn push_chars_adds_new_chars() {
        let mut buffer = BufferContent::new();
        buffer.push_chars(['a', 'b', 'c', 'd', '\n', 'e', '\n']);
        assert_eq!(buffer.to_string(), "abcd\ne\n");
    }

    #[test]
    fn push_chars_to_buffer_with_string_appends() {
        let mut buffer = BufferContent::with_str("name: ");
        assert_eq!(buffer.to_string(), "name: ");

        buffer.push_chars(['w', 'i', 'l', 'l', 'y']);
        assert_eq!(buffer.to_string(), "name: willy");

        buffer.push_chars("\nfunction: editor".chars());
        assert_eq!(buffer.to_string(), "name: willy\nfunction: editor");
    }

    #[test]
    fn pop_char_on_empty_buffer_is_none() {
        let mut buffer = BufferContent::new();
        assert_eq!(buffer.pop_char(), None);
    }

    #[test]
    fn pop_char_removes_last_char() {
        let mut buffer = BufferContent::with_str("typoo");
        assert_eq!(buffer.to_string(), "typoo");
        assert_eq!(buffer.pop_char(), Some('o'));
        assert_eq!(buffer.to_string(), "typo");
    }
}

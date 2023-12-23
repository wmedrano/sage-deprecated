/// Buffer holds editable text.
#[derive(Default)]
pub struct Buffer {
    lines: Vec<String>,
}

impl Buffer {
    /// Create a new empty `Buffer`.
    pub fn new() -> Buffer {
        Buffer::default()
    }

    /// Create a new buffer from text.
    pub fn with_text(text: &str) -> Buffer {
        Buffer {
            lines: text.split('\n').map(str::to_string).collect(),
        }
    }

    /// Create a new scratch buffer. The buffer contains a friendly message.
    pub fn new_scratch() -> Buffer {
        Buffer {
            lines: vec![
                ";; Welcome to Willy!".to_string(),
                ";; A Scheme configured text editor.".to_string(),
                "".to_string(),
            ],
        }
    }

    /// Convert the buffer into text
    pub fn to_text(&self) -> String {
        self.lines.join("\n")
    }

    /// Iterate through all lines.
    pub fn iter_lines<'a>(&'a self) -> impl 'a + ExactSizeIterator + Iterator<Item = &'a str> {
        self.lines.iter().map(|s| s.as_str())
    }

    /// Add a new character to the buffer.
    pub fn push_char(&mut self, ch: char) {
        if self.lines.is_empty() {
            self.lines.push(String::new());
        }
        if ch == '\n' {
            self.lines.push(String::new());
            return;
        }
        self.lines.last_mut().unwrap().push(ch);
    }

    /// Push several characters onto the buffer.
    pub fn push_chars(&mut self, chs: impl IntoIterator<Item = char>) {
        for ch in chs.into_iter() {
            self.push_char(ch);
        }
    }

    /// Pop a character from the buffer or `None` if the buffer is empty.
    pub fn pop_char(&mut self) -> Option<char> {
        if self.lines.is_empty() {
            return None;
        }
        match self.lines.last_mut().unwrap().pop() {
            Some(ch) => Some(ch),
            None => {
                self.lines.pop();
                Some('\n')
            }
        }
    }
}

impl std::fmt::Display for Buffer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in self.lines.iter() {
            writeln!(f, "{line}")?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn push_char_adds_new_char() {
        let mut buffer = Buffer::new();
        buffer.push_char('a');
        assert_eq!(buffer.to_text(), "a");
    }

    #[test]
    fn push_chars_adds_new_chars() {
        let mut buffer = Buffer::new();
        buffer.push_chars(['a', 'b', 'c', 'd', '\n', 'e', '\n']);
        assert_eq!(buffer.to_text(), "abcd\ne\n");
    }

    #[test]
    fn push_chars_to_buffer_with_text_appends() {
        let mut buffer = Buffer::with_text("name: ");
        buffer.push_chars(['w', 'i', 'l', 'l', 'y']);
        assert_eq!(buffer.to_text(), "name: willy");
    }

    #[test]
    fn pop_char_on_empty_buffer_is_none() {
        let mut buffer = Buffer::new();
        assert_eq!(buffer.pop_char(), None);
    }

    #[test]
    fn pop_char_removes_last_char() {
        let mut buffer = Buffer::with_text("typoo");
        assert_eq!(buffer.to_text(), "typoo");
        assert_eq!(buffer.pop_char(), Some('o'));
        assert_eq!(buffer.to_text(), "typo");
    }
}

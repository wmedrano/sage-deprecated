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

    /// Iterate through all lines.
    pub fn iter_lines<'a>(&'a self) -> impl 'a + Iterator<Item = &'a str> {
        self.lines.iter().map(|s| s.as_str())
    }

    /// Push a new line onto the buffer.
    pub fn push_line(&mut self, line: &str) {
        self.lines.push(line.to_string());
    }

    /// Add a new character to the buffer.
    pub fn push_char(&mut self, ch: char) {
        if self.lines.is_empty() {
            self.push_line("");
        }
        if ch == '\n' {
            self.push_line("");
            return;
        }
        self.lines.last_mut().unwrap().push(ch);
    }

    /// Push several characters onto the buffer.
    pub fn push_chars(&mut self, chs: impl Iterator<Item = char>) {
        for ch in chs {
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

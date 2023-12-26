use flashkick::foreign_object::ForeignObjectType;

/// Buffer holds editable text.
#[derive(Default)]
pub struct Buffer {
    lines: Vec<String>,
}

impl ForeignObjectType for Buffer {
    const NAME: &'static str = "willy-buffer";
}

impl Buffer {
    /// Create a new blank buffer.
    pub fn new() -> Buffer {
        Buffer { lines: Vec::new() }
    }

    /// Create a new buffer from text.
    #[cfg(test)]
    pub fn with_text(text: &str) -> Buffer {
        Buffer {
            lines: text.split('\n').map(str::to_string).collect(),
        }
    }

    /// Convert the buffer into text
    pub fn to_text(&self) -> String {
        self.lines.join("\n")
    }

    pub fn iter_lines(&self) -> impl Iterator<Item = &str> {
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
        assert_eq!(buffer.to_text(), "");
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
        assert_eq!(buffer.to_text(), "name: ");

        buffer.push_chars(['w', 'i', 'l', 'l', 'y']);
        assert_eq!(buffer.to_text(), "name: willy");

        buffer.push_chars("\nfunction: editor".chars());
        assert_eq!(buffer.to_text(), "name: willy\nfunction: editor");
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

pub mod scm {
    use std::ffi::CStr;

    use flashkick::{
        foreign_object::ForeignObjectType,
        module::{Module, ModuleInitContext},
        Scm,
    };

    use super::Buffer;

    #[no_mangle]
    pub unsafe extern "C" fn scm_init_willy_internal_buffer_module() {
        BufferModule.init();
    }

    struct BufferModule;

    impl Module for BufferModule {
        fn name() -> &'static std::ffi::CStr {
            CStr::from_bytes_with_nul(b"willy internal buffer\0").unwrap()
        }

        unsafe fn define(&self, ctx: &mut ModuleInitContext) {
            ctx.define_type::<Buffer>();
            ctx.define_subr_0(
                CStr::from_bytes_with_nul(b"--new-buffer\0").unwrap(),
                scm_new_buffer,
            );
            ctx.define_subr_1(
                CStr::from_bytes_with_nul(b"--buffer-to-text\0").unwrap(),
                scm_buffer_to_text,
                1,
            );
            ctx.define_subr_2(
                CStr::from_bytes_with_nul(b"--buffer-insert-string\0").unwrap(),
                scm_buffer_insert_string,
                2,
            );
            ctx.define_subr_1(
                CStr::from_bytes_with_nul(b"--buffer-pop-char\0").unwrap(),
                scm_buffer_pop_char,
                1,
            );
        }
    }

    extern "C" fn scm_new_buffer() -> Scm {
        let buffer = Box::new(Buffer::new());
        unsafe { Buffer::to_scm(buffer) }
    }

    extern "C" fn scm_buffer_to_text(buffer: Scm) -> Scm {
        let buffer = unsafe { Buffer::from_scm(&buffer) };
        let text = buffer.to_text();
        unsafe { Scm::new_string(&text) }
    }

    extern "C" fn scm_buffer_insert_string(buffer: Scm, string: Scm) -> Scm {
        let mut buffer = buffer;
        let buffer = unsafe { Buffer::from_scm_mut(&mut buffer) };
        let string = unsafe { string.to_string() };
        buffer.push_chars(string.chars());
        Scm::EOL
    }

    extern "C" fn scm_buffer_pop_char(buffer: Scm) -> Scm {
        let mut buffer = buffer;
        let buffer = unsafe { Buffer::from_scm_mut(&mut buffer) };
        match buffer.pop_char() {
            Some(c) => unsafe {
                let mut text_buffer = [0u8; 4];
                Scm::new_string(c.encode_utf8(&mut text_buffer))
            },
            None => unsafe { Scm::new_string("") },
        }
    }
}

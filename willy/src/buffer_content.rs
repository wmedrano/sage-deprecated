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

    /// Convert the buffer into a string.
    pub fn to_string(&self) -> String {
        self.lines.join("\n")
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

impl std::fmt::Display for BufferContent {
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

pub mod scm {
    use std::ffi::CStr;

    use flashkick::{
        foreign_object::ForeignObjectType,
        module::{Module, ModuleInitContext},
        Scm,
    };

    use super::BufferContent;

    #[no_mangle]
    pub unsafe extern "C" fn scm_init_willy_internal_buffer_content() {
        BufferContentModule.init();
    }

    struct BufferContentModule;

    impl Module for BufferContentModule {
        fn name() -> &'static std::ffi::CStr {
            CStr::from_bytes_with_nul(b"willy internal\0").unwrap()
        }

        unsafe fn define(&self, ctx: &mut ModuleInitContext) {
            ctx.define_type::<BufferContent>();
            ctx.define_subr_0(
                CStr::from_bytes_with_nul(b"--make-buffer-content\0").unwrap(),
                scm_make_buffer_content,
            );
            ctx.define_subr_1(
                CStr::from_bytes_with_nul(b"--buffer-content-to-string\0").unwrap(),
                scm_buffer_content_to_string,
                1,
            );
            ctx.define_subr_2(
                CStr::from_bytes_with_nul(b"--buffer-content-insert-string\0").unwrap(),
                scm_buffer_content_insert_string,
                2,
            );
            ctx.define_subr_2(
                CStr::from_bytes_with_nul(b"--buffer-content-set-string\0").unwrap(),
                scm_buffer_content_set_string,
                2,
            );
            ctx.define_subr_1(
                CStr::from_bytes_with_nul(b"--buffer-content-pop-char\0").unwrap(),
                scm_buffer_content_pop_char,
                1,
            );
        }
    }

    extern "C" fn scm_make_buffer_content() -> Scm {
        let buffer = Box::new(BufferContent::new());
        unsafe { BufferContent::to_scm(buffer) }
    }

    extern "C" fn scm_buffer_content_to_string(buffer: Scm) -> Scm {
        let buffer = match unsafe { BufferContent::from_scm(&buffer) } {
            Some(b) => b,
            None => return unsafe { Scm::new_string("") },
        };
        let s = buffer.to_string();
        unsafe { Scm::new_string(&s) }
    }

    // TODO: Fix.
    extern "C" fn scm_buffer_content_insert_string(scm_buffer_content: Scm, string: Scm) -> Scm {
        let mut buffer_content = scm_buffer_content;
        let buffer_content = match unsafe { BufferContent::from_scm_mut(&mut buffer_content) } {
            Some(b) => b,
            None => return scm_buffer_content,
        };
        let string = unsafe { string.to_string() };
        buffer_content.push_chars(string.chars());
        scm_buffer_content
    }

    extern "C" fn scm_buffer_content_set_string(scm_buffer_content: Scm, string: Scm) -> Scm {
        let mut buffer_content = scm_buffer_content;
        let buffer_content = match unsafe { BufferContent::from_scm_mut(&mut buffer_content) } {
            Some(b) => b,
            None => return scm_buffer_content,
        };
        let s = unsafe { string.to_string() };
        *buffer_content = BufferContent::with_str(&s);
        scm_buffer_content
    }

    extern "C" fn scm_buffer_content_pop_char(buffer_content: Scm) -> Scm {
        let mut buffer_content = buffer_content;
        let buffer_content = match unsafe { BufferContent::from_scm_mut(&mut buffer_content) } {
            Some(b) => b,
            None => return unsafe { Scm::new_string("") },
        };
        match buffer_content.pop_char() {
            Some(c) => unsafe {
                let mut tmp_buffer = [0u8; 4];
                Scm::new_string(c.encode_utf8(&mut tmp_buffer))
            },
            None => unsafe { Scm::new_string("") },
        }
    }
}

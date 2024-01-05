use std::{
    ffi::CStr,
    fmt::Display,
    ops::{Deref, RangeBounds},
    panic::catch_unwind,
    time::Instant,
};

use flashkick::{
    err::ResultToScm, foreign_object::ForeignObjectType, module::ModuleInitContext, Scm,
};
use tree_sitter::{Parser, Tree};

/// Implements a rope datastructure for efficiently editing text.
pub struct Rope {
    inner: crop::Rope,
    modified_timestamp: Instant,
    parser: Option<Parser>,
    tree: Option<Tree>,
}

impl ForeignObjectType for Rope {
    const NAME: &'static str = "sage-rope";
}

/// A structure that  can be used to compare the state of a rope.
#[derive(PartialEq)]
pub struct RopeFingerprint {
    rope_id: *const Rope,
    modified_timestamp: Instant,
}

impl Rope {
    /// Create a new rope.
    pub fn new() -> Rope {
        Rope {
            inner: crop::Rope::new(),
            modified_timestamp: Instant::now(),
            parser: None,
            tree: None,
        }
    }

    /// Replace the contents within `byte_range` with `text`.
    pub fn replace<R, T>(&mut self, byte_range: R, text: T)
    where
        R: RangeBounds<usize>,
        T: AsRef<str>,
    {
        self.inner.replace(byte_range, text);
        self.modified_timestamp = Instant::now();
    }

    /// Get a fingerprint of the state. Can be used to see if anything has changed.
    pub fn fingerprint(&self) -> RopeFingerprint {
        RopeFingerprint {
            rope_id: self,
            modified_timestamp: self.modified_timestamp,
        }
    }

    pub fn tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }

    /// Sets the tree sitter parser.
    pub fn set_parser(&mut self, parser: Option<Parser>) {
        self.parser = parser;
        self.tree.take();
        self.reparse();
        self.modified_timestamp = Instant::now();
    }

    fn reparse(&mut self) {
        if let Some(parser) = self.parser.as_mut() {
            self.tree = parser.parse_with(
                &mut |start_byte, _| {
                    self.inner
                        .byte_slice(start_byte..)
                        .chunks()
                        .next()
                        .unwrap_or("")
                },
                self.tree.as_ref(),
            );
        }
    }
}

impl Deref for Rope {
    type Target = crop::Rope;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl Display for Rope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for chunk in self.inner.chunks() {
            f.write_str(chunk)?;
        }
        Ok(())
    }
}

impl Default for RopeFingerprint {
    fn default() -> Self {
        RopeFingerprint {
            rope_id: std::ptr::null(),
            modified_timestamp: Instant::now(),
        }
    }
}

pub unsafe fn define_rope(ctx: &mut ModuleInitContext) {
    ctx.define_type::<Rope>();
    ctx.define_subr_0(
        CStr::from_bytes_with_nul(b"make-rope\0").unwrap(),
        scm_make_rope,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"rope->string\0").unwrap(),
        scm_rope_to_string,
        1,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"rope-byte-length\0").unwrap(),
        scm_rope_byte_length,
        1,
    );
    ctx.define_subr_4(
        CStr::from_bytes_with_nul(b"rope-replace!\0").unwrap(),
        scm_rope_replace,
        4,
    );
    ctx.define_subr_2(
        CStr::from_bytes_with_nul(b"rope-set-language!\0").unwrap(),
        scm_rope_set_language,
        2,
    );
}

extern "C" fn scm_make_rope() -> Scm {
    catch_unwind(|| {
        let rope = unsafe { Rope::to_scm(Rope::new().into()) };
        rope
    })
    .map_err(|_| "Rust panic encountered on make-rope.")
    .scm_unwrap()
}

extern "C" fn scm_rope_to_string(rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        Scm::new_string(rope.to_string().as_str())
    })
    .map_err(|_| "Rust panic encountered on rope->string.")
    .scm_unwrap()
}

extern "C" fn scm_rope_byte_length(rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        Scm::new_u32(rope.byte_len() as u32)
    })
    .map_err(|_| "Rust panic encountered on rope->byte-length.")
    .scm_unwrap()
}

extern "C" fn scm_rope_replace(rope: Scm, start_byte: Scm, end_byte: Scm, text: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm_mut(rope).unwrap();
        let start = start_byte.to_u32() as usize;
        let end = end_byte.to_u32() as usize;
        if text.is_char() {
            let mut text_buffer = [0; 4];
            let char_str = text.to_char().unwrap().encode_utf8(&mut text_buffer);
            rope.replace(start..end, char_str);
        } else {
            rope.replace(start..end, text.to_string());
        }
    })
    .map_err(|_| "Rust panic encountered on rope-replace!.")
    .scm_unwrap();
    Scm::UNDEFINED
}

extern "C" fn scm_rope_set_language(rope: Scm, language: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm_mut(rope).unwrap();
        let language_str = language.to_string();
        let language = match language_str.as_str() {
            "rust" => Some(tree_sitter_rust::language()),
            "" => None,
            _ => panic!("Unknown language {language_str}",),
        };
        let parser = language.map(|l| {
            let mut p = Parser::new();
            p.set_language(l).scm_unwrap();
            p
        });
        rope.set_parser(parser);
    })
    .map_err(|_| "Rust panic encountered on rope-replace!.")
    .scm_unwrap();
    Scm::UNDEFINED
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_rope_creates_empty_rope() {
        let r = Rope::new();
        assert_eq!(r.to_string(), "");
    }

    #[test]
    fn test_rope() {
        let mut rope = crop::Rope::new();
        rope.insert(0, "This is my\ntext that\nwill be here.");
        let line = rope.line(1);
        assert_eq!(line.byte_slice(0..3), "tex");
        assert_eq!(line.byte_of_line(1), 0);
        assert_eq!(line.byte_len(), 9);
        assert!(false);
    }
}

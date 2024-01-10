use std::{
    ffi::CStr,
    fmt::Display,
    ops::{Deref, Range},
    panic::catch_unwind,
};

use anyhow::{anyhow, Result};
use flashkick::{
    err::{throw_error, ResultToScm},
    foreign_object::ForeignObjectType,
    module::ModuleInitContext,
    Scm,
};
use tree_sitter::{Language, Parser, Point, Tree};

use crate::theme::{HighlightRange, SyntaxTheme};

/// Implements a rope datastructure for efficiently editing text.
pub struct Rope {
    inner: ropey::Rope,
    parser: Option<Parser>,
    tree: Option<Tree>,
    syntax_theme: Option<SyntaxTheme>,
    highlights: Vec<HighlightRange>,
}

impl ForeignObjectType for Rope {
    const NAME: &'static str = "<sage-rope>";
}

impl Rope {
    /// Create a new rope.
    pub fn new() -> Rope {
        Rope {
            inner: ropey::Rope::new(),
            parser: None,
            tree: None,
            syntax_theme: None,
            highlights: Vec::new(),
        }
    }

    /// Replace the contents within `byte_range` with `text`. The new end point is returned.
    pub fn replace<T>(&mut self, char_range: Range<usize>, text: T) -> Result<usize>
    where
        T: AsRef<str>,
    {
        let text = text.as_ref();
        let byte_range_start = self.inner.try_char_to_byte(char_range.start)?;
        let byte_range_end = self.inner.try_char_to_byte(char_range.end)?;
        let byte_range = byte_range_start..byte_range_end;
        self.inner.try_remove(char_range.clone())?;
        // TODO: If try_remove succeeds in editing and try_insert fails, then the tree sitter tree
        // may be invalid as tree.edit will not be called.
        self.inner.try_insert(char_range.start, text)?;
        if let Some(tree) = self.tree.as_mut() {
            tree.edit(&tree_sitter::InputEdit {
                start_byte: byte_range.start,
                old_end_byte: byte_range.end,
                new_end_byte: byte_range.start + text.len(),
                start_position: Point::default(),
                old_end_position: Point::default(),
                new_end_position: Point::default(),
            });
            self.reparse();
        }
        Ok(char_range.start + text.chars().count())
    }

    /// Get the tree for the rope.
    pub fn tree(&self) -> Option<&Tree> {
        self.tree.as_ref()
    }

    /// Sets the tree sitter language.
    pub fn set_language<L: Into<Option<Language>>>(&mut self, language: L, highlights: &str) {
        self.parser.take();
        self.tree.take();
        self.syntax_theme.take();
        if let Some(language) = language.into() {
            self.parser = {
                let mut p = Parser::new();
                p.set_language(language).unwrap();
                Some(p)
            };
            self.syntax_theme = SyntaxTheme::new(language, highlights).into();
            self.tree.take();
            self.reparse();
        }
    }

    /// Update the highlights.
    pub fn update_highlights(&mut self) {
        if self.highlights.is_empty() {
            if let (Some(theme), Some(tree)) = (self.syntax_theme.as_mut(), self.tree.as_ref()) {
                self.highlights
                    .extend(theme.highlight(tree, RopeTextProvider(&self.inner)));
                self.highlights.sort_by_key(|h| h.range.start)
            }
        }
    }

    /// Get the current highlights for the rope.
    pub fn highlights(&self) -> &[HighlightRange] {
        &self.highlights
    }

    fn reparse(&mut self) {
        if let Some(parser) = self.parser.as_mut() {
            self.highlights.clear();
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
    type Target = ropey::Rope;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

pub(crate) struct RopeTextProvider<'a>(&'a ropey::Rope);

impl<'a> tree_sitter::TextProvider<'a> for RopeTextProvider<'a> {
    type I = std::iter::Map<ropey::iter::Chunks<'a>, fn(&str) -> &[u8]>;

    fn text(&mut self, node: tree_sitter::Node) -> Self::I {
        let start = node.start_byte();
        let end = node.end_byte();
        let chunks = self.0.byte_slice(start..end).chunks();
        chunks.map(|s| s.as_bytes())
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
        CStr::from_bytes_with_nul(b"rope-length\0").unwrap(),
        scm_rope_length,
        1,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"rope-line-count\0").unwrap(),
        scm_rope_line_count,
        1,
    );
    ctx.define_subr_2(
        CStr::from_bytes_with_nul(b"rope-line-length\0").unwrap(),
        scm_rope_line_length,
        2,
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
    ctx.define_subr_2(
        CStr::from_bytes_with_nul(b"rope-cursor->position\0").unwrap(),
        scm_rope_cursor_to_position,
        2,
    );
    ctx.define_subr_2(
        CStr::from_bytes_with_nul(b"rope-position->cursor\0").unwrap(),
        scm_rope_position_to_cursor,
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

extern "C" fn scm_rope_length(rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        Scm::new_u32(rope.len_chars() as u32)
    })
    .map_err(|_| "Rust panic encountered on rope-length.")
    .scm_unwrap()
}

extern "C" fn scm_rope_line_count(rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        Scm::new_u32(rope.len_lines() as u32)
    })
    .map_err(|_| "Rust panic encountered on rope-length.")
    .scm_unwrap()
}

extern "C" fn scm_rope_line_length(rope: Scm, line: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        let line_idx = line.to_u32() as usize;
        let line_count = rope.len_lines();
        if line_idx >= line_count {
            throw_error(anyhow!(
                "line {line_idx} is equal to or greater than line count {line_count}"
            ));
        }
        let line = rope.line(line_idx);
        Scm::new_u32(line.len_chars() as u32)
    })
    .map_err(|_| "Rust panic encountered on rope-length.")
    .scm_unwrap()
}

extern "C" fn scm_rope_replace(rope: Scm, start_byte: Scm, end_byte: Scm, text: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm_mut(rope).unwrap();
        let start = start_byte.to_u32() as usize;
        let end = end_byte.to_u32() as usize;
        let new_end = if text.is_char() {
            let mut text_buffer = [0; 4];
            let char_str = text.to_char().unwrap().encode_utf8(&mut text_buffer);
            rope.replace(start..end, char_str).scm_unwrap()
        } else {
            rope.replace(start..end, text.to_string()).scm_unwrap()
        };
        Scm::new_u32(new_end as u32)
    })
    .map_err(|_| "Rust panic encountered on rope-replace!.")
    .scm_unwrap()
}

extern "C" fn scm_rope_set_language(rope: Scm, language: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm_mut(rope).unwrap();
        let language_str = language.to_string();
        let language = match language_str.as_str() {
            "rust" => Some(tree_sitter_rust::language()),
            "scheme" => Some(tree_sitter_scheme::language()),
            "" => None,
            _ => panic!("Unknown language {language_str}",),
        };
        let highlights = match language_str.as_str() {
            "rust" => tree_sitter_rust::HIGHLIGHT_QUERY,
            "scheme" => tree_sitter_scheme::HIGHLIGHTS_QUERY,
            _ => "",
        };
        rope.set_language(language, highlights);
    })
    .map_err(|_| "Rust panic encountered on rope-replace!.")
    .scm_unwrap();
    Scm::UNDEFINED
}

extern "C" fn scm_rope_cursor_to_position(rope: Scm, cursor: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        let cursor = cursor.to_u32() as usize;
        if cursor >= rope.len_chars() {
            return Scm::FALSE;
        }
        let row = rope.char_to_line(cursor);
        let col = cursor - rope.line_to_char(row);
        Scm::cons(Scm::new_u32(row as u32), Scm::new_u32(col as u32))
    })
    .map_err(|_| "Rust panic encountered on rope-cursor->position.")
    .scm_unwrap()
}

extern "C" fn scm_rope_position_to_cursor(rope: Scm, position: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = Rope::from_scm(rope).unwrap();
        let (row, col) = (
            position.car().to_u32() as usize,
            position.cdr().to_u32() as usize,
        );
        let line_count = rope.len_lines();
        if row >= line_count {
            return Scm::FALSE;
        }
        let line_length = rope.line(row).len_chars();
        if col >= line_length {
            return Scm::FALSE;
        }
        let cursor = rope.line_to_char(row) + col;
        Scm::new_u32(cursor as u32)
    })
    .map_err(|_| "Rust panic encountered on rope-position->cursor.")
    .scm_unwrap()
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
    fn rope_char_length() {
        let mut r = Rope::new();
        r.replace(0..0, "123\n🤖robots🤖\n456\n8").unwrap();
        assert_eq!(r.len_chars(), 18);
        assert_eq!(r.len_lines(), 4);
        assert_eq!(r.line(1), "🤖robots🤖\n");
        assert_eq!(r.line(1).len_chars(), 9);
    }
}

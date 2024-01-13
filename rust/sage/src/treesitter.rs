use std::{ffi::CStr, panic::catch_unwind};

use anyhow::anyhow;
use flashkick::{
    err::{throw_error, ResultToScm},
    foreign_object::ForeignObjectType,
    module::ModuleInitContext,
    Scm,
};
use tree_sitter::Point;

use crate::rope::Rope;

/// Wrapper around the tree sitter parser.
pub struct Parser {
    inner: tree_sitter::Parser,
}

impl ForeignObjectType for Parser {
    const NAME: &'static str = "<sage-treesitter-parser>";
}

impl From<tree_sitter::Parser> for Parser {
    fn from(value: tree_sitter::Parser) -> Parser {
        Parser { inner: value }
    }
}

pub struct Tree {
    inner: tree_sitter::Tree,
}

impl ForeignObjectType for Tree {
    const NAME: &'static str = "<sage-treesitter-tree>";
}

impl From<tree_sitter::Tree> for Tree {
    fn from(value: tree_sitter::Tree) -> Self {
        Tree { inner: value }
    }
}

pub unsafe fn define_treesitter(ctx: &mut ModuleInitContext) {
    ctx.define_type::<Parser>();
    ctx.define_type::<Tree>();
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"make-parser\0").unwrap(),
        scm_make_parser,
        1,
    );
    ctx.define_subr_3(
        CStr::from_bytes_with_nul(b"parser-parse!\0").unwrap(),
        scm_parser_parse,
        2,
    );
    ctx.define_subr_4(
        CStr::from_bytes_with_nul(b"tree-edit!\0").unwrap(),
        scm_tree_edit,
        4,
    );
}

extern "C" fn scm_make_parser(language_name: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let language = match language_name.to_symbol().as_str() {
            "rust" => tree_sitter_rust::language(),
            "scheme" => tree_sitter_scheme::language(),
            l => throw_error(anyhow!("language {l} not supported")),
        };
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(language).scm_unwrap();
        Parser::to_scm(Box::new(Parser::from(parser)))
    })
    .map_err(|_| "Rust panic encountered on make-rope.")
    .scm_unwrap()
}

extern "C" fn scm_parser_parse(parser: Scm, rope: Scm, previous_tree: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let parser = Parser::from_scm_mut(parser).unwrap();
        let rope = Rope::from_scm(rope).unwrap();
        let previous_tree = if previous_tree.is_undefined() || !previous_tree.is_true() {
            None
        } else {
            Tree::from_scm(previous_tree)
        };
        let maybe_tree = parser.inner.parse_with(
            &mut |start_byte, _| rope.byte_slice(start_byte..).chunks().next().unwrap_or(""),
            previous_tree.map(|t| &t.inner),
        );
        match maybe_tree {
            None => Scm::FALSE,
            Some(t) => Tree::to_scm(Box::new(Tree::from(t))),
        }
    })
    .map_err(|_| "Rust panic encountered on parser-parse!")
    .scm_unwrap()
}

extern "C" fn scm_tree_edit(
    tree: Scm,
    start_byte: Scm,
    old_end_byte: Scm,
    new_end_byte: Scm,
) -> Scm {
    catch_unwind(|| unsafe {
        let start_byte = start_byte.to_u32() as usize;
        let old_end_byte = old_end_byte.to_u32() as usize;
        let new_end_byte = new_end_byte.to_u32() as usize;
        let tree = Tree::from_scm_mut(tree).unwrap();
        tree.inner.edit(&tree_sitter::InputEdit {
            start_byte,
            old_end_byte,
            new_end_byte,
            start_position: Point::default(),
            old_end_position: Point::default(),
            new_end_position: Point::default(),
        });
        Scm::UNDEFINED
    })
    .map_err(|_| "Rust panic encountered on tree-edit!.")
    .scm_unwrap()
}

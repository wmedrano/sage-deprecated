use std::sync::OnceLock;

use flashkick::Scm;

static SCM_OBJECT_CACHE: OnceLock<ScmObjectCache> = OnceLock::new();

pub fn cache() -> &'static ScmObjectCache {
    SCM_OBJECT_CACHE.get_or_init(|| unsafe {
        ScmObjectCache {
            symbols: Symbols {
                border: Scm::new_symbol("border").permanent(),
                buffer: Scm::new_symbol("buffer").permanent(),
                content: Scm::new_symbol("content").permanent(),
                cursor: Scm::new_symbol("cursor").permanent(),
                features: Scm::new_symbol("features").permanent(),
                height: Scm::new_symbol("height").permanent(),
                highlight_line: Scm::new_symbol("highlight-line").permanent(),
                line_numbers: Scm::new_symbol("line-numbers").permanent(),
                terminal: Scm::new_symbol("terminal").permanent(),
                test: Scm::new_symbol("test").permanent(),
                width: Scm::new_symbol("width").permanent(),
                x: Scm::new_symbol("x").permanent(),
                y: Scm::new_symbol("y").permanent(),
                press: Scm::new_symbol("press").permanent(),
                release: Scm::new_symbol("release").permanent(),
                repeat: Scm::new_symbol("repeat").permanent(),
                event_type: Scm::new_symbol("event-type").permanent(),
                key: Scm::new_symbol("key").permanent(),
                shift_question: Scm::new_symbol("shift?").permanent(),
                ctrl_question: Scm::new_symbol("ctrl?").permanent(),
                alt_question: Scm::new_symbol("alt?").permanent(),
            },
            key_codes: KeyCodes {
                backspace: Scm::new_string("<backspace>").permanent(),
                down: Scm::new_string("<down>").permanent(),
                esc: Scm::new_string("<esc>").permanent(),
                left: Scm::new_string("<left>").permanent(),
                right: Scm::new_string("<right>").permanent(),
                up: Scm::new_string("<up>").permanent(),
                back_tab: Scm::new_string("<back-tab>").permanent(),
                shift: Scm::new_string("<shift>").permanent(),
                ctrl: Scm::new_string("<ctrl>").permanent(),
                alt: Scm::new_string("<alt>").permanent(),
            },
        }
    })
}

pub struct ScmObjectCache {
    pub symbols: Symbols,
    pub key_codes: KeyCodes,
}

pub struct Symbols {
    pub alt_question: Scm,
    pub border: Scm,
    pub buffer: Scm,
    pub content: Scm,
    pub ctrl_question: Scm,
    pub cursor: Scm,
    pub event_type: Scm,
    pub features: Scm,
    pub height: Scm,
    pub highlight_line: Scm,
    pub key: Scm,
    pub line_numbers: Scm,
    pub press: Scm,
    pub release: Scm,
    pub repeat: Scm,
    pub shift_question: Scm,
    pub terminal: Scm,
    pub test: Scm,
    pub width: Scm,
    pub x: Scm,
    pub y: Scm,
}

pub struct KeyCodes {
    pub backspace: Scm,
    pub down: Scm,
    pub esc: Scm,
    pub left: Scm,
    pub right: Scm,
    pub up: Scm,
    pub back_tab: Scm,
    pub shift: Scm,
    pub ctrl: Scm,
    pub alt: Scm,
}

unsafe impl Send for ScmObjectCache {}
unsafe impl Sync for ScmObjectCache {}

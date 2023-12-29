use anyhow::anyhow;
use ratatui::prelude::Rect;
use std::{ffi::CStr, panic::catch_unwind};

use crate::buffer_content::{BufferContent, EMPTY_BUFFER_CONTENT};
use crate::frame_limiter::FrameLimiter;
use crate::tui::{next_event, widgets::BufferWidget, BackendType, Tui, Window};
use crossterm::event;
use flashkick::{
    err::{throw_error, ResultToScm},
    foreign_object::ForeignObjectType,
    module::{Module, ModuleInitContext},
    Scm,
};

/// Guile Scheme module for for willy.
pub struct WillyCoreInternalModule;

impl Module for WillyCoreInternalModule {
    fn name() -> &'static std::ffi::CStr {
        CStr::from_bytes_with_nul(b"willy core internal\0").unwrap()
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

        ctx.define_type::<Tui>();
        ctx.define_subr_0(
            CStr::from_bytes_with_nul(b"--next-event-from-terminal\0").unwrap(),
            scm_next_event_from_terminal,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--make-tui\0").unwrap(),
            scm_make_tui,
            1,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--delete-tui\0").unwrap(),
            scm_delete_tui,
            1,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--tui-size\0").unwrap(),
            scm_tui_size,
            1,
        );
        ctx.define_subr_2(
            CStr::from_bytes_with_nul(b"--tui-draw\0").unwrap(),
            scm_tui_draw,
            2,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--tui-state-for-test\0").unwrap(),
            scm_tui_state_for_test,
            1,
        );

        ctx.define_type::<FrameLimiter>();
    }
}

extern "C" fn scm_make_buffer_content() -> Scm {
    let buffer = Box::new(BufferContent::new());
    unsafe { BufferContent::to_scm(buffer) }
}

extern "C" fn scm_buffer_content_to_string(buffer: Scm) -> Scm {
    let buffer = match unsafe { BufferContent::from_scm(buffer).as_ref() } {
        Some(b) => b,
        None => return unsafe { Scm::new_string("") },
    };
    let s = buffer.to_string();
    unsafe { Scm::new_string(&s) }
}

extern "C" fn scm_buffer_content_insert_string(buffer_content: Scm, string: Scm) -> Scm {
    match unsafe { BufferContent::from_scm(buffer_content).as_mut() } {
        Some(b) => b,
        None => return buffer_content,
    }
    .push_chars(unsafe { string.to_string() }.chars());
    buffer_content
}

extern "C" fn scm_buffer_content_set_string(scm_buffer_content: Scm, string: Scm) -> Scm {
    let buffer_content = match unsafe { BufferContent::from_scm(scm_buffer_content).as_mut() } {
        Some(b) => b,
        None => return scm_buffer_content,
    };
    let s = unsafe { string.to_string() };
    *buffer_content = BufferContent::with_str(&s);
    scm_buffer_content
}

extern "C" fn scm_buffer_content_pop_char(buffer_content: Scm) -> Scm {
    let bc = match unsafe { BufferContent::from_scm(buffer_content).as_mut() } {
        Some(b) => b,
        None => return unsafe { Scm::new_string("") },
    };
    match bc.pop_char() {
        Some(c) => unsafe {
            let mut tmp_buffer = [0u8; 16];
            Scm::new_string(c.encode_utf8(&mut tmp_buffer))
        },
        None => unsafe { Scm::new_string("") },
    }
}
pub extern "C" fn scm_next_event_from_terminal() -> Scm {
    catch_unwind(|| {
        let e = match next_event() {
            Some(e) => e,
            None => return Scm::FALSE,
        };
        unsafe { event_to_scm(e) }.unwrap_or(Scm::FALSE)
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

extern "C" fn scm_make_tui(backend_type: Scm) -> Scm {
    catch_unwind(|| {
        let backend_type = match unsafe { backend_type.to_symbol().as_str() } {
            "terminal" => BackendType::Terminal,
            "test" => BackendType::Test,
            t => unsafe {
                throw_error(anyhow!(
                    "unknown backend type {t}, valid values are 'terminal and 'test"
                ))
            },
        };
        let tui = Box::new(Tui::new(backend_type).scm_unwrap());
        unsafe { Tui::to_scm(tui) }
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

extern "C" fn scm_delete_tui(tui: Scm) -> Scm {
    catch_unwind(|| {
        unsafe { Tui::scm_drop(tui) };
        Scm::EOL
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

extern "C" fn scm_tui_size(tui: Scm) -> Scm {
    catch_unwind(|| {
        let make_size = |width, height| unsafe {
            Scm::with_alist([
                (Scm::new_symbol("width"), Scm::new_u16(width)),
                (Scm::new_symbol("height"), Scm::new_u16(height)),
            ])
        };
        let tui = match unsafe { Tui::from_scm(tui).as_ref() } {
            Some(t) => t,
            None => return make_size(0, 0),
        };
        let (width, height) = tui.size().scm_unwrap();
        make_size(width, height)
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

unsafe fn scm_widget_to_widget(widget: Scm) -> Window<'static> {
    let mut ret = BufferWidget {
        buffer: &EMPTY_BUFFER_CONTENT,
        line_numbers: false,
        highlight_line: false,
        cursor: false,
        border: false,
    };
    let mut area = Rect {
        x: 0,
        y: 0,
        width: 0,
        height: 0,
    };
    for (key, value) in widget.iter_pairs() {
        let key = key.to_symbol();
        match key.as_str() {
            "buffer" => {
                if let Some((_, b)) = value.iter_pairs().find(|(k, _)| k.to_symbol() == "content") {
                    ret.buffer = BufferContent::from_scm(b)
                        .as_ref()
                        .unwrap_or(&EMPTY_BUFFER_CONTENT);
                }
            }
            "x" => area.x = value.to_f64() as _,
            "y" => area.y = value.to_f64() as _,
            "width" => area.width = value.to_f64() as _,
            "height" => area.height = value.to_f64() as _,
            "features" => {
                for (feature, feature_value) in value.iter_pairs() {
                    match feature.to_symbol().as_str() {
                        "line-numbers" => ret.line_numbers = feature_value.to_bool(),
                        "highlight-line" => ret.highlight_line = feature_value.to_bool(),
                        "cursor" => ret.cursor = feature_value.to_bool(),
                        "border" => ret.border = feature_value.to_bool(),
                        _ => (),
                    }
                }
            }
            _ => (),
        }
    }
    Window { area, widget: ret }
}

extern "C" fn scm_tui_draw(tui: Scm, windows: Scm) -> Scm {
    catch_unwind(|| {
        unsafe {
            let tui = match Tui::from_scm(tui).as_mut() {
                Some(t) => t,
                None => return Scm::EOL,
            };
            let widgets = windows.iter().map(|scm| scm_widget_to_widget(scm));
            tui.draw(widgets)
        }
        .scm_unwrap();
        Scm::EOL
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

extern "C" fn scm_tui_state_for_test(tui: Scm) -> Scm {
    catch_unwind(|| {
        let tui = match unsafe { Tui::from_scm(tui).as_ref() } {
            Some(t) => t,
            None => return unsafe { Scm::new_string("") },
        };
        let state = tui.state().scm_unwrap();
        unsafe { Scm::new_string(&state) }
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

unsafe fn event_to_scm(e: event::Event) -> Option<Scm> {
    match e {
        event::Event::Key(event::KeyEvent {
            code,
            kind,
            modifiers,
            ..
        }) => {
            let mut tmp_ch_buffer = [0u8; 4];
            let key = match code {
                event::KeyCode::Char(ch) => ch.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Enter => '\n'.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Tab => '\t'.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Backspace => "<backspace>",
                event::KeyCode::Down => "<down>",
                event::KeyCode::Esc => "<esc>",
                event::KeyCode::Left => "<left>",
                event::KeyCode::Right => "<right>",
                event::KeyCode::Up => "<up>",
                _ => return None,
            };
            let event_type = match kind {
                event::KeyEventKind::Press => Scm::new_symbol("press"),
                event::KeyEventKind::Release => Scm::new_symbol("release"),
                event::KeyEventKind::Repeat => return None,
            };
            let alist = Scm::with_alist([
                (Scm::new_symbol("event-type"), event_type),
                (Scm::new_symbol("key"), Scm::new_string(key)),
                (
                    Scm::new_symbol("shift?"),
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::SHIFT)),
                ),
                (
                    Scm::new_symbol("ctrl?"),
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::CONTROL)),
                ),
                (
                    Scm::new_symbol("alt?"),
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::ALT)),
                ),
            ]);
            Some(alist)
        }
        _ => None,
    }
}

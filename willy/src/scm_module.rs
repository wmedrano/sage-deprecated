use anyhow::anyhow;
use flashkick::without_guile;
use ratatui::prelude::Rect;
use std::{ffi::CStr, panic::catch_unwind};

use crate::buffer_content::{BufferContent, EMPTY_BUFFER_CONTENT};
use crate::frame_limiter::FrameLimiter;
use crate::scm_object_cache::cache;
use crate::tui::widgets::HighlightLine;
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
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--buffer-content-clear\0").unwrap(),
            scm_buffer_content_clear,
            1,
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
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--make-frame-limiter\0").unwrap(),
            scm_make_frame_limiter,
            1,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"--limit-frames\0").unwrap(),
            scm_limit_frames,
            1,
        );
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
    let b = match unsafe { BufferContent::from_scm(buffer_content).as_mut() } {
        Some(b) => b,
        None => return buffer_content,
    };
    if unsafe { string.is_char() } {
        b.push_chars(unsafe { string.to_char() });
    } else {
        b.push_chars(unsafe { string.to_string() }.chars());
    }
    buffer_content
}

extern "C" fn scm_buffer_content_clear(scm_buffer_content: Scm) -> Scm {
    if let Some(bc) = unsafe { BufferContent::from_scm(scm_buffer_content).as_mut() } {
        bc.clear();
    }
    scm_buffer_content
}

extern "C" fn scm_buffer_content_pop_char(buffer_content: Scm) -> Scm {
    let bc = match unsafe { BufferContent::from_scm(buffer_content).as_mut() } {
        Some(b) => b,
        None => return Scm::FALSE,
    };
    match bc.pop_char() {
        Some(c) => unsafe { Scm::new_char(c) },
        None => Scm::FALSE,
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
        let backend_type = unsafe {
            if backend_type.is_eq(cache().symbols.terminal) {
                BackendType::Terminal
            } else if backend_type.is_eq(cache().symbols.test) {
                BackendType::Test
            } else {
                throw_error(anyhow!(
                    "unknown backend type {t}, valid values are 'terminal and 'test",
                    t = backend_type.to_symbol(),
                ))
            }
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
                (cache().symbols.width, Scm::new_u16(width)),
                (cache().symbols.height, Scm::new_u16(height)),
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
        highlight_line: HighlightLine::None,
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
        match key {
            k if k.is_eq(cache().symbols.buffer) => {
                if let Some((_, b)) = value
                    .iter_pairs()
                    .find(|(k, _)| k.is_eq(cache().symbols.content))
                {
                    ret.buffer = BufferContent::from_scm(b)
                        .as_ref()
                        .unwrap_or(&EMPTY_BUFFER_CONTENT);
                }
            }
            k if k.is_eq(cache().symbols.x) => area.x = value.to_f64() as _,
            k if k.is_eq(cache().symbols.y) => area.y = value.to_f64() as _,
            k if k.is_eq(cache().symbols.width) => area.width = value.to_f64() as _,
            k if k.is_eq(cache().symbols.height) => area.height = value.to_f64() as _,
            k if k.is_eq(cache().symbols.features) => {
                for (feature, feature_value) in value.iter_pairs() {
                    match feature {
                        f if f.is_eq(cache().symbols.line_numbers) => {
                            ret.line_numbers = feature_value.to_bool()
                        }
                        f if f.is_eq(cache().symbols.highlight_line) => {
                            ret.highlight_line = if feature_value.is_number() {
                                HighlightLine::Index(feature_value.to_u32() as usize)
                            } else if feature_value.to_bool() {
                                HighlightLine::Last
                            } else {
                                HighlightLine::None
                            };
                        }
                        f if f.is_eq(cache().symbols.cursor) => {
                            ret.cursor = feature_value.to_bool()
                        }
                        f if f.is_eq(cache().symbols.border) => {
                            ret.border = feature_value.to_bool()
                        }
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
            let widgets: Vec<_> = windows
                .iter()
                .map(|scm| scm_widget_to_widget(scm))
                .collect();
            without_guile(|| tui.draw(widgets.into_iter().rev()))
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
    let cache = cache();
    match e {
        event::Event::Key(event::KeyEvent {
            code,
            kind,
            modifiers,
            ..
        }) => {
            let key = match code {
                event::KeyCode::Char(ch) => Scm::new_char(ch),
                event::KeyCode::Enter => Scm::new_char('\n'),
                event::KeyCode::Tab => Scm::new_char('\t'),
                event::KeyCode::Backspace => cache.key_codes.backspace,
                event::KeyCode::Down => cache.key_codes.down,
                event::KeyCode::Esc => cache.key_codes.esc,
                event::KeyCode::Left => cache.key_codes.left,
                event::KeyCode::Right => cache.key_codes.right,
                event::KeyCode::Up => cache.key_codes.up,
                event::KeyCode::BackTab => cache.key_codes.back_tab,
                event::KeyCode::Modifier(
                    event::ModifierKeyCode::LeftShift | event::ModifierKeyCode::RightShift,
                ) => cache.key_codes.shift,
                event::KeyCode::Modifier(
                    event::ModifierKeyCode::LeftControl | event::ModifierKeyCode::RightControl,
                ) => cache.key_codes.ctrl,
                event::KeyCode::Modifier(
                    event::ModifierKeyCode::LeftAlt | event::ModifierKeyCode::RightAlt,
                ) => cache.key_codes.alt,
                _ => return None,
            };
            let event_type = match kind {
                event::KeyEventKind::Press => cache.symbols.press,
                // Consider keeping only press as release and repeat events are not being provided
                // by crossterm at the moment.
                event::KeyEventKind::Release => cache.symbols.release,
                event::KeyEventKind::Repeat => cache.symbols.repeat,
            };
            let alist = [
                (cache.symbols.event_type, event_type),
                (cache.symbols.key, key),
                (
                    cache.symbols.shift_question,
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::SHIFT)),
                ),
                (
                    cache.symbols.ctrl_question,
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::CONTROL)),
                ),
                (
                    cache.symbols.alt_question,
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::ALT)),
                ),
            ]
            .into_iter()
            .filter(|(_, v)| v.to_bool());
            Some(Scm::with_alist(alist))
        }
        _ => None,
    }
}

extern "C" fn scm_make_frame_limiter(target_fps: Scm) -> Scm {
    catch_unwind(|| unsafe {
        FrameLimiter::to_scm(Box::new(FrameLimiter::new(target_fps.to_f64() as u16)))
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

extern "C" fn scm_limit_frames(frame_limiter: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let limiter = FrameLimiter::from_scm(frame_limiter);
        let res = without_guile(|| limiter.as_mut().map(|f| f.limit()).unwrap_or(false));
        Scm::new_bool(res)
    })
    .map_err(|e| format!("{e:?}"))
    .scm_unwrap()
}

use anyhow::anyhow;
use flashkick::without_guile;
use ratatui::prelude::Rect;
use std::{ffi::CStr, panic::catch_unwind};

use crate::buffer_content::{BufferContent, EMPTY_BUFFER_CONTENT};
use crate::frame_limiter::FrameLimiter;
use crate::scm_object_cache::cache;
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
        ctx.define_subr_3(
            CStr::from_bytes_with_nul(b"--buffer-content-insert-string\0").unwrap(),
            scm_buffer_content_insert_string,
            2,
        );
        ctx.define_subr_2(
            CStr::from_bytes_with_nul(b"--buffer-content-set-string\0").unwrap(),
            scm_buffer_content_set_string,
            2,
        );
        ctx.define_subr_2(
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

extern "C" fn scm_buffer_content_insert_string(buffer_content: Scm, string: Scm, line: Scm) -> Scm {
    let line = if line.is_undefined() || !unsafe { line.to_bool() } {
        None
    } else {
        Some(unsafe { line.to_u64() } as usize)
    };
    match unsafe { BufferContent::from_scm(buffer_content).as_mut() } {
        Some(b) => b,
        None => return buffer_content,
    }
    .push_chars(unsafe { string.to_string() }.chars(), line);
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

extern "C" fn scm_buffer_content_pop_char(buffer_content: Scm, line_number: Scm) -> Scm {
    let bc = match unsafe { BufferContent::from_scm(buffer_content).as_mut() } {
        Some(b) => b,
        None => return unsafe { Scm::new_string("") },
    };
    let line_number = if line_number.is_undefined() || !unsafe { line_number.to_bool() } {
        None
    } else {
        Some(unsafe { line_number.to_u64() } as usize)
    };
    match bc.pop_char(line_number) {
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
                            ret.highlight_line = feature_value.to_bool()
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
                event::KeyEventKind::Press => cache().symbols.press,
                event::KeyEventKind::Release => cache().symbols.release,
                event::KeyEventKind::Repeat => return None,
            };
            let alist = Scm::with_alist([
                (cache().symbols.event_type, event_type),
                (cache().symbols.key, Scm::new_string(key)),
                (
                    cache().symbols.shift_question,
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::SHIFT)),
                ),
                (
                    cache().symbols.ctrl_question,
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::CONTROL)),
                ),
                (
                    cache().symbols.ctrl_question,
                    Scm::new_bool(modifiers.contains(event::KeyModifiers::ALT)),
                ),
            ]);
            Some(alist)
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

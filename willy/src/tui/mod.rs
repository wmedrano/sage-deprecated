use std::time::{Duration, Instant};

use anyhow::{bail, Result};
use crossterm::event;
use flashkick::{foreign_object::ForeignObjectType, Scm};
use ratatui::{prelude::Rect, style::Stylize, widgets::Block, Frame, Terminal};

use self::{terminal_backends::TerminalBackend, theme::ONEDARK_THEME, widgets::BufferWidget};

mod terminal_backends;
mod theme;
mod widgets;

const TARGET_FPS: u64 = 60;

pub struct Tui {
    terminal: Terminal<TerminalBackend>,
    previous_draw_time: Instant,
}

impl ForeignObjectType for Tui {
    const NAME: &'static str = "willy-tui";
}

#[derive(Copy, Clone)]
pub enum BackendType {
    Default,
    Test,
}

impl Tui {
    /// Create a new terminal UI with the given backend.
    pub fn new(backend_type: BackendType) -> Result<Tui> {
        let backend = TerminalBackend::new(backend_type)?;
        let mut terminal = Terminal::new(backend)?;
        terminal.hide_cursor()?;
        Ok(Tui {
            terminal,
            previous_draw_time: Instant::now(),
        })
    }

    /// Get the size of the terminal.
    pub fn size(&self) -> Result<(u16, u16)> {
        let sz = self.terminal.size()?;
        Ok((sz.width, sz.height))
    }

    /// Get the current state of the buffer.
    ///
    /// Only valid if the backend is test.
    pub fn state(&self) -> Result<String> {
        match self.terminal.backend() {
            TerminalBackend::Default(_) => {
                bail!("it is invalid to get state for the default tui backend")
            }
            TerminalBackend::Test(b) => {
                let width = self.size()?.0 as usize;
                let cells: Vec<&str> = b.buffer().content().iter().map(|c| c.symbol()).collect();
                let lines: Vec<String> = cells.chunks_exact(width).map(|l| l.join("")).collect();
                Ok(lines.join("\n"))
            }
        }
    }

    /// Draw the widgets at the screen. Calls to draw are throttled to a reasonable fps.
    pub fn draw<'a>(&mut self, widgets: impl Iterator<Item = Layout<'a>>) -> Result<()> {
        self.terminal.draw(|frame: &mut Frame| {
            let window_area = frame.size();
            frame.render_widget(Block::default().bg(ONEDARK_THEME.black1), window_area);
            let should_render_widget = |w: &Layout| {
                w.area.width > 0 && w.area.height > 0 && window_area.intersection(w.area) == w.area
            };
            for widget in widgets.filter(should_render_widget) {
                frame.render_widget(widget.widget, widget.area);
            }
        })?;
        let target_time =
            self.previous_draw_time + Duration::from_nanos(1_000_000_000 / TARGET_FPS);
        let delay = target_time.duration_since(Instant::now());
        std::thread::sleep(delay);
        self.previous_draw_time = Instant::now();
        Ok(())
    }
}

/// TODO: Remove indirection and use the real widgets directly.
pub struct Layout<'a> {
    pub area: Rect,
    pub widget: BufferWidget<'a>,
}

fn next_event() -> Option<event::Event> {
    let is_ready =
        crossterm::event::poll(Duration::ZERO).expect("could not get next event in event loop");
    if !is_ready {
        return None;
    }
    let e = crossterm::event::read().expect("could not read next event in event loop");
    Some(e)
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
                event::KeyCode::Enter => '\n'.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Tab => '\t'.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Backspace => "<backspace>",
                event::KeyCode::Esc => "<esc>",
                event::KeyCode::Char(ch) => ch.encode_utf8(&mut tmp_ch_buffer),
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

pub mod scm {
    use std::{ffi::CStr, panic::catch_unwind};

    use anyhow::anyhow;
    use flashkick::{
        err::{throw_error, ResultToScm},
        foreign_object::ForeignObjectType,
        module::Module,
        without_guile, Scm,
    };
    use ratatui::prelude::Rect;

    use crate::buffer_content::{BufferContent, EMPTY_BUFFER_CONTENT};

    use super::{event_to_scm, next_event, widgets::BufferWidget, BackendType, Layout, Tui};

    /// Initialize the `(willy tui)` module.
    ///
    /// # Safety
    /// Calls unsafe code.
    #[no_mangle]
    pub unsafe extern "C" fn scm_init_willy_internal_tui_module() {
        catch_unwind(|| {
            TuiModule.init();
        })
        .map_err(|err| format!("{err:?}"))
        .scm_unwrap();
    }

    struct TuiModule;

    impl Module for TuiModule {
        fn name() -> &'static std::ffi::CStr {
            CStr::from_bytes_with_nul(b"willy internal\0").unwrap()
        }

        unsafe fn define(&self, ctx: &mut flashkick::module::ModuleInitContext) {
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
                "default" => BackendType::Default,
                "test" => BackendType::Test,
                t => unsafe {
                    throw_error(anyhow!(
                        "unknown backend type {t}, valid values are 'default and 'dummy"
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
            let tui = match unsafe { Tui::from_scm(&tui) } {
                Some(t) => t,
                None => return make_size(0, 0),
            };
            let (width, height) = tui.size().scm_unwrap();
            make_size(width, height)
        })
        .map_err(|e| format!("{e:?}"))
        .scm_unwrap()
    }

    unsafe fn scm_widget_to_widget(widget: Scm) -> Layout<'static> {
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
                    if let Some((_, b)) =
                        value.iter_pairs().find(|(k, _)| k.to_symbol() == "content")
                    {
                        ret.buffer = BufferContent::ptr_from_scm(b)
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
        Layout { area, widget: ret }
    }

    extern "C" fn scm_tui_draw(tui: Scm, widgets: Scm) -> Scm {
        catch_unwind(|| {
            let mut tui = tui;
            unsafe {
                let tui = match Tui::from_scm_mut(&mut tui) {
                    Some(t) => t,
                    None => return Scm::EOL,
                };
                let widgets = widgets.iter().map(|scm| scm_widget_to_widget(scm));
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
            let mut tui = tui;
            let tui = match unsafe { Tui::from_scm_mut(&mut tui) } {
                Some(t) => t,
                None => return unsafe { Scm::new_string("") },
            };
            let state = tui.state().scm_unwrap();
            unsafe { Scm::new_string(&state) }
        })
        .map_err(|e| format!("{e:?}"))
        .scm_unwrap()
    }
}

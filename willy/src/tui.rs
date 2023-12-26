use std::time::Duration;

use anyhow::{bail, Result};
use crossterm::event;
use flashkick::{foreign_object::ForeignObjectType, Scm};
use ratatui::{
    prelude::{Constraint, Direction, Layout, Rect},
    widgets::Paragraph,
    Frame, Terminal,
};

use crate::buffer::Buffer;

use self::terminal_backends::TerminalBackend;

mod terminal_backends;

pub struct Tui {
    terminal: Terminal<TerminalBackend>,
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
    pub fn new(backend_type: BackendType) -> Result<Tui> {
        let backend = TerminalBackend::new(backend_type)?;
        let mut terminal = Terminal::new(backend)?;
        terminal.hide_cursor()?;
        Ok(Tui { terminal })
    }

    pub fn size(&self) -> Result<(u16, u16)> {
        let sz = self.terminal.size()?;
        Ok((sz.width, sz.height))
    }

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

    pub fn draw<'a>(&mut self, buffers: impl Clone + Iterator<Item = &'a Buffer>) -> Result<()> {
        self.terminal.draw(|frame: &mut Frame| {
            let main_layout = MainLayout::new(frame);
            frame.render_widget(
                Paragraph::new("Looking at a buffer in Willy!"),
                main_layout.status,
            );

            let buffers_count = buffers.clone().count() as u32;
            let buffers_layout = Layout::new(
                Direction::Vertical,
                buffers.clone().map(|_| Constraint::Ratio(1, buffers_count)),
            )
            .split(main_layout.buffers);
            for (buffer, layout) in buffers.zip(buffers_layout.iter()) {
                frame.render_widget(Paragraph::new(buffer.to_text() + "_"), *layout);
            }
        })?;
        Ok(())
    }
}

pub struct MainLayout {
    buffers: Rect,
    status: Rect,
}

impl MainLayout {
    fn new(f: &Frame) -> MainLayout {
        let layouts = Layout::new(
            Direction::Vertical,
            [
                Constraint::Length(f.size().height - 1),
                Constraint::Length(1),
            ],
        )
        .split(f.size());
        let layouts: &[Rect] = &layouts;
        match &layouts {
            [main, status] => MainLayout {
                buffers: *main,
                status: *status,
            },
            _ => unreachable!(),
        }
    }
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
            let ch = match code {
                event::KeyCode::Enter => '\n'.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Tab => '\t'.encode_utf8(&mut tmp_ch_buffer),
                event::KeyCode::Backspace => "<backspace>",
                event::KeyCode::Char(ch) => ch.encode_utf8(&mut tmp_ch_buffer),
                _ => return None,
            };
            let event_type = match kind {
                event::KeyEventKind::Press => Scm::new_symbol("press"),
                event::KeyEventKind::Release => Scm::new_symbol("release"),
                event::KeyEventKind::Repeat => return None,
            };
            let is_ctrl = modifiers.contains(event::KeyModifiers::CONTROL);
            let alist = Scm::with_alist([
                (Scm::new_keyword("event-type"), event_type),
                (Scm::new_keyword("char"), Scm::new_string(ch)),
                (Scm::new_keyword("ctrl?"), Scm::new_bool(is_ctrl)),
            ]);
            Some(alist)
        }
        _ => None,
    }
}

mod scm {
    use std::ffi::CStr;

    use anyhow::anyhow;
    use flashkick::{
        err::{throw_error, ResultToScm},
        foreign_object::ForeignObjectType,
        module::Module,
        Scm,
    };

    use crate::buffer::Buffer;

    use super::{event_to_scm, next_event, BackendType, Tui};

    /// Initialize the `(willy tui)` module.
    ///
    /// # Safety
    /// Calls unsafe code.
    #[no_mangle]
    pub unsafe extern "C" fn scm_init_willy_internal_tui_module() {
        TuiModule.init();
    }

    struct TuiModule;

    impl Module for TuiModule {
        fn name() -> &'static std::ffi::CStr {
            CStr::from_bytes_with_nul(b"willy internal tui\0").unwrap()
        }

        unsafe fn define(&self, ctx: &mut flashkick::module::ModuleInitContext) {
            ctx.define_type::<Tui>();
            ctx.define_subr_0(
                CStr::from_bytes_with_nul(b"--next-event\0").unwrap(),
                scm_next_event,
            );
            ctx.define_subr_1(
                CStr::from_bytes_with_nul(b"--new-tui\0").unwrap(),
                scm_new_tui,
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

    pub extern "C" fn scm_next_event() -> Scm {
        let e = match next_event() {
            Some(e) => e,
            None => return Scm::FALSE,
        };
        unsafe { event_to_scm(e) }.unwrap_or(Scm::FALSE)
    }

    extern "C" fn scm_new_tui(backend_type: Scm) -> Scm {
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
    }

    extern "C" fn scm_delete_tui(tui: Scm) -> Scm {
        unsafe { Tui::scm_drop(tui) };
        Scm::EOL
    }

    extern "C" fn scm_tui_size(tui: Scm) -> Scm {
        let tui = unsafe { Tui::from_scm(&tui) };
        let (width, height) = tui.size().scm_unwrap();
        unsafe {
            Scm::with_alist([
                (Scm::new_symbol("width"), Scm::new_u16(width)),
                (Scm::new_symbol("height"), Scm::new_u16(height)),
            ])
        }
    }

    extern "C" fn scm_tui_draw(tui: Scm, buffers: Scm) -> Scm {
        let mut tui = tui;
        let tui = unsafe { Tui::from_scm_mut(&mut tui) };
        tui.draw(unsafe {
            buffers
                .iter()
                .map(|scm_buffer| Buffer::ptr_from_scm(scm_buffer))
                .map(|ptr| &*ptr)
        })
        .scm_unwrap();
        Scm::EOL
    }

    extern "C" fn scm_tui_state_for_test(tui: Scm) -> Scm {
        let mut tui = tui;
        let tui = unsafe { Tui::from_scm_mut(&mut tui) };
        let state = tui.state().scm_unwrap();
        unsafe { Scm::new_string(&state) }
    }
}

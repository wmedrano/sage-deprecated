use std::{io::Stdout, time::Duration};

use anyhow::Result;
use crossterm::event;
use flashkick::{foreign_object::ForeignObjectType, Scm};
use ratatui::{
    prelude::{Constraint, CrosstermBackend, Direction, Layout},
    widgets::Paragraph,
    Terminal,
};

use crate::buffer::Buffer;

pub struct Tui {
    terminal: Terminal<CrosstermBackend<Stdout>>,
}

impl ForeignObjectType for Tui {
    const NAME: &'static str = "willy-tui";
}

impl Tui {
    pub fn new() -> Result<Tui> {
        let backend = CrosstermBackend::new(std::io::stdout());
        let mut terminal = Terminal::new(backend)?;
        crossterm::terminal::enable_raw_mode()?;
        crossterm::execute!(
            std::io::stdout(),
            crossterm::terminal::EnterAlternateScreen,
            crossterm::event::EnableMouseCapture
        )?;
        terminal.hide_cursor()?;
        Ok(Tui { terminal })
    }

    pub fn draw(&mut self, buffer: &Buffer, log: &Buffer) -> Result<()> {
        self.terminal.draw(|frame| {
            let layout = Layout::new(
                Direction::Vertical,
                [
                    Constraint::Percentage(49), // 0 - Buffer window.
                    Constraint::Percentage(49), // 1 - Log window.
                    Constraint::Length(1),      // 2 - Bottom status bar.
                ],
            )
            .split(frame.size());
            let log_text: Vec<_> = log.iter_lines().rev().collect();
            frame.render_widget(Paragraph::new(buffer.to_text() + "_"), layout[0]);
            frame.render_widget(Paragraph::new(log_text.join("\n")), layout[1]);
            frame.render_widget(Paragraph::new("Looking at a buffer in Willy!"), layout[2]);
        })?;
        Ok(())
    }
}

impl Drop for Tui {
    fn drop(&mut self) {
        let _ = crossterm::execute!(
            std::io::stdout(),
            crossterm::event::DisableMouseCapture,
            crossterm::terminal::LeaveAlternateScreen
        );
        let _ = crossterm::terminal::disable_raw_mode();
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

    use flashkick::{err::ResultToScm, foreign_object::ForeignObjectType, module::Module, Scm};

    use crate::buffer::Buffer;

    use super::{event_to_scm, next_event, Tui};

    /// Initialize the `(willy tui)` module.
    ///
    /// # Safety
    /// Calls unsafe code.
    #[no_mangle]
    pub unsafe extern "C" fn scm_init_willy_tui_module() {
        TuiModule.init();
    }

    struct TuiModule;

    impl Module for TuiModule {
        fn name() -> &'static std::ffi::CStr {
            CStr::from_bytes_with_nul(b"willy tui\0").unwrap()
        }

        unsafe fn define(&self, ctx: &mut flashkick::module::ModuleInitContext) {
            ctx.define_type::<Tui>();
            ctx.define_subr_0(
                CStr::from_bytes_with_nul(b"new-tui\0").unwrap(),
                scm_new_tui,
            );
            ctx.define_subr_1(
                CStr::from_bytes_with_nul(b"delete-tui\0").unwrap(),
                scm_delete_tui,
                1,
            );
            ctx.define_subr_0(
                CStr::from_bytes_with_nul(b"next-event\0").unwrap(),
                scm_next_event,
            );
            ctx.define_subr_3(CStr::from_bytes_with_nul(b"draw\0").unwrap(), scm_draw, 3);
        }
    }

    extern "C" fn scm_new_tui() -> Scm {
        let tui = Box::new(Tui::new().scm_unwrap());
        unsafe { Tui::to_scm(tui) }
    }

    extern "C" fn scm_delete_tui(tui: Scm) -> Scm {
        unsafe { Tui::scm_drop(tui) };
        Scm::EOL
    }

    extern "C" fn scm_draw(tui: Scm, buffer: Scm, log: Scm) -> Scm {
        let mut tui = tui;
        let tui = unsafe { Tui::from_scm_mut(&mut tui) };
        let buffer = unsafe { Buffer::from_scm(&buffer) };
        let log = unsafe { Buffer::from_scm(&log) };
        tui.draw(buffer, log).scm_unwrap();
        Scm::EOL
    }

    pub extern "C" fn scm_next_event() -> Scm {
        let e = match next_event() {
            Some(e) => e,
            None => return Scm::FALSE,
        };
        unsafe { event_to_scm(e) }.unwrap_or(Scm::FALSE)
    }
}

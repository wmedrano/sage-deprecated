use std::{
    ffi::CStr,
    fmt::Display,
    panic::catch_unwind,
    time::{Duration, Instant},
};

use anyhow::Result;
use flashkick::{
    err::{throw_error, ResultToScm},
    foreign_object::ForeignObjectType,
    module::ModuleInitContext,
    Scm,
};
use ratatui::{
    prelude::Rect,
    style::{Color, Stylize},
    widgets::Block,
    Frame, Terminal,
};

use crate::rope::{Rope, RopeFingerprint};

use self::{
    backend::{BackendType, TerminalBackend},
    widgets::LineWidget,
};

mod backend;
mod widgets;

/// Used to manage the terminal UI.
pub struct Tui {
    /// The terminal to render on to.
    terminal: Terminal<TerminalBackend>,
    /// The last time draw was called.
    frame_time: Instant,
    /// The state of the last draw. Used to determine if a redraw is necessary.
    previous_state: RopeFingerprint,
}

impl ForeignObjectType for Tui {
    const NAME: &'static str = "sage-tui";
}

impl Tui {
    /// Create a new `Tui`.
    pub fn new(backend_type: BackendType) -> Result<Tui> {
        let backend = TerminalBackend::new(backend_type)?;
        let terminal = Terminal::new(backend)?;
        Ok(Tui {
            terminal,
            frame_time: Instant::now(),
            previous_state: RopeFingerprint::default(),
        })
    }

    /// Draw the contents on the screen. This is limited to 60 calls per second.
    pub fn draw(&mut self, rope: &Rope) -> Result<()> {
        let new_state = rope.fingerprint();
        if self.previous_state != new_state {
            self.do_draw(rope)?;
            self.previous_state = new_state;
        }
        self.limit_frames();
        Ok(())
    }

    fn do_draw(&mut self, rope: &Rope) -> Result<()> {
        self.terminal.draw(|frame: &mut Frame| {
            let area = frame.size();
            frame.render_widget(Block::default().bg(Color::Black), area);
            for (y, line) in (area.y..area.bottom()).zip(rope.lines()) {
                frame.render_widget(
                    LineWidget::new(line.chunks()),
                    Rect::new(area.x, y, area.width, 1),
                )
            }
        })?;
        Ok(())
    }

    fn limit_frames(&mut self) {
        let target_frame_duration = Duration::from_nanos(1_000_000_000 / 60);
        let current_frame_time = Instant::now();
        let frame_duration = current_frame_time.duration_since(self.frame_time);
        self.frame_time = if frame_duration < target_frame_duration {
            std::thread::sleep(target_frame_duration - frame_duration);
            self.frame_time + target_frame_duration
        } else {
            current_frame_time
        };
    }
}

impl Display for Tui {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.terminal.backend().fmt(f)
    }
}

pub unsafe fn define_tui(ctx: &mut ModuleInitContext) {
    ctx.define_type::<Tui>();
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"make-tui\0").unwrap(),
        scm_make_tui,
        1,
    );
    ctx.define_subr_2(
        CStr::from_bytes_with_nul(b"tui-draw!\0").unwrap(),
        scm_tui_draw,
        2,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"delete-tui!\0").unwrap(),
        scm_delete_tui,
        1,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"tui->string\0").unwrap(),
        scm_tui_to_string,
        1,
    );
}

extern "C" fn scm_make_tui(backend: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = if backend.is_eq(Scm::new_symbol("test")) {
            Tui::new(BackendType::Test).scm_unwrap()
        } else if backend.is_eq(Scm::new_symbol("terminal")) {
            Tui::new(BackendType::Terminal).scm_unwrap()
        } else {
            throw_error("expected backend to be 'test or 'terminal.")
        };
        Tui::to_scm(Box::new(tui))
    })
    .map_err(|_| "Rust panic encountered on make-tui.")
    .scm_unwrap()
}

extern "C" fn scm_tui_draw(tui: Scm, rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        let rope = Rope::from_scm(rope).unwrap();
        tui.draw(&rope).scm_unwrap();
    })
    .map_err(|_| "Rust panic encountered on make-tui.")
    .scm_unwrap();
    tui
}

extern "C" fn scm_delete_tui(tui: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        let mut test_tui = Tui::new(BackendType::Test).scm_unwrap();
        std::mem::swap(&mut test_tui, tui);
    })
    .map_err(|_| "Rust panic encountered on make-tui.")
    .scm_unwrap();
    Scm::UNDEFINED
}

extern "C" fn scm_tui_to_string(tui: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        Scm::new_string(&tui.to_string())
    })
    .map_err(|_| "Rust panic encountered on make-tui.")
    .scm_unwrap()
}

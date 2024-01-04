use std::{ffi::CStr, fmt::Display, panic::catch_unwind, time::Duration};

use anyhow::{bail, Result};
use crossterm::event;
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

use self::backend::{BackendType, TerminalBackend};

mod backend;

pub struct Tui {
    terminal: Terminal<TerminalBackend>,
}

impl ForeignObjectType for Tui {
    const NAME: &'static str = "sage-tui";
}

impl Tui {
    pub fn new(backend_type: BackendType) -> Result<Tui> {
        let backend = TerminalBackend::new(backend_type)?;
        let terminal = Terminal::new(backend)?;
        Ok(Tui { terminal })
    }

    pub fn draw(&mut self) -> Result<()> {
        self.terminal.draw(|frame: &mut Frame| {
            frame.render_widget(Block::default().bg(Color::Black), frame.size());
        })?;
        Ok(())
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
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"tui-draw!\0").unwrap(),
        scm_tui_draw,
        1,
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

extern "C" fn scm_tui_draw(tui: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        tui.draw().scm_unwrap();
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

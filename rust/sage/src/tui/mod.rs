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
use ratatui::{prelude::Rect, Frame, Terminal};

use crate::{rope::Rope, scm_const};

use self::{
    backend::{BackendType, TerminalBackend},
    widgets::CursorPosition,
    window::Window,
};

mod backend;
mod widgets;
mod window;

/// Used to manage the terminal UI.
pub struct Tui {
    /// The terminal to render on to.
    terminal: Terminal<TerminalBackend>,
    /// The last time draw was called.
    frame_time: Instant,
    /// The size of the frame.
    frame_size: Rect,
}

impl ForeignObjectType for Tui {
    const NAME: &'static str = "sage-tui";
}

impl Tui {
    /// Create a new `Tui`.
    pub fn new(backend_type: BackendType) -> Result<Tui> {
        let backend = TerminalBackend::new(backend_type)?;
        let terminal = Terminal::new(backend)?;
        let frame_size = terminal.size()?;
        Ok(Tui {
            terminal,
            frame_time: Instant::now(),
            frame_size,
        })
    }

    /// Draw the contents on the screen. This is limited to 60 calls per second.
    pub fn draw<'a>(&mut self, windows: impl Iterator<Item = Window<'a>>) -> Result<Rect> {
        self.terminal.draw(|frame: &mut Frame| {
            let area = frame.size();
            self.frame_size = area;
            for window in windows {
                frame.render_widget(window, area);
            }
        })?;
        self.limit_frames();
        Ok(self.frame_size)
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

extern "C" fn scm_tui_draw(tui: Scm, windows: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        let windows: Vec<_> = windows
            .iter()
            .map(|window| scm_window_from_alist(window))
            .collect();
        let frame_size = tui.draw(windows.into_iter().rev()).scm_unwrap();
        scm_rect_to_alist(frame_size)
    })
    .map_err(|_| "Rust panic encountered on tui-draw.")
    .scm_unwrap()
}

extern "C" fn scm_delete_tui(tui: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        let mut test_tui = Tui::new(BackendType::Test).scm_unwrap();
        std::mem::swap(&mut test_tui, tui);
    })
    .map_err(|_| "Rust panic encountered on tui-delete.")
    .scm_unwrap();
    Scm::UNDEFINED
}

extern "C" fn scm_tui_to_string(tui: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        Scm::new_string(&tui.to_string())
    })
    .map_err(|_| "Rust panic encountered on tui->string.")
    .scm_unwrap()
}

unsafe fn scm_area_from_scm(rect: Scm) -> Rect {
    Rect::new(
        rect.struct_ref(scm_const::X).to_f64() as u16,
        rect.struct_ref(scm_const::Y).to_f64() as u16,
        rect.struct_ref(scm_const::WIDTH).to_f64() as u16,
        rect.struct_ref(scm_const::HEIGHT).to_f64() as u16,
    )
}

unsafe fn scm_rect_to_alist(rect: Rect) -> Scm {
    Scm::with_alist([
        (Scm::new_symbol("x"), Scm::new_u16(rect.x)),
        (Scm::new_symbol("y"), Scm::new_u16(rect.y)),
        (Scm::new_symbol("width"), Scm::new_u16(rect.width)),
        (Scm::new_symbol("height"), Scm::new_u16(rect.height)),
    ])
}

unsafe fn scm_window_from_alist<'a>(window: Scm) -> Window<'a> {
    {
        let scm_buffer = window.struct_ref(scm_const::BUFFER);
        let scm_cursor = scm_buffer.struct_ref(scm_const::CURSOR);
        let scm_rope = scm_buffer.struct_ref(scm_const::ROPE);
        let scm_area = window.struct_ref(scm_const::AREA);
        let scm_features = window.struct_ref(scm_const::FEATURES);
        let rope = Rope::from_scm_mut(scm_rope).unwrap();
        let area = scm_area_from_scm(scm_area);

        let mut border = false;
        let mut line_numbers = false;
        let cursor = if scm_cursor.is_number() {
            CursorPosition::Byte(scm_cursor.to_u32() as usize)
        } else {
            CursorPosition::None
        };
        let mut title = None;
        for (feature, value) in scm_features.iter_pairs() {
            match feature.to_symbol().as_str() {
                "border?" => border = value.to_bool(),
                "line-numbers?" => line_numbers = value.to_bool(),
                "title" => title = Some(value.to_string()),
                _ => (),
            }
        }

        rope.update_highlights();
        Window {
            rope,
            area,
            title,
            border,
            line_numbers,
            cursor,
        }
    }
}

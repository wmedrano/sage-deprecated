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
    widgets::{Block, BorderType, Borders, Clear},
    Frame, Terminal,
};

use crate::rope::Rope;

use self::{
    backend::{BackendType, TerminalBackend},
    widgets::SyntaxHighlightedText,
};

mod backend;
mod widgets;

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

pub struct Window<'a> {
    pub rope: &'a Rope,
    pub area: Rect,
    pub border: bool,
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
    pub fn draw<'a>(&mut self, windows: impl Iterator<Item = &'a Window<'a>>) -> Result<Rect> {
        self.do_draw(windows)?;
        self.limit_frames();
        Ok(self.frame_size)
    }

    fn do_draw<'a>(&mut self, windows: impl Iterator<Item = &'a Window<'a>>) -> Result<()> {
        self.terminal.draw(|frame: &mut Frame| {
            let frame_area = frame.size();
            self.frame_size = frame_area;
            for window in windows {
                // 1. Clear the area.
                let mut area = window.area.intersection(frame_area);
                frame.render_widget(Clear, area);
                // 2. Render the block and possibly a border.
                let mut block = Block::new().bg(Color::Reset);
                let block_area = area;
                if window.border {
                    block = block.borders(Borders::ALL).border_type(BorderType::Rounded);
                }
                area = block.inner(area);
                frame.render_widget(block, block_area);
                // 3. Render the text widget.
                let widget = SyntaxHighlightedText::new(window.rope);
                frame.render_widget(widget, self.frame_size.intersection(area));
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

extern "C" fn scm_tui_draw(tui: Scm, windows: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let tui = Tui::from_scm_mut(tui).unwrap();
        let windows: Vec<_> = windows
            .iter()
            .map(|window| {
                let mut rope = None;
                let mut area = Rect::new(0, 0, 0, 0);
                let mut border = false;
                for (key, value) in window.iter_pairs() {
                    match key.to_symbol().as_str() {
                        "rope" => rope = Some(Rope::from_scm_mut(value).unwrap()),
                        "position" => area = scm_rect_from_alist(value),
                        "features" => {
                            for (feature, value) in value.iter_pairs() {
                                match feature.to_symbol().as_str() {
                                    "border?" => border = value.to_bool(),
                                    _ => (),
                                }
                            }
                        }
                        _ => (),
                    }
                }
                rope.as_mut().unwrap().update_highlights();
                Window {
                    rope: rope.unwrap(),
                    area,
                    border,
                }
            })
            .collect();
        let frame_size = tui.draw(windows.iter().rev()).scm_unwrap();
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

unsafe fn scm_rect_from_alist(rect: Scm) -> Rect {
    let mut r = Rect::new(0, 0, 0, 0);
    for (key, value) in rect.iter_pairs() {
        match key.to_symbol().as_str() {
            "x" => r.x = value.to_f64() as u16,
            "y" => r.y = value.to_f64() as u16,
            "width" => r.width = value.to_f64() as u16,
            "height" => r.height = value.to_f64() as u16,
            _ => (),
        }
    }
    r
}

unsafe fn scm_rect_to_alist(rect: Rect) -> Scm {
    Scm::with_alist([
        (Scm::new_symbol("x"), Scm::new_u16(rect.x)),
        (Scm::new_symbol("y"), Scm::new_u16(rect.y)),
        (Scm::new_symbol("width"), Scm::new_u16(rect.width)),
        (Scm::new_symbol("height"), Scm::new_u16(rect.height)),
    ])
}

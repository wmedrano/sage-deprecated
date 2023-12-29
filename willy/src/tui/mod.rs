use std::time::Duration;

use anyhow::{bail, Result};
use crossterm::event;
use flashkick::foreign_object::ForeignObjectType;
use ratatui::{prelude::Rect, style::Stylize, widgets::Block, Frame, Terminal};

use self::{terminal_backends::TerminalBackend, theme::ONEDARK_THEME, widgets::BufferWidget};

pub mod terminal_backends;
pub mod theme;
pub mod widgets;

pub struct Tui {
    terminal: Terminal<TerminalBackend>,
}

impl ForeignObjectType for Tui {
    const NAME: &'static str = "willy-tui";
}

#[derive(Copy, Clone)]
pub enum BackendType {
    Terminal,
    Test,
}

impl Tui {
    /// Create a new terminal UI with the given backend.
    pub fn new(backend_type: BackendType) -> Result<Tui> {
        let backend = TerminalBackend::new(backend_type)?;
        let mut terminal = Terminal::new(backend)?;
        terminal.hide_cursor()?;
        Ok(Tui { terminal })
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
    pub fn draw<'a>(&mut self, widgets: impl Iterator<Item = Window<'a>>) -> Result<()> {
        self.terminal.draw(|frame: &mut Frame| {
            let window_area = frame.size();
            frame.render_widget(Block::default().bg(ONEDARK_THEME.black1), window_area);
            for widget in widgets {
                let area = window_area.intersection(widget.area);
                if area.width > 0 && area.height > 0 {
                    frame.render_widget(widget.widget, area);
                }
            }
        })?;
        Ok(())
    }
}

/// TODO: Remove indirection and use the real widgets directly.
pub struct Window<'a> {
    pub area: Rect,
    pub widget: BufferWidget<'a>,
}

pub fn next_event() -> Option<event::Event> {
    let is_ready =
        crossterm::event::poll(Duration::ZERO).expect("could not get next event in event loop");
    if !is_ready {
        return None;
    }
    let e = crossterm::event::read().expect("could not read next event in event loop");
    Some(e)
}

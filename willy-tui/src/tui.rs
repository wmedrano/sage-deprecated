use std::io::Stdout;

use anyhow::Result;
use flashkick::foreign_object::ForeignObjectType;
use ratatui::{prelude::CrosstermBackend, Terminal};

pub struct Tui {
    _terminal: Terminal<CrosstermBackend<Stdout>>,
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
        Ok(Tui {
            _terminal: terminal,
        })
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

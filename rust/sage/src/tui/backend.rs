use std::{
    fmt::{Display, Write},
    io::Stdout,
};

use anyhow::Result;
use ratatui::{
    backend::TestBackend,
    prelude::{Backend, CrosstermBackend, Rect},
};

#[derive(Copy, Clone)]
pub enum BackendType {
    Terminal,
    Test,
}

pub enum TerminalBackend {
    Default(CrosstermBackend<Stdout>),
    Test(TestBackend),
}

impl TerminalBackend {
    pub fn new(backend_type: BackendType) -> Result<TerminalBackend> {
        match backend_type {
            BackendType::Terminal => {
                let backend = CrosstermBackend::new(std::io::stdout());
                crossterm::execute!(std::io::stdout(), crossterm::terminal::EnterAlternateScreen)?;
                crossterm::terminal::enable_raw_mode()?;
                Ok(TerminalBackend::Default(backend))
            }
            BackendType::Test => {
                let backend = TestBackend::new(80, 24);
                Ok(TerminalBackend::Test(backend))
            }
        }
    }
}

impl Backend for TerminalBackend {
    fn draw<'a, I>(&mut self, content: I) -> std::io::Result<()>
    where
        I: Iterator<Item = (u16, u16, &'a ratatui::buffer::Cell)>,
    {
        match self {
            TerminalBackend::Default(b) => b.draw(content),
            TerminalBackend::Test(b) => b.draw(content),
        }
    }

    fn hide_cursor(&mut self) -> std::io::Result<()> {
        match self {
            TerminalBackend::Default(b) => b.hide_cursor(),
            TerminalBackend::Test(b) => b.hide_cursor(),
        }
    }

    fn show_cursor(&mut self) -> std::io::Result<()> {
        match self {
            TerminalBackend::Default(b) => b.show_cursor(),
            TerminalBackend::Test(b) => b.show_cursor(),
        }
    }

    fn get_cursor(&mut self) -> std::io::Result<(u16, u16)> {
        match self {
            TerminalBackend::Default(b) => b.get_cursor(),
            TerminalBackend::Test(b) => b.get_cursor(),
        }
    }

    fn set_cursor(&mut self, x: u16, y: u16) -> std::io::Result<()> {
        match self {
            TerminalBackend::Default(b) => b.set_cursor(x, y),
            TerminalBackend::Test(b) => b.set_cursor(x, y),
        }
    }

    fn clear(&mut self) -> std::io::Result<()> {
        match self {
            TerminalBackend::Default(b) => b.clear(),
            TerminalBackend::Test(b) => b.clear(),
        }
    }

    fn size(&self) -> std::io::Result<Rect> {
        match self {
            TerminalBackend::Default(b) => b.size(),
            TerminalBackend::Test(b) => b.size(),
        }
    }

    fn window_size(&mut self) -> std::io::Result<ratatui::backend::WindowSize> {
        match self {
            TerminalBackend::Default(b) => b.window_size(),
            TerminalBackend::Test(b) => b.window_size(),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            TerminalBackend::Default(b) => b.flush(),
            TerminalBackend::Test(b) => b.flush(),
        }
    }
}

impl Display for TerminalBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminalBackend::Default(_) => f.write_str("TERMINAL TUI\n")?,
            TerminalBackend::Test(b) => {
                let mut chars = b.buffer().content().iter().map(|c| c.symbol());
                let area = b.buffer().area;
                for row in 0..area.height {
                    if row > 0 {
                        f.write_char('\n')?;
                    }
                    for _ in 0..area.width {
                        f.write_str(chars.next().unwrap_or_default())?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl Drop for TerminalBackend {
    fn drop(&mut self) {
        match self {
            TerminalBackend::Default(_) => {
                let _ = crossterm::execute!(
                    std::io::stdout(),
                    crossterm::terminal::LeaveAlternateScreen
                );
                let _ = crossterm::terminal::disable_raw_mode();
            }
            TerminalBackend::Test(_) => {}
        }
    }
}

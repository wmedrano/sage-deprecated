use std::io::{stdout, Stdout};

use anyhow::{bail, Result};
use crossterm::{
    event,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::{prelude::CrosstermBackend, Terminal};

use crate::{buffer::Buffer, widgets::buffer::BufferWidget};

type CrossTerminal = Terminal<CrosstermBackend<Stdout>>;

/// Runs the `willy` application.
pub struct App {
    terminal: CrossTerminal,
    state: State,
}

/// Stores the state of the `willy` application.
pub struct State {
    buffer: Buffer,
    should_exit: bool,
}

impl App {
    /// Create a new `App`.
    pub fn new() -> Result<App> {
        stdout().execute(EnterAlternateScreen)?;
        enable_raw_mode()?;
        let mut terminal = Terminal::new(CrosstermBackend::new(stdout()))?;
        terminal.clear()?;
        Ok(App {
            terminal,
            state: State::default(),
        })
    }

    /// Render the current state of the `App`.
    pub fn render(&mut self) -> Result<()> {
        if self.state.should_exit {
            bail!("Exit requested.");
        }
        self.terminal.draw(|frame| {
            let area = frame.size();
            frame.render_widget(BufferWidget::new(&self.state.buffer), area);
        })?;
        Ok(())
    }

    /// Handle all events and update the `App` state accordingly.
    pub fn handle_events(&mut self) -> Result<()> {
        while event::poll(std::time::Duration::from_millis(16))? {
            let raw_event = event::read()?;
            match raw_event {
                event::Event::Key(event::KeyEvent {
                    code: event::KeyCode::Char('c'),
                    modifiers: event::KeyModifiers::CONTROL,
                    kind: event::KeyEventKind::Press,
                    ..
                }) => self.state.should_exit = true,
                event::Event::Key(event::KeyEvent {
                    code: event::KeyCode::Enter,
                    modifiers: event::KeyModifiers::NONE,
                    kind: event::KeyEventKind::Press,
                    ..
                }) => self.state.buffer.push_line(""),
                event::Event::Key(event::KeyEvent {
                    code: event::KeyCode::Backspace,
                    modifiers: event::KeyModifiers::NONE,
                    kind: event::KeyEventKind::Press,
                    ..
                }) => {
                    self.state.buffer.pop_char();
                }
                event::Event::Key(event::KeyEvent {
                    code: event::KeyCode::Char(ch),
                    modifiers: event::KeyModifiers::NONE,
                    kind: event::KeyEventKind::Press,
                    ..
                }) => self.state.buffer.push_char(ch),
                event::Event::Key(event::KeyEvent {
                    code: event::KeyCode::Char(ch),
                    modifiers: event::KeyModifiers::SHIFT,
                    kind: event::KeyEventKind::Press,
                    ..
                }) => self.state.buffer.push_chars(ch.to_uppercase()),
                _ => (),
            }
        }
        Ok(())
    }
}

impl Drop for App {
    fn drop(&mut self) {
        let _ = stdout().execute(LeaveAlternateScreen);
        let _ = disable_raw_mode();
    }
}

impl Default for State {
    fn default() -> State {
        State {
            buffer: Buffer::new_scratch(),
            should_exit: false,
        }
    }
}

use anyhow::{bail, Result};
use crossterm::event;
use ratatui::{prelude::Backend, Terminal};

use crate::{buffer::Buffer, widgets::buffer::BufferWidget};

/// Runs the `willy` application.
pub struct App<B: Backend> {
    terminal: Terminal<B>,
    state: State,
}

/// Stores the state of the `willy` application.
pub struct State {
    buffer: Buffer,
    should_exit: bool,
}

impl<B: Backend> App<B> {
    /// Create a new `App`.
    pub fn new(backend: B) -> Result<App<B>> {
        let mut terminal = Terminal::new(backend)?;
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
    pub fn handle_events(
        &mut self,
        events: impl Iterator<Item = Result<event::Event>>,
    ) -> Result<()> {
        for event_or_err in events {
            self.handle_event(event_or_err?);
        }
        Ok(())
    }

    fn handle_event(&mut self, e: event::Event) {
        match e {
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
}

impl<B: Backend> Drop for App<B> {
    fn drop(&mut self) {}
}

impl Default for State {
    fn default() -> State {
        State {
            buffer: Buffer::new_scratch(),
            should_exit: false,
        }
    }
}

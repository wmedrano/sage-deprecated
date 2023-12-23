use anyhow::Result;
use crossterm::event;
use ratatui::{prelude::Backend, Terminal};

use crate::{buffer::Buffer, widgets::buffer::BufferWidget};

/// What the current control state is.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum AppControlState {
    /// The app should continue running.
    Continue,
    /// The app should exit.
    Exit,
}

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

#[cfg(test)]
impl App<ratatui::backend::TestBackend> {
    /// Create a new app with a dummy backend.
    pub fn new_dummy(width: u16, height: u16) -> Result<App<ratatui::backend::TestBackend>> {
        Self::new(ratatui::backend::TestBackend::new(width, height))
    }

    /// Get the underlying buffer for the test backend.
    pub fn buffer(&self) -> &ratatui::prelude::Buffer {
        self.terminal.backend().buffer()
    }
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
        self.terminal.draw(|frame| {
            let area = frame.size();
            frame.render_widget(BufferWidget::new(&self.state.buffer), area);
        })?;
        Ok(())
    }

    /// Handle all events and update the `App` state accordingly.
    pub fn handle_events(
        &mut self,
        events: impl IntoIterator<Item = Result<event::Event>>,
    ) -> Result<AppControlState> {
        for event_or_err in events.into_iter() {
            self.handle_event(event_or_err?);
        }
        Ok(if self.state.should_exit {
            AppControlState::Exit
        } else {
            AppControlState::Continue
        })
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
            }) => self.state.buffer.push_char('\n'),
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

impl Default for State {
    fn default() -> State {
        State {
            buffer: Buffer::new_scratch(),
            should_exit: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use ratatui::assert_buffer_eq;

    fn make_key_press(ch: char) -> event::Event {
        assert!(ch.is_ascii()); // Only ascii us supported for testing.
        event::Event::Key(event::KeyEvent {
            code: event::KeyCode::Char(ch.to_ascii_lowercase()),
            modifiers: if ch.is_lowercase() {
                event::KeyModifiers::NONE
            } else {
                event::KeyModifiers::SHIFT
            },
            kind: event::KeyEventKind::Press,
            state: event::KeyEventState::empty(),
        })
    }

    use super::*;

    #[test]
    fn ctr_c_exits_app() {
        let mut app = App::new_dummy(80, 20).unwrap();

        let res = app
            .handle_events(std::iter::once(Ok(make_key_press('c'))))
            .unwrap();
        assert_eq!(res, AppControlState::Continue);

        let res = app
            .handle_events(std::iter::once(Ok(event::Event::Key(event::KeyEvent {
                code: event::KeyCode::Char('c'),
                modifiers: event::KeyModifiers::CONTROL,
                kind: event::KeyEventKind::Press,
                state: event::KeyEventState::empty(),
            }))))
            .unwrap();
        assert_eq!(res, AppControlState::Exit);
    }

    #[test]
    fn default_state_has_friendly_message() {
        let mut app = App::new_dummy(40, 4).unwrap();
        app.render().unwrap();
        assert_buffer_eq!(
            app.buffer(),
            &ratatui::prelude::Buffer::with_lines(vec![
                ";; Welcome to Willy!                    ",
                ";; A Scheme configured text editor.     ",
                "_                                       ",
                "                                        ",
            ])
        );
    }

    #[test]
    fn key_press_inserts_text() {
        let mut app = App::new_dummy(40, 4).unwrap();
        app.handle_events([
            Ok(make_key_press('H')),
            Ok(make_key_press('e')),
            Ok(make_key_press('l')),
            Ok(make_key_press('l')),
            Ok(make_key_press('o')),
            Ok(event::Event::Key(event::KeyEvent {
                code: event::KeyCode::Enter,
                modifiers: event::KeyModifiers::NONE,
                kind: event::KeyEventKind::Press,
                state: event::KeyEventState::empty(),
            })),
            Ok(make_key_press('W')),
            Ok(make_key_press('o')),
            Ok(make_key_press('r')),
            Ok(make_key_press('l')),
            Ok(make_key_press('d')),
            Ok(make_key_press('!')),
        ])
        .unwrap();
        app.render().unwrap();
        assert_buffer_eq!(
            app.buffer(),
            &ratatui::prelude::Buffer::with_lines(vec![
                ";; Welcome to Willy!                    ",
                ";; A Scheme configured text editor.     ",
                "Hello                                   ",
                "World!_                                 ",
            ])
        );
    }

    #[test]
    fn backspace_deletes_text() {
        let mut app = App::new_dummy(40, 4).unwrap();
        app.handle_events(
            std::iter::repeat_with(|| {
                Ok(event::Event::Key(event::KeyEvent {
                    code: event::KeyCode::Backspace,
                    modifiers: event::KeyModifiers::NONE,
                    kind: event::KeyEventKind::Press,
                    state: event::KeyEventState::empty(),
                }))
            })
            .take(3),
        )
        .unwrap();
        app.render().unwrap();
        assert_buffer_eq!(
            app.buffer(),
            &ratatui::prelude::Buffer::with_lines(vec![
                ";; Welcome to Willy!                    ",
                ";; A Scheme configured text edito_      ",
                "                                        ",
                "                                        ",
            ])
        );
    }
}

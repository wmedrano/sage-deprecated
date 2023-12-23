use std::{
    io::{stdout, Stdout},
    time::{Duration, Instant},
};

use anyhow::{anyhow, Result};
use app::App;
use crossterm::{
    event,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use ratatui::prelude::CrosstermBackend;

pub mod app;
pub mod buffer;
pub mod widgets;

fn main() -> Result<()> {
    // Setup
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;

    // Run
    let result = run_app(CrosstermBackend::new(stdout()));

    // Cleanup
    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    result
}

fn run_app(terminal: CrosstermBackend<Stdout>) -> Result<()> {
    let mut willy = App::new(terminal)?;
    loop {
        willy.render()?;
        willy.handle_events(iter_crossterm_events())?;
    }
}

/// Iterate through all crossterm events in the queue.
fn iter_crossterm_events() -> impl Iterator<Item = Result<event::Event>> {
    let timeout = Duration::from_millis(16);
    let deadline = Instant::now() + timeout;
    std::iter::from_fn(move || {
        let timeout = deadline.duration_since(Instant::now());
        match event::poll(timeout) {
            Ok(true) => match event::read() {
                Ok(e) => Some(Ok(e)),
                Err(err) => Some(Err(anyhow!(err))),
            },
            Ok(false) => None,
            Err(err) => Some(Err(anyhow!(err))),
        }
    })
}

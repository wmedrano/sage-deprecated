use std::io::{stdout, Stdout};

use anyhow::Result;
use app::App;
use crossterm::{
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
    ExecutableCommand,
};
use flashkick::Scm;
use ratatui::prelude::CrosstermBackend;
use terminal_backend::{event_to_scm, iter_crossterm_events};

mod app;
mod buffer;
mod scm_module;
mod terminal_backend;
mod widgets;

extern "C" fn scm_run_willy(event_handler: flashkick::Scm) -> flashkick::Scm {
    match run_willy(event_handler) {
        Ok(()) => flashkick::Scm::EOL,
        Err(err) => unsafe {
            let err_sym = Scm::new_symbol("willy-error");
            let msg = Scm::new_string(&err.to_string());
            let args = Scm::with_list(std::iter::once(msg));
            flashkick::ffi::scm_throw(err_sym.0, args.0);
        },
    }
}

fn run_willy(event_handler: flashkick::Scm) -> Result<()> {
    // Setup
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;

    // Run
    let result = run_willy_with_terminal(event_handler, CrosstermBackend::new(stdout()));

    // Cleanup
    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    result
}

fn run_willy_with_terminal(
    event_handler: flashkick::Scm,
    terminal: CrosstermBackend<Stdout>,
) -> Result<()> {
    let mut willy = App::new(terminal)?;
    loop {
        willy.render()?;
        let events = iter_crossterm_events().inspect(|e| unsafe {
            if let Ok(e) = e {
                let scm_event = event_to_scm(e);
                flashkick::ffi::scm_call_1(event_handler.0, scm_event.0);
            }
        });
        match willy.handle_events(events)? {
            app::AppControlState::Continue => (),
            app::AppControlState::Exit => return Ok(()),
        };
    }
}

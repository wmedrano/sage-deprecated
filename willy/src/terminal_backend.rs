use std::time::{Duration, Instant};

use anyhow::{anyhow, Result};
use crossterm::event;
use flashkick::Scm;

/// Iterate through all crossterm events in the queue.
pub fn iter_crossterm_events() -> impl Iterator<Item = Result<event::Event>> {
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

/// Convert an event into a Scheme object.
pub unsafe fn event_to_scm(e: &event::Event) -> Scm {
    match e {
        event::Event::Key(event::KeyEvent {
            code: event::KeyCode::Char(ch),
            kind: event::KeyEventKind::Press,
            ..
        }) => Scm::with_alist(
            [
                (Scm::new_keyword("char"), Scm::new_string(&ch.to_string())),
                (Scm::new_keyword("event-type"), Scm::new_symbol("key-press")),
            ]
            .into_iter(),
        ),
        _ => Scm::with_alist(std::iter::empty()),
    }
}

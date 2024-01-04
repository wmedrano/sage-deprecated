use std::{ffi::CStr, panic::catch_unwind, time::Duration};

use anyhow::anyhow;
use crossterm::event;
use flashkick::{err::ResultToScm, module::ModuleInitContext, Scm};

pub fn events_from_terminal() -> impl Iterator<Item = event::Event> {
    std::iter::from_fn(move || match event::poll(Duration::ZERO) {
        Err(err) => Err(anyhow!("failed to poll events from terminal: {err}")).unwrap(),
        Ok(false) => None,
        Ok(true) => Some(event::read().unwrap()),
    })
}

pub unsafe fn define_event(ctx: &mut ModuleInitContext) {
    ctx.define_subr_0(
        CStr::from_bytes_with_nul(b"events-from-terminal\0").unwrap(),
        scm_events_from_terminal,
    );
}

unsafe fn to_scm_event(e: event::Event) -> Option<Scm> {
    match e {
        event::Event::Key(event::KeyEvent {
            code,
            modifiers,
            kind,
            ..
        }) => {
            let key_code = match code {
                event::KeyCode::Char(ch) => Scm::new_char(ch),
                event::KeyCode::Enter => Scm::new_char('\n'),
                event::KeyCode::Tab => Scm::new_char('\t'),
                event::KeyCode::Backspace => Scm::new_string("<backspace>"),
                _ => return None,
            };
            let ctrl = Scm::new_bool(modifiers.contains(event::KeyModifiers::CONTROL));
            let alt = Scm::new_bool(modifiers.contains(event::KeyModifiers::ALT));
            let kind = match kind {
                // TODO: Consider caching symbols to improve performance.
                event::KeyEventKind::Press => Scm::new_symbol("press"),
                event::KeyEventKind::Repeat => Scm::new_symbol("repeat"),
                event::KeyEventKind::Release => Scm::new_symbol("release"),
            };
            Some(Scm::with_alist([
                (Scm::new_symbol("kind"), kind),
                (Scm::new_symbol("key-code"), key_code),
                (Scm::new_symbol("ctrl?"), ctrl),
                (Scm::new_symbol("alt?"), alt),
            ]))
        }
        _ => None,
    }
}

extern "C" fn scm_events_from_terminal() -> Scm {
    catch_unwind(|| unsafe {
        let events = events_from_terminal().filter_map(|e| to_scm_event(e));
        Scm::with_list(events)
    })
    .map_err(|_| "Rust panic encountered on make-tui.")
    .scm_unwrap()
}

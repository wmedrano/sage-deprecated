use std::{
    ffi::{c_void, CStr},
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
use flashkick::Scm;
use ratatui::prelude::CrosstermBackend;

pub mod app;
pub mod buffer;
pub mod widgets;

fn main() -> Result<()> {
    let mut args: Vec<*const i8> = vec![
        CStr::from_bytes_with_nul(b"-l\0").unwrap().as_ptr(),
        CStr::from_bytes_with_nul(b"scheme/main.scm\0")
            .unwrap()
            .as_ptr(),
    ];
    unsafe {
        flashkick::ffi::scm_boot_guile(
            args.len() as i32,
            args.as_mut_ptr() as *mut *mut i8,
            Some(inner_main),
            std::ptr::null_mut(),
        );
    }
    Ok(())
}

pub extern "C" fn inner_main(_: *mut c_void, argc: i32, argv: *mut *mut i8) {
    unsafe {
        flashkick::ffi::scm_c_define_gsubr(
            CStr::from_bytes_with_nul(b"run-willy\0").unwrap().as_ptr(),
            0,
            0,
            0,
            scm_run_willy as _,
        );
        flashkick::ffi::scm_shell(argc, argv);
    }
}

extern "C" fn scm_run_willy() -> flashkick::Scm {
    match run_willy() {
        Ok(()) => flashkick::Scm::EOL,
        Err(err) => unsafe {
            let err_sym = Scm::new_symbol("willy-error");
            let msg = Scm::new_string(&err.to_string());
            let args = Scm::with_reversed_list(std::iter::once(msg));
            flashkick::ffi::scm_throw(err_sym.0, args.0);
        },
    }
}

fn run_willy() -> Result<()> {
    // Setup
    stdout().execute(EnterAlternateScreen)?;
    enable_raw_mode()?;

    // Run
    let result = run_willy_with_terminal(CrosstermBackend::new(stdout()));

    // Cleanup
    stdout().execute(LeaveAlternateScreen)?;
    disable_raw_mode()?;
    result
}

fn run_willy_with_terminal(terminal: CrosstermBackend<Stdout>) -> Result<()> {
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

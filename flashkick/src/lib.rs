#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(improper_ctypes)] // Consider leaving this one on.

use fn_runner::{fn_runner_run, fn_runner_run_ignore_args, CArgs, FnRunner};
pub use scm::Scm;

pub mod err;
pub mod ffi;
mod fn_runner;
pub mod foreign_object;
pub mod module;
mod scm;

/// Run the Scheme shell.
///
/// # Safety
/// Calls C code.
pub unsafe fn shell(args: impl Iterator<Item = String>) {
    let cargs = CArgs::new(args);
    ffi::scm_shell(cargs.argc(), cargs.argv());
}

/// Boot up guile.
///
/// ```
/// unsafe {
///     flashkick::boot_guile(std::env::args(), || {
///         flashkick::shell(std::env::args());
///     })
/// };
/// ```
///
/// # Safety
/// Calls C code.
pub unsafe fn boot_guile<A: Iterator<Item = String>, F: FnOnce() -> T, T>(args: A, f: F) -> T {
    let cargs = CArgs::new(args);
    let mut runner = FnRunner::new(f);
    ffi::scm_boot_guile(
        cargs.argc(),
        cargs.argv(),
        Some(fn_runner_run_ignore_args::<F, T>),
        runner.as_mut_ptr(),
    );
    runner.take()
}

/// Runs the function in guile mode and returns the result.
///
/// # Safety
/// Calls C code.
pub unsafe fn with_guile<F: FnOnce() -> T, T>(f: F) -> T {
    let mut runner = FnRunner::new(f);
    ffi::scm_with_guile(Some(fn_runner_run::<F, T>), runner.as_mut_ptr());
    runner.take()
}

/// Runs the function without guile mode and returns the result.
///
/// # Safety
/// Calls C code.
pub unsafe fn without_guile<F: FnOnce() -> T, T>(f: F) -> T {
    let mut runner = FnRunner::new(f);
    ffi::scm_without_guile(Some(fn_runner_run::<F, T>), runner.as_mut_ptr());
    runner.take()
}

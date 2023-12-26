#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(improper_ctypes)] // Consider leaving this one on.

use fn_runner::{fn_runner_run, FnRunner};
pub use scm::Scm;

pub mod err;
pub mod ffi;
mod fn_runner;
pub mod foreign_object;
pub mod module;
mod scm;

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

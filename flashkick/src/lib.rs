#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(improper_ctypes)] // Consider leaving this one on.

// TODO: Consider using a sys crate for ffi. One could be made at the moment, but it is preferable
// to find an existing well maintained one.
pub mod ffi;

pub mod err;
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

struct FnRunner<F: FnOnce() -> T, T> {
    f: Option<F>,
    t: Option<T>,
}

impl<F: FnOnce() -> T, T> FnRunner<F, T> {
    fn new(f: F) -> Self {
        FnRunner {
            f: Some(f),
            t: None,
        }
    }

    fn as_mut_ptr(&mut self) -> *mut c_void {
        let ptr: *mut FnRunner<F, T> = self;
        ptr.cast()
    }

    fn run(&mut self) {
        if let Some(f) = self.f.take() {
            self.t = Some((f)());
        }
    }

    unsafe fn take(self) -> T {
        match self.t {
            Some(t) => t,
            None => throw_error("function exited, most likely due to throw or rust panic"),
        }
    }
}

extern "C" fn fn_runner_run<F: FnOnce() -> T, T>(data: *mut c_void) -> *mut c_void {
    let ptr: *mut FnRunner<F, T> = data.cast();
    let runner = unsafe { &mut *ptr };
    runner.run();
    std::ptr::null_mut()
}

use std::ffi::c_void;

use err::throw_error;
pub use scm::Scm;

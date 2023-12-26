use std::ffi::c_void;

use crate::err::throw_error;

pub(crate) struct FnRunner<F: FnOnce() -> T, T> {
    f: Option<F>,
    t: Option<T>,
}

impl<F: FnOnce() -> T, T> FnRunner<F, T> {
    pub fn new(f: F) -> Self {
        FnRunner {
            f: Some(f),
            t: None,
        }
    }

    pub fn as_mut_ptr(&mut self) -> *mut c_void {
        let ptr: *mut FnRunner<F, T> = self;
        ptr.cast()
    }

    fn run(&mut self) {
        if let Some(f) = self.f.take() {
            self.t = Some((f)());
        }
    }

    pub unsafe fn take(self) -> T {
        match self.t {
            Some(t) => t,
            None => throw_error("function exited, most likely due to throw or rust panic"),
        }
    }
}

pub(crate) extern "C" fn fn_runner_run<F: FnOnce() -> T, T>(data: *mut c_void) -> *mut c_void {
    let ptr: *mut FnRunner<F, T> = data.cast();
    let runner = unsafe { &mut *ptr };
    runner.run();
    std::ptr::null_mut()
}

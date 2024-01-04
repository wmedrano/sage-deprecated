use std::ffi::{c_void, CString};

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
    fn_runner_run_ignore_args::<F, T>(data, 0, std::ptr::null_mut());
    std::ptr::null_mut()
}

pub(crate) extern "C" fn fn_runner_run_ignore_args<F: FnOnce() -> T, T>(
    data: *mut c_void,
    _: i32,
    _: *mut *mut i8,
) {
    let ptr: *mut FnRunner<F, T> = data.cast();
    let runner = unsafe { &mut *ptr };
    runner.run();
}

pub struct CArgs {
    raw_args: Vec<CString>,
    arg_ptr: Vec<*mut i8>,
}

impl CArgs {
    pub fn new(args: impl Iterator<Item = String>) -> CArgs {
        let raw_args: Vec<_> = args.map(|a| CString::new(a).unwrap()).collect();
        let arg_ptr = raw_args.iter().map(|a| a.as_ptr() as *mut _).collect();
        CArgs { raw_args, arg_ptr }
    }

    pub fn argc(&self) -> i32 {
        self.raw_args.len() as i32
    }

    pub fn argv(&self) -> *mut *mut i8 {
        let ptr: *const *mut i8 = self.arg_ptr.as_ptr();
        ptr as *mut *mut i8
    }
}

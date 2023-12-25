use std::ffi::CStr;

use crate::scm_run_willy;

#[no_mangle]
pub extern "C" fn scm_init_willy_module() {
    unsafe { flashkick::module::init(WillyModule) };
}

/// The main module for willy.
pub struct WillyModule;

impl flashkick::module::Module for WillyModule {
    fn name() -> &'static CStr {
        CStr::from_bytes_with_nul(b"willy\0").unwrap()
    }

    unsafe fn init(&self, ctx: &mut flashkick::module::ModuleInitContext) {
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"run-willy\0").unwrap(),
            scm_run_willy,
        );
    }
}

use std::ffi::CStr;

use flashkick::module::{Module, ModuleInitContext};

mod event;
pub mod rope;
mod tui;

pub struct SageModule;

impl Module for SageModule {
    fn name() -> &'static CStr {
        CStr::from_bytes_with_nul(b"sage core internal\0").unwrap()
    }

    unsafe fn define(&self, ctx: &mut ModuleInitContext) {
        rope::define_rope(ctx);
        tui::define_tui(ctx);
        event::define_event(ctx);
    }
}

#[no_mangle]
extern "C" fn sage_core_internal_init() {
    unsafe { SageModule.init() };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scm_init() {
        unsafe {
            flashkick::boot_guile(std::iter::empty(), || {
                sage_core_internal_init();
            })
        };
    }
}

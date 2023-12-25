use std::ffi::CStr;

use flashkick::{err::ResultToScm, foreign_object::ForeignObjectType, module::Module, Scm};
use tui::Tui;

mod tui;

#[no_mangle]
pub unsafe extern "C" fn scm_init_willy_tui_module() {
    TuiModule.init();
}

struct TuiModule;

impl Module for TuiModule {
    fn name() -> &'static std::ffi::CStr {
        CStr::from_bytes_with_nul(b"willy tui\0").unwrap()
    }

    unsafe fn define(&self, ctx: &mut flashkick::module::ModuleInitContext) {
        ctx.define_type::<Tui>();
        ctx.define_subr_0(
            CStr::from_bytes_with_nul(b"new-tui\0").unwrap(),
            scm_new_tui,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"delete-tui\0").unwrap(),
            scm_delete_tui,
            1,
        );
    }
}

extern "C" fn scm_new_tui() -> Scm {
    let tui = Box::new(Tui::new().scm_unwrap());
    unsafe { Tui::to_scm(tui) }
}

extern "C" fn scm_delete_tui(tui: Scm) -> Scm {
    unsafe { Tui::scm_drop(tui) };
    Scm::EOL
}

use std::panic::catch_unwind;

use flashkick::{err::ResultToScm, module::Module};
use scm_module::WillyCoreInternalModule;

mod buffer_content;
mod frame_limiter;
mod scm_module;
mod scm_object_cache;
mod tui;

/// # Safety
/// Makes call to unsafe code.
#[no_mangle]
pub unsafe extern "C" fn scm_init_willy_core_internal_module() {
    catch_unwind(|| {
        WillyCoreInternalModule.init();
    })
    .map_err(|err| format!("{err:?}"))
    .scm_unwrap();
}

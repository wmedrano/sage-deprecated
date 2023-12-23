use std::ffi::{c_void, CStr};

use crate::{ffi, Scm};

/// Initialize a new module.
///
/// # Safety
/// Makes calls to C.
pub unsafe fn init<T: Module>(module: T) {
    unsafe {
        let module_ptr: *const T = &module;
        let name = T::name();
        ffi::scm_c_define_module(name.as_ptr(), Some(init_module_impl::<T>), module_ptr as _);
    }
}

/// Defines a scheme module. Must be registered with `init_module`.
pub trait Module {
    /// The name of the module. For example: "my-module submodule".
    fn name() -> &'static CStr;

    /// Initialize the module. `ctx` can be used to register subroutines within the module.
    ///
    /// # Safety
    /// Usually makes calls to C through `ctx` object.
    unsafe fn init(&self, ctx: &mut ModuleInitContext);
}

/// Context for initializing a module.
pub struct ModuleInitContext {
    _unused: (),
}

impl ModuleInitContext {
    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_0(&mut self, name: &CStr, subr: extern "C" fn(Scm) -> Scm) {
        ffi::scm_c_define_gsubr(name.as_ptr(), 0, 0, 0, subr as _);
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }

    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_1(&mut self, name: &CStr, subr: extern "C" fn(Scm) -> Scm) {
        ffi::scm_c_define_gsubr(name.as_ptr(), 1, 0, 0, subr as _);
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }

    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_2(&mut self, name: &CStr, subr: extern "C" fn(Scm, Scm) -> Scm) {
        ffi::scm_c_define_gsubr(name.as_ptr(), 2, 0, 0, subr as _);
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }
}

unsafe extern "C" fn init_module_impl<T: Module>(data: *mut c_void) {
    let self_ptr = data as *const T;
    let self_obj = &*self_ptr;
    self_obj.init(&mut ModuleInitContext { _unused: () });
}

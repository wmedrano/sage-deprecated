use std::ffi::{c_void, CStr};

use crate::{ffi, foreign_object::ForeignObjectType, Scm};

/// Defines a scheme module. Must be registered with `init_module`.
pub trait Module: Sized {
    /// The name of the module. For example: "my-module submodule".
    fn name() -> &'static CStr;

    /// Initialize the module. `ctx` can be used to register subroutines within the module.
    ///
    /// # Safety
    /// Usually makes calls to C through `ctx` object.
    unsafe fn define(&self, ctx: &mut ModuleInitContext);

    /// # Safety
    /// Makes calls to C.
    unsafe fn init(&self) {
        let module_ptr: *const Self = self;
        let name = Self::name();
        ffi::scm_c_define_module(
            name.as_ptr(),
            Some(init_module_impl::<Self>),
            module_ptr as _,
        );
    }
}

/// Context for initializing a module.
pub struct ModuleInitContext {
    _unused: (),
}

impl ModuleInitContext {
    /// Define a foreign type.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_type<T: ForeignObjectType>(&mut self) {
        T::init()
    }

    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_0(&mut self, name: &CStr, subr: extern "C" fn() -> Scm) {
        ffi::scm_c_define_gsubr(name.as_ptr(), 0, 0, 0, subr as _);
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }

    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_1(
        &mut self,
        name: &CStr,
        subr: extern "C" fn(Scm) -> Scm,
        required_params: usize,
    ) {
        let optional_params = 1 - required_params;
        ffi::scm_c_define_gsubr(
            name.as_ptr(),
            required_params as _,
            optional_params as _,
            0,
            subr as _,
        );
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }

    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_2(
        &mut self,
        name: &CStr,
        subr: extern "C" fn(Scm, Scm) -> Scm,
        required_params: usize,
    ) {
        let optional_params = 2 - required_params;
        ffi::scm_c_define_gsubr(
            name.as_ptr(),
            required_params as _,
            optional_params as _,
            0,
            subr as _,
        );
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }

    /// Define a subroutine in the module.
    ///
    /// # Safety
    /// Makes calls to C.
    pub unsafe fn define_subr_3(
        &mut self,
        name: &CStr,
        subr: extern "C" fn(Scm, Scm, Scm) -> Scm,
        required_params: usize,
    ) {
        let optional_params = 3 - required_params;
        ffi::scm_c_define_gsubr(
            name.as_ptr(),
            required_params as _,
            optional_params as _,
            0,
            subr as _,
        );
        ffi::scm_c_export(name.as_ptr(), std::ptr::null_mut::<c_void>());
    }
}

unsafe extern "C" fn init_module_impl<T: Module>(data: *mut c_void) {
    let self_ptr = data as *const T;
    let self_obj = &*self_ptr;
    self_obj.define(&mut ModuleInitContext { _unused: () });
}

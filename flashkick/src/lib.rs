#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![allow(improper_ctypes)] // Consider leaving this one on.

// TODO: Consider using a sys crate for ffi. One could be made at the moment, but it is preferable
// to find an existing well maintained one.
pub mod ffi;

pub mod module;
mod scm_obj;

pub use scm_obj::Scm;

/// Throw a scheme error.
///
/// # Safety
/// Makes calls to C.
pub unsafe fn throw<I, IT>(key: Scm, args: I)
where
    I: IntoIterator<Item = Scm, IntoIter = IT>,
    IT: DoubleEndedIterator + Iterator<Item = Scm>,
{
    let scm_args = Scm::with_list(args);
    crate::ffi::scm_throw(key.0, scm_args.0)
}

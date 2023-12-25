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

pub use scm::Scm;

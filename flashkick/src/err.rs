use crate::Scm;

/// Surface Rust errors in Scheme.
pub trait ResultToScm {
    type T;

    /// Returns `T` if `self` is `Ok`. If not, then an error is thrown in Scheme.
    fn scm_unwrap(self) -> Self::T;
}

impl<T, E: std::fmt::Display> ResultToScm for Result<T, E> {
    type T = T;

    fn scm_unwrap(self) -> T {
        match self {
            Ok(t) => t,
            Err(err) => unsafe { throw_error(err) },
        }
    }
}

unsafe fn throw_error(err: impl std::fmt::Display) -> ! {
    let key = Scm::new_symbol("rust-error");
    let scm_args = Scm::with_list(std::iter::once(Scm::new_string(&err.to_string())));
    crate::ffi::scm_throw(key.0, scm_args.0)
}

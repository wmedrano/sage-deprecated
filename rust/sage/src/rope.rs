use std::{
    ffi::CStr,
    fmt::Display,
    ops::{Deref, DerefMut},
    panic::catch_unwind,
};

use flashkick::{
    err::ResultToScm, foreign_object::ForeignObjectType, module::ModuleInitContext, Scm,
};

#[repr(transparent)]
pub struct RopeWrapper(crop::Rope);

impl ForeignObjectType for RopeWrapper {
    const NAME: &'static str = "sage-rope";
}

impl RopeWrapper {
    /// Create a new rope.
    pub fn new() -> RopeWrapper {
        RopeWrapper(crop::Rope::new())
    }
}

impl Deref for RopeWrapper {
    type Target = crop::Rope;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for RopeWrapper {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Display for RopeWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for chunk in self.0.chunks() {
            f.write_str(chunk)?;
        }
        Ok(())
    }
}

pub unsafe fn define_rope(ctx: &mut ModuleInitContext) {
    ctx.define_type::<RopeWrapper>();
    ctx.define_subr_0(
        CStr::from_bytes_with_nul(b"make-rope\0").unwrap(),
        scm_make_rope,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"rope->string\0").unwrap(),
        scm_rope_to_string,
        1,
    );
    ctx.define_subr_1(
        CStr::from_bytes_with_nul(b"rope-byte-length\0").unwrap(),
        scm_rope_byte_length,
        1,
    );
    ctx.define_subr_4(
        CStr::from_bytes_with_nul(b"rope-replace!\0").unwrap(),
        scm_rope_insert,
        4,
    );
}

extern "C" fn scm_make_rope() -> Scm {
    catch_unwind(|| {
        let rope = unsafe { RopeWrapper::to_scm(RopeWrapper::new().into()) };
        rope
    })
    .map_err(|_| "Rust panic encountered on make-rope.")
    .scm_unwrap()
}

extern "C" fn scm_rope_to_string(rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = RopeWrapper::from_scm(rope).unwrap();
        Scm::new_string(rope.to_string().as_str())
    })
    .map_err(|_| "Rust panic encountered on make-rope.")
    .scm_unwrap()
}

extern "C" fn scm_rope_byte_length(rope: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = RopeWrapper::from_scm(rope).unwrap();
        Scm::new_u32(rope.byte_len() as u32)
    })
    .map_err(|_| "Rust panic encountered on make-rope.")
    .scm_unwrap()
}

extern "C" fn scm_rope_insert(rope: Scm, start_byte: Scm, end_byte: Scm, text: Scm) -> Scm {
    catch_unwind(|| unsafe {
        let rope = RopeWrapper::from_scm_mut(rope).unwrap();
        let start = start_byte.to_u32() as usize;
        let end = end_byte.to_u32() as usize;
        if text.is_char() {
            let mut text_buffer = [0; 4];
            let char_str = text.to_char().unwrap().encode_utf8(&mut text_buffer);
            rope.replace(start..end, char_str);
        } else {
            rope.replace(start..end, text.to_string());
        }
    })
    .map_err(|_| "Rust panic encountered on make-rope.")
    .scm_unwrap();
    Scm::UNDEFINED
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn make_rope_creates_empty_rope() {
        let r = RopeWrapper::new();
        assert_eq!(r.to_string(), "");
    }
}

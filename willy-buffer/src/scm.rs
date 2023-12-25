use std::ffi::CStr;

use flashkick::{
    foreign_object::ForeignObjectType,
    module::{Module, ModuleInitContext},
    Scm,
};

#[no_mangle]
pub unsafe extern "C" fn scm_init_willy_buffer_module() {
    BufferModule.init();
}

use crate::Buffer;

struct BufferModule;

impl Module for BufferModule {
    fn name() -> &'static std::ffi::CStr {
        CStr::from_bytes_with_nul(b"willy buffer\0").unwrap()
    }

    unsafe fn define(&self, ctx: &mut ModuleInitContext) {
        ctx.define_type::<Buffer>();
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"new-buffer\0").unwrap(),
            scm_new_buffer,
        );
        ctx.define_subr_1(
            CStr::from_bytes_with_nul(b"buffer-to-text\0").unwrap(),
            scm_buffer_to_text,
        );
        ctx.define_subr_2(
            CStr::from_bytes_with_nul(b"buffer-insert-string\0").unwrap(),
            scm_buffer_insert_string,
        );
    }
}

extern "C" fn scm_new_buffer(text: Scm) -> Scm {
    let text = unsafe { text.to_string() };
    let buffer = Box::new(Buffer::with_text(&text));
    unsafe { Buffer::to_scm(buffer) }
}

extern "C" fn scm_buffer_to_text(buffer: Scm) -> Scm {
    let buffer = unsafe { Buffer::from_scm(&buffer) };
    let text = buffer.to_text();
    unsafe { Scm::new_string(&text) }
}

extern "C" fn scm_buffer_insert_string(buffer: Scm, string: Scm) -> Scm {
    let buffer = unsafe { Buffer::from_scm_mut(&buffer) };
    let string = unsafe { string.to_string() };
    buffer.push_chars(string.chars());
    Scm::EOL
}

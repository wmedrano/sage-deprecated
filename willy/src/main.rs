use std::panic::catch_unwind;

mod buffer;
mod tui;

fn main() {
    unsafe {
        flashkick::boot_guile(std::env::args(), || {
            catch_unwind(|| {
                tui::scm::scm_init_willy_internal_tui_module();
                buffer::scm::scm_init_willy_internal_buffer_module();
                flashkick::shell(std::env::args());
            })
        })
        .unwrap();
    }
}

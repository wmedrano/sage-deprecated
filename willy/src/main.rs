use std::panic::catch_unwind;

mod buffer_content;
mod tui;

fn main() {
    unsafe {
        flashkick::boot_guile(std::env::args(), || {
            catch_unwind(|| {
                tui::scm::scm_init_willy_core_internal_tui_module();
                buffer_content::scm::scm_init_willy_core_internal_buffer_content();
                flashkick::shell(std::env::args());
            })
        })
        .unwrap();
    }
}

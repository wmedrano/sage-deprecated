(define-module (willy buffer))

(load-extension "target/release/libwilly.so" "scm_init_willy_tui_module")

(define-public backspace-char "<backspace>")

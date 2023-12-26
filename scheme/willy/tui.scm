(define-module (willy buffer))

(load-extension "target/debug/libwilly.so" "scm_init_willy_tui_module")

(define-public backspace-char "<backspace>")

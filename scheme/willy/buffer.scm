(define-module (willy buffer))

(load-extension "target/debug/libwilly.so" "scm_init_willy_buffer_module")

(define-public (new-scratch-buffer)
  "Create a new scratch buffer."
  (new-buffer scratch-buffer-text))

(define scratch-buffer-text ";; Welcome to Willy! A Scheme based text editor.\n")

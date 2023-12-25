(define-module (willy buffer))

(load-extension "./target/debug/libwilly_buffer.so" "scm_init_willy_buffer_module")

(define scratch-buffer-text ";; Welcome to Willy! A Scheme based text editor.\n")

(define-public (new-scratch-buffer)
  (new-buffer scratch-buffer-text))

(load-extension "./target/debug/libwilly.so" "scm_init_willy_module")
(use-modules (willy))

(define log-file (open-file "/tmp/willy.log" "a"))

(define (handle-event event)
  (display event log-file)
  (newline log-file))
(run-willy handle-event)

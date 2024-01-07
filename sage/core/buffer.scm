(define-module (sage core buffer)
  #:export (
            make-buffer
            buffer-rope
            buffer-set-rope!
            buffer-file-path
            buffer-set-file-path!
            ))
(use-modules (srfi srfi-9))

(define-record-type <buffer>
  (%make-buffer rope file-path)
  buffer?
  (rope buffer-rope buffer-set-rope!)
  (file-path buffer-file-path buffer-set-file-path!))

(define* (make-buffer #:key
                      rope
                      (file-path ""))
  "Create a new record."
  (%make-buffer rope file-path))

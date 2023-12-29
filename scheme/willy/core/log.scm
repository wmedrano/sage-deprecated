(define-module (willy core log)
  #:export (
            log!
            log-buffer
            ))
(use-modules ((willy core buffer) #:prefix buffer:))

(define --log-buffer #f)

(define* (log-buffer)
  (if (not --log-buffer)
      (set! --log-buffer (buffer:make-buffer #:name "*log*")))
  --log-buffer)

(define* (log! . strings)
  (let ((b (log-buffer))
        (t (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))))
    (buffer:buffer-insert-string b "[")
    (buffer:buffer-insert-string b t)
    (buffer:buffer-insert-string b "]: ")
    (for-each (lambda (string) (buffer:buffer-insert-string b string))
              strings)
    (buffer:buffer-insert-string b "\n")))

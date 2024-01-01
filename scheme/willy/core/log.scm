(define-module (willy core log)
  #:export (
            log!
            log-buffer
            ))
(use-modules ((willy core buffer) #:prefix buffer:))

(define log-buffer (buffer:make-buffer #:name "*log*"))

(define* (log! . vals)
  (define t (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time))))
  (define (stringify x)
    (if (or (char? x) (string? x))
        x
        (object->string x)))
  (define (output x)
    (buffer:buffer-insert-string! log-buffer
                                 (stringify x)))
  (buffer:buffer-insert-string! log-buffer #\[)
  (buffer:buffer-insert-string! log-buffer t)
  (buffer:buffer-insert-string! log-buffer "]: ")
  (for-each output vals)
  (buffer:buffer-insert-string! log-buffer #\newline))

(define-module (willy core log)
  #:export (
            log!
            log-buffer
            ))
(use-modules ((willy core buffer) #:prefix buffer:))

(define log-buffer (buffer:make-buffer #:name "*log*"))

(define* (log! . strings)
  (let ((t (strftime "%Y-%m-%d %H:%M:%S" (localtime (current-time)))))
    (buffer:buffer-insert-string log-buffer "[")
    (buffer:buffer-insert-string log-buffer t)
    (buffer:buffer-insert-string log-buffer "]: ")
    (for-each (lambda (string) (buffer:buffer-insert-string log-buffer string))
              strings)
    (buffer:buffer-insert-string log-buffer "\n")))

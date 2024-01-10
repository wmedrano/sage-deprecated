(define-module (sage core log)
  #:export (
            clear-logs!
            log!
            log-buffer
            ))
(use-modules ((sage core rope)   #:prefix rope:)
             ((sage core buffer) #:prefix buffer:))

(define %log-buffer (buffer:make-buffer #:name "*log*"
                                        #:cursor 0))

(define* (log! #:rest args)
  "Log a message."
  (define (obj->string obj)
    (if (string? obj)
        obj
        (object->string  obj)))
  (buffer:buffer-insert-at-cursor! %log-buffer
                                   (string-concatenate (map obj->string args)))
  (buffer:buffer-insert-at-cursor! %log-buffer #\newline))

(define* (log-buffer)
  "Get the log buffer."
  %log-buffer)

(define* (clear-logs!)
  "Clear the logs."
  (buffer:buffer-set-cursor! %log-buffer 0)
  (rope:rope-set-string!
   (buffer:buffer-rope %log-buffer)
   ""))

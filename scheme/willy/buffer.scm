(define-module (willy buffer)
  #:export (buffer-content-to-string
	    buffer-content-pop-char
	    buffer-content-insert-string
	    make-buffer-content)
  #:use-module (willy internal))

(define* (buffer-content-to-string buffer-content)
  "Convert the contents of buffer-content to a string."
  (--buffer-content-to-string buffer-content))

(define* (buffer-content-pop-char buffer-content)
  "Pop a character from buffer-content."
  (--buffer-content-pop-char buffer-content))

(define* (buffer-content-insert-string buffer-content string)
  (--buffer-content-insert-string buffer-content string)
  buffer-content)

(define* (make-buffer-content #:optional string)
  "Create a new buffer."
  (if string
      (buffer-content-insert-string (--make-buffer-content) string)
      (--make-buffer-content)))

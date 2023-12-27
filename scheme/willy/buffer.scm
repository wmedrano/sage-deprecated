(define-module (willy buffer))
(use-modules (willy internal buffer-content))

(export buffer-content-to-string)
(define* (buffer-content-to-string buffer-content)
  "Convert the contents of buffer-content to a string."
  (--buffer-content-to-string buffer-content))

(export buffer-content-pop-char)
(define* (buffer-content-pop-char buffer-content)
  "Pop a character from buffer-content."
  (--buffer-content-pop-char buffer-content))

(export buffer-content-insert-string)
(define* (buffer-content-insert-string buffer-content string)
  (--buffer-content-insert-string buffer-content string)
  buffer-content)

(export make-buffer-content)
(define* (make-buffer-content #:optional string)
  "Create a new buffer."
  (if string
      (buffer-content-insert-string (--make-buffer-content) string)
      (--make-buffer-content)))

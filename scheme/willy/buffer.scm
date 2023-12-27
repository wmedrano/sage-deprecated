(define-module (willy buffer)
  #:export (make-buffer
            buffer-to-string
	    buffer-pop-char
	    buffer-insert-string
            buffer-name)
  #:use-module (willy internal))

(define* (make-buffer #:key
                      (name   "*default*")
                      (string ""))
  "Create a new buffer."
  `((name . ,name)
    (content . ,(--buffer-content-insert-string (--make-buffer-content) string))))

(define* (buffer-content buffer)
  "Get the underlying buffer contents object."
  (assoc-ref buffer 'content))

(define* (buffer-name buffer)
  "Get the buffer's name."
  (assoc-ref buffer 'name))

(define* (buffer-to-string buffer)
  "Convert the contents of buffer-content to a string."
  (--buffer-content-to-string (buffer-content buffer)))

(define* (buffer-pop-char buffer)
  "Pop a character from buffer-content."
  (--buffer-content-pop-char (buffer-content buffer)))

(define* (buffer-insert-string buffer string)
  "Insert string into the contents of buffer."
  (--buffer-content-insert-string (buffer-content buffer) string)
  buffer)

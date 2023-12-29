(define-module (willy core buffer)
  #:export (
	    buffer-insert-string
	    buffer-pop-char
	    buffer-set-string
            buffer-name
            buffer-to-string
            make-buffer
            ))
(use-modules ((willy core internal) #:prefix internal:)
             ((srfi srfi-1)))

(define* (make-buffer #:key
                      (name   "*default*")
                      (string ""))
  "Create a new buffer."
  `((name . ,name)
    (content . ,(internal:--buffer-content-insert-string (internal:--make-buffer-content) string))))

(define* (buffer-content buffer)
  "Get the underlying buffer contents object."
  (assoc-ref buffer 'content))

(define* (buffer-name buffer)
  "Get the buffer's name."
  (assoc-ref buffer 'name))

(define* (buffer-to-string buffer)
  "Convert the contents of buffer-content to a string."
  (internal:--buffer-content-to-string (buffer-content buffer)))

(define* (buffer-pop-char buffer)
  "Pop a character from buffer-content."
  (internal:--buffer-content-pop-char (buffer-content buffer)))

(define* (buffer-insert-string buffer string)
  "Insert string into the contents of buffer."
  (internal:--buffer-content-insert-string (buffer-content buffer) string)
  buffer)

(define* (buffer-set-string buffer string)
  "Set the contents of the buffer to string."
  (internal:--buffer-content-set-string (buffer-content buffer) string)
  buffer)

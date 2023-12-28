(define-module (willy core buffer)
  #:export (
	    buffer-insert-string
	    buffer-pop-char
	    buffer-set-string
            buffer-by-name
            buffer-name
            buffer-to-string
            buffers
            make-buffer
            register-buffer!
            remove-buffer-by-name!
            )
  #:use-module (willy core internal)
  #:use-module (srfi srfi-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stateful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define buffer-registry
  (list
   (make-buffer #:name "main"
                #:string ";; Welcome to Willy! A Scheme based text environment.\n")
   (make-buffer #:name "*status*"
                #:string "Willy | Status: OK")))

(define* (register-buffer! buffer)
  "Register a new buffer."
  (set! buffer-registry (cons buffer buffer-registry))
  buffer)

(define* (remove-buffer-by-name! name)
  (let ((b (buffer-by-name name)))
    (if (and b (pair? buffer-registry))
        (set! buffer-registry
              (filter (lambda (b) (not (equal? (buffer-name b)
                                               name)))
                      buffer-registry)))
    b))

(define* (buffers)
  "Get a list of all the buffers. Buffers consist of an alist of:
- 'buffer - The underlying buffer.
- 'name   - The name of the buffer."
  buffer-registry)

(define* (buffer-by-name name #:key (allow-create? #f))
  "Get a buffer by its name.

If the buffer does not exist and allow-create? is #t, then a new
buffer will be created and returned."
  (let ((found (find (lambda (buffer) (equal? (buffer-name buffer)
                                              name))
                     (buffers))))
    (or found
        (if allow-create?
            (register-buffer! (make-buffer #:name name))
            #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define* (buffer-set-string buffer string)
  "Set the contents of the buffer to string."
  (--buffer-content-set-string (buffer-content buffer) string)
  buffer)

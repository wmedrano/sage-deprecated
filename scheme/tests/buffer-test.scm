(use-modules (srfi srfi-64)
	     (willy buffer))

(define %test-suite-name "buffer")
(test-begin %test-suite-name)

(test-equal "New buffers have empty tests."
  ""
  (buffer-to-string (make-buffer)))

(test-equal "Make buffer with name has valid name"
  "my custom name"
  (buffer-name (make-buffer #:name "my custom name")))

(test-equal "New buffers have empty tests."
  ""
  (buffer-to-string (make-buffer)))

(test-equal "Make buffer with string has valid string"
  "my text"
  (buffer-to-string (make-buffer #:string "my text")))

(test-equal "Can insert string to buffer."
  "name: Willy\n"
  (buffer-to-string
   (buffer-insert-string
    (make-buffer #:string "name: ")
    "Willy\n")))

(test-equal "buffer-pop-char removes last character."
  "my tex"
  (let ((b (make-buffer #:string "my text")))
    (buffer-pop-char b)
    (buffer-to-string b)))

(test-equal "buffer-pop-char returns last character."
  "t"
  (buffer-pop-char (make-buffer #:string "my text")))

(test-end %test-suite-name)

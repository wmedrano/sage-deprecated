(use-modules (srfi srfi-64)
	     (willy buffer))

(define %test-suite-name "willy-buffer")
(test-begin %test-suite-name)

(test-equal "New buffers have empty tests."
  ""
  (buffer-content-to-string (make-buffer-content)))

(test-equal "New buffers have empty tests."
  ""
  (buffer-content-to-string (make-buffer-content)))

(test-equal "Buffer string can be initialized."
  "my text"
  (buffer-content-to-string (make-buffer-content "my text")))

(test-equal "Can insert string to buffer."
  "name: Willy\n"
  (buffer-content-to-string
   (buffer-content-insert-string
    (make-buffer-content "name: ")
    "Willy\n")))

(test-equal "buffer-content-pop-char removes last character."
  "my tex"
  (let ((b (make-buffer-content "my text")))
    (buffer-content-pop-char b)
    (buffer-content-to-string b)))

(test-equal "buffer-content-pop-char returns last character."
  "t"
  (buffer-content-pop-char (make-buffer-content "my text")))

(test-end %test-suite-name)

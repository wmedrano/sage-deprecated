(use-modules (srfi srfi-64)
	     (willy buffer))

(define %test-suite-name "willy-buffer")
(test-begin %test-suite-name)

(test-equal "New buffers have empty tests."
  ""
  (buffer-to-text (new-buffer)))

(test-equal "New buffers have empty tests."
  ""
  (buffer-to-text (new-buffer)))

(test-equal "Buffer text can be initialized."
  "my text"
  (buffer-to-text (new-buffer "my text")))

(test-equal "Can insert text to buffer."
  "name: Willy\n"
  (let ((b (new-buffer "name: ")))
    (buffer-insert-string b "Willy\n")
    (buffer-to-text b)))

(test-equal "buffer-pop-char removes last character."
  "my tex"
  (let ((b (new-buffer "my text")))
    (buffer-pop-char b)
    (buffer-to-text b)))

(test-equal "buffer-pop-char returns last character."
  "t"
  (buffer-pop-char (new-buffer "my text")))

(test-end %test-suite-name)

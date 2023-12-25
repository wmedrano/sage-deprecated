(use-modules (srfi srfi-64)
	     (willy buffer))

(define %test-suite-name "willy-buffer")
(test-begin %test-suite-name)

(test-equal "New buffers have empty tests."
  (buffer-to-text (new-buffer))
  "")

(test-equal "New buffers have empty tests."
  (buffer-to-text (new-buffer))
  "")

(test-equal "Buffer text can be initialized."
  (buffer-to-text (new-buffer "my text"))
  "my text")

(test-equal "Can insert text to buffer."
  (let ((b (new-buffer "name: ")))
    (buffer-insert-string b "Willy\n")
    (buffer-to-text b))
  "name: Willy\n")

(test-end %test-suite-name)

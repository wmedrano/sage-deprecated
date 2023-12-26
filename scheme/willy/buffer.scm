(define-module (willy buffer))
(use-modules (willy internal buffer))

(define-public (buffer-to-text buffer)
  (--buffer-to-text buffer))

(define-public (buffer-pop-char buffer)
  (--buffer-pop-char buffer))

(define-public (buffer-insert-string buffer string)
  (--buffer-insert-string buffer string))

(export new-buffer)
(define* (new-buffer #:optional text)
  "Create a new buffer."
  (let ((b (--new-buffer)))
    (if text (buffer-insert-string b text))
    b))

(define-public (new-scratch-buffer)
  "Create a new scratch buffer."
  (new-buffer scratch-buffer-text))

(define scratch-buffer-text ";; Welcome to Willy! A Scheme based text editor.\n")

(define-module (sage core rope)
  #:export (
            make-rope
            rope->string
            rope-byte-length
            rope-replace!
            rope-delete!
            rope-insert!
            rope-clear!
            ))
(use-modules ((sage core internal) #:prefix ffi:))

(define* (make-rope #:key (text ""))
  "Create a new rope."
  (let ((rope (ffi:make-rope)))
    (unless (equal? text "")
      (rope-insert! rope 0 text))
    rope))

(define* (rope->string rope)
  "Get the string representation in the rope."
  (ffi:rope->string rope))

(define* (rope-byte-length rope)
  "Get the length (in bytes) of the rope."
  (ffi:rope-byte-length rope))

(define* (rope-replace! rope start end string-or-char)
  "Replace the contents between start (inclusive) and end (exclusive)
with string-or-char."
  (ffi:rope-replace! rope start end string-or-char))

(define* (rope-delete! rope start end)
  "Delete the contents between start (inclusive) and end (exclusive)."
  (rope-replace! rope start end ""))

(define* (rope-insert! rope position string-or-char)
  "Insert string-or-char into the rope at position."
  (rope-replace! rope position position string-or-char))

(define* (rope-clear! rope)
  "Delete the contents between start (inclusive) and end (exclusive)."
  (rope-delete! rope 0 (rope-byte-length rope)))

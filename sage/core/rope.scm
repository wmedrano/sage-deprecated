(define-module (sage core rope)
  #:export (
            make-rope
            rope->string
            rope-append!
            rope-byte-length
            rope-clear!
            rope-delete!
            rope-set-string!
            rope-insert!
            rope-pop!
            rope-replace!
            rope-set-language!
            ))
(use-modules ((sage core internal) #:prefix ffi:)
             (srfi srfi-2))

(define* (make-rope #:key
                    (text "")
                    (language ""))
  "Create a new rope."
  (let ((rope (ffi:make-rope)))
    (unless (equal? text "")
      (rope-insert! rope 0 text))
    (unless (equal? language "")
      (rope-set-language! rope language))
    rope))

(define* (rope->string rope)
  "Get the string representation in the rope."
  (ffi:rope->string rope))

(define* (rope-append! rope string-or-char)
  "Append string-or-char to the end of the rope."
  (rope-insert! rope (rope-byte-length rope) string-or-char)
  rope)

(define* (rope-pop! rope)
  "Delete the last byte in the rope."
  (and-let* ((end       (rope-byte-length rope))
             (has-text? (> end 0))
             (start     (- end 1)))
    (rope-delete! rope start end))
  rope)

(define* (rope-byte-length rope)
  "Get the length (in bytes) of the rope."
  (ffi:rope-byte-length rope))

(define* (rope-clear! rope)
  "Delete the contents between start (inclusive) and end (exclusive)."
  (rope-delete! rope 0 (rope-byte-length rope)))

(define* (rope-delete! rope start end)
  "Delete the contents between start (inclusive) and end (exclusive)."
  (rope-replace! rope start end ""))

(define* (rope-insert! rope position string-or-char)
  "Insert string-or-char into the rope at position."
  (rope-replace! rope position position string-or-char))

;; TODO: This does not handle UTF-8 well. If the bytes are replaced
;; with invalid UTF-8, then the Rust portion will crash. The most
;; common scenario is when editing part of a UTF-8 word. Example:
;;   - rope-append! a robot emoji(🤖) onto a rope.
;;   - rope-pop! the last byte of the robot emoji.
;;   - See rust stack trace error: byte offset 11 is not a char boundary: it is inside '🤖' (bytes 8..12) of "🤖🤖🤖"
(define* (rope-replace! rope start end string-or-char)
  "Replace the contents between start (inclusive) and end (exclusive)
with string-or-char."
  (ffi:rope-replace! rope start end string-or-char))

(define* (rope-set-string! rope string)
  "Sets the string for the rope."
  (rope-clear! rope)
  (rope-append! rope string))

(define* (rope-set-language! rope language)
  "Set the language for the rope. Valid values are empty string for no
language or rust."
  (ffi:rope-set-language! rope language)
  rope)

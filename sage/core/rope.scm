(define-module (sage core rope)
  #:export (
            make-rope
            rope->string
            rope-length
            rope-cursor-line-offset
            rope-insert!
            rope-pop!
            rope-replace!
            rope-set-language!
            rope-position->cursor
            rope-cursor->position
            rope-set-string!
            ))
(use-modules ((sage core internal) #:prefix ffi:)
             (srfi srfi-2))

(define* (make-rope #:key
                    (text "")
                    (language ""))
  "Create a new rope."
  (let ((rope (ffi:make-rope)))
    (unless (equal? text "")
      (rope-set-string! rope text))
    (unless (equal? language "")
      (rope-set-language! rope language))
    rope))

(define* (rope->string rope)
  "Get the string representation in the rope."
  (ffi:rope->string rope))

;; TODO: This does not handle UTF-8 well. If the bytes are replaced
;; with invalid UTF-8, then the Rust portion will crash. The most
;; common scenario is when editing part of a UTF-8 word. Example:
;;   - rope-insert! a robot emoji(🤖) onto a rope.
;;   - rope-delete! just the last byte of the robot emoji.
;;   - See rust stack trace error: byte offset 11 is not a char boundary: it is inside '🤖' (bytes 8..12) of "🤖🤖🤖"
(define* (rope-replace! rope start end string-or-char)
  "Replace the contents between start (inclusive) and end (exclusive)
with string-or-char. The new end byte is returned"
  (ffi:rope-replace! rope start end string-or-char))

(define* (rope-insert! rope position string-or-char)
  "Insert string-or-char into the rope at position. Returns the new
end byte index."
  (rope-replace! rope position position string-or-char))

(define* (rope-delete! rope start end)
  "Delete the contents between start and end."
  (rope-replace! rope start end ""))

(define* (rope-set-string! rope string)
  "Sets the string for the rope."
  (rope-replace! rope 0 (rope-length rope) string))

(define* (rope-length rope)
  "Get the length of the rope in bytes."
  (ffi:rope-length rope))

(define* (rope-position->cursor rope position)
  (ffi:rope-position->cursor rope position))

(define* (rope-cursor->position rope cursor)
  (ffi:rope-cursor->position rope cursor))

(define* (rope-set-language! rope language)
  "Set the language for the rope. Valid values are empty string for no
language or rust."
  (ffi:rope-set-language! rope language)
  rope)

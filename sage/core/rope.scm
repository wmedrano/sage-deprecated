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
            rope-set-string!
            rope-line-count
            rope-line-length
            rope-line->cursor
            rope-cursor->line
            ))
(use-modules ((sage core internal) #:prefix ffi:)
             (srfi srfi-2))

(define* (make-rope #:key
                    (text "")
                    (language ""))
  "Create a new rope."
  (define rope (ffi:make-rope))
  (unless (equal? text "")
    (rope-set-string! rope text))
  (unless (equal? language "")
    (rope-set-language! rope language))
  rope)

(define* (rope-set-language! rope language)
  "Set the language for the rope. Valid values are empty string for no
language or rust."
  (ffi:rope-set-language! rope language)
  rope)

(define* (rope->string rope)
  "Get the string representation in the rope."
  (ffi:rope->string rope))

(define* (rope-replace! rope start end string-or-char)
  "Replace the contents between start (inclusive) and end (exclusive)
with string-or-char. The new end cursor is returned."
  (rope-delete! rope start end)
  (rope-insert! rope start string-or-char))

(define* (rope-insert! rope cursor string-or-char)
  "Insert string-or-char into the rope at cursor. Returns the new
end cursor."
  (ffi:rope-insert! rope cursor string-or-char))

(define* (rope-delete! rope start end)
  "Delete the contents between start and end."
  (ffi:rope-delete! rope start end))

(define* (rope-set-string! rope string)
  "Sets the string for the rope."
  (rope-replace! rope 0 (rope-length rope) string))

(define* (rope-length rope)
  "Get the length of the rope in number of characters."
  (ffi:rope-length rope))

(define* (rope-line-count rope)
  "Returns the number of lines within the rope."
  (ffi:rope-line-count rope))

(define* (rope-line-length rope line)
  "Returns the length of the given line."
  (ffi:rope-line-length rope line))

(define* (rope-line->cursor rope line)
  "Get the cursor (character index) for the given line."
  (ffi:rope-line->cursor rope line))

(define* (rope-cursor->line rope cursor)
  "Get the line for the cursor (character index)."
  (ffi:rope-cursor->line rope cursor))

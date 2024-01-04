(define-module (sage core rope)
  #:export (
            make-rope
            rope->string
            ))
(use-modules ((sage core internal) #:prefix ffi:))

(define* (make-rope)
  "Create a new rope."
  (ffi:make-rope))

(define* (rope->string rope)
  "Get the string representation in the rope."
  (ffi:rope->string rope))

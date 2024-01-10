(define-module (sage builtin tutorial)
  #:export (make-tutorial-buffer))
(use-modules
 ((sage core buffer)    #:prefix buffer:)
 ((sage core rope)      #:prefix rope:)
 (srfi srfi-1))

(define* (make-tutorial-buffer)
  "Make a new tutorial buffer."
  (buffer:make-buffer
   #:name "*tutorial*"
   #:rope   (rope:make-rope
             #:text     "Welcome to Sage!\n\n

ctrl+o - Open a file.
ctrl+p - Select and execute a command.
ctrl+c - Exit.")))

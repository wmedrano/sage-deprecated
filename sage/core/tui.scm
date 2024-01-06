(define-module (sage core tui)
  #:export (
            make-tui
            tui-draw!
            delete-tui!
            tui->string
            ))
(use-modules ((sage core internal) #:prefix ffi:))

(define* (make-tui #:key (backend 'test))
  "Create a new tui with the given backend.

Valid backend values are 'test and 'terminal."
  (ffi:make-tui backend))

(define* (tui-draw! tui rope)
  "Draw the contents of rope on tui. Returns the size of the terminal
window."
  (ffi:tui-draw! tui rope))

(define* (delete-tui! tui)
  "Delete the tui.

This is mostly used to forcefully restore the terminal state back to normal."
  (ffi:delete-tui! tui))

(define* (tui->string tui)
  "Convert the contents of tui to string.

Only valid for test backend."
  (ffi:tui->string tui))

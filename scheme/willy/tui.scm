(define-module (willy tui))
(use-modules (willy internal tui))

(define-public backspace-key "<backspace>")

(export new-tui)
(define* (new-tui #:optional backend-type)
  "Create a new terminal UI.
backend-type must be either 'default or 'test."
  (--new-tui (or backend-type 'default)))

(define-public (delete-tui tui)
  "Delete a terminal UI."
  (--delete-tui tui))

(define-public (next-event)
  "Get the next terminal event."
  (--next-event))

(define-public (tui-draw tui buffers)
  "Draw a new screen for the given tui containing the given buffers."
  (--tui-draw tui buffers))

(define-public (tui-size tui)
  "Get the size of the terminal.

Example return value: '((width . 80) (height . 24))"
  (--tui-size tui))

(define-public (tui-state-for-test tui)
  "Get the state (as a string) for the test tui.

The tui must have been constructed with (new-tui 'test)"
  (--tui-state-for-test tui))

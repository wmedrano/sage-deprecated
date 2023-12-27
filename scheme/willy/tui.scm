(define-module (willy tui)
  #:export (backspace-key make-tui delete-tui tui-draw tui-size tui-state-for-test)
  #:use-module (willy internal))

(define* backspace-key "<backspace>")

(define* (make-tui #:optional backend-type)
  "Create a new terminal UI.
backend-type must be either 'default or 'test."
  (--make-tui (or backend-type 'default)))

(define* (delete-tui tui)
  "Delete a terminal UI."
  (--delete-tui tui))

(define* (tui-draw tui layouts)
  "Draw a new screen for the given tui containing the given layouts.

Layout should be a list of layout where each layout consists of an
alist with the following keys:
- buffer - The buffer to render.
- x - The x position. If not present, then 0 will be used.
- y - The y position. If not present, then 0 will be used.
- width - The width of the layout.
- height - The height of the layout."
  (--tui-draw tui layouts)
  tui)

(define* (tui-size tui)
  "Get the size of the terminal.

Example return value: '((width . 80) (height . 24))"
  (--tui-size tui))

(define* (tui-state-for-test tui)
  "Get the state (as a string) for the test tui.

The tui must have been constructed with (make-tui 'test)"
  (--tui-state-for-test tui))

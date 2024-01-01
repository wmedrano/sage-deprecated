(define-module (willy core tui)
  #:export (
            delete-tui!
            main-tui
            make-tui
            tui-draw
            tui-is-active?
            tui-size
            tui-state-for-test
           ))
(use-modules ((willy core internal) #:prefix internal:))


(define* (make-tui #:optional backend-type)
  "Create a new terminal UI.
backend-type must be either 'terminal or 'test. If not set, then 'test
will be used."
  (internal:--make-tui (or backend-type 'test)))

(define* (delete-tui! tui)
  "Delete a terminal UI."
  (internal:--delete-tui tui))

(define* (tui-draw tui windows)
  "Draw a new screen for the given tui containing the given windows.

Windows should be a list of windows where each element consists of an
alist with the following keys:
- buffer - The buffer to render.
- x - The x position. If not present, then 0 will be used.
- y - The y position. If not present, then 0 will be used.
- width - The width of the layout.
- height - The height of the layout.
- features - An alist to add special features such as borders and line numbers."
  (internal:--tui-draw tui windows)
  tui)

(define* (tui-size tui)
  "Get the size of the terminal.

Example return value: '((width . 80) (height . 24))"
  (internal:--tui-size tui))

(define* (tui-state-for-test tui)
  "Get the state (as a string) for the test tui.

The tui must have been constructed with the 'test backend like:
(make-tui 'test)"
  (internal:--tui-state-for-test tui))

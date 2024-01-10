(define-module (sage state)
  #:export (
            event-hook
            resize-hook
            quit-hook

            buffers
            remove-buffer!
            add-buffer!
            add-task!
            add-window!
            focused-window
            frame-height
            frame-width
            quit!
            remove-window!
            run!
            set-focused-window!
            tui
            windows
            ))
(use-modules ((sage core window)   #:prefix window:)
             ((sage core tui)      #:prefix tui:)
             ((sage core internal) #:prefix internal:)
             (srfi srfi-2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Called on the focused window for each event. The procedure must be
;; of the form: (proc window buffer event)
(define event-hook (make-hook 3))

;; Called when the terminal window is resized. The parameters are the
;; new width and height.
(define resize-hook (make-hook 2))

(define %tasks (make-hook))

;; Called when the sage will quit.
(define quit-hook (make-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define %tui #f)
(define %tui-frame-size '((width . 80) (height . 24)))

(define* (frame-width)
  "Get the width of the frame."
  (assoc-ref %tui-frame-size 'width))

(define* (frame-height)
  "Get the height of the frame."
  (assoc-ref %tui-frame-size 'height))

(define* (quit!)
  "Quit out of sage."
  (when %tui
    (tui:delete-tui! %tui)
    (run-hook quit-hook)
    (set! %tui #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* %windows '())
(define* %focused-window #f)

(define* (windows)
  "Get a list of the current windows."
  %windows)

(define* (add-window! window
                      #:key
                      (set-focus?       #f))
  "Add a new window and return it.

set-focus? - If true, then the window will automatically be focused.
"
  (set! %windows (cons window %windows))
  (when set-focus?
    (set-focused-window! window))
  (add-buffer! (window:window-buffer window))
  window)

(define* (remove-window! window)
  "Remove the given window."
  (set! %windows
        (filter (lambda (w) (not (eq? w window)))
                %windows))
  (when (eq? focused-window window)
    (set-focused-window! (if (null? %windows)
                             #f
                             (car %windows)))))

(define* (focused-window)
  "Get the currently focused window or #f if no window is in focus."
  %focused-window)

(define* (set-focused-window! window)
  "Set the focused window to window. Window can be #f, in which case
no window will have focus."
  (set! %focused-window window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* %buffers '())

(define* (buffers)
  "Get a list of all the registered buffers."
  %buffers)

(define* (remove-buffer! buffer)
  "Removes the buffer from the set of registered buffers."
  (set! %buffers (filter (lambda (b) (not (eq? buffer b)))
                         %buffers)))

(define* (add-buffer! buffer)
  "Register the given buffer if it is not already registered. If it is
registered, then do nothing."
  (unless (member buffer %buffers)
    (set! %buffers (cons buffer %buffers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (add-task! task)
  (add-hook! %tasks task))

(define* (%run-tasks!)
  (run-hook %tasks)
  (reset-hook! %tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main program.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (run! tui)
  "Run the Sage text editor."
  (quit!)
  (set! %tui tui)
  (define (init-frame-size!)
    (let ((frame-size (tui:tui-draw! %tui '())))
      (run-hook resize-hook
                (assoc-ref frame-size 'width)
                (assoc-ref frame-size 'height))))
  (define (run-loop-iteration!)
    (let handle-all-events ((events (internal:events-from-terminal)))
      (for-each (lambda (e)
                  (and-let* ((window %focused-window)
                             (buffer (window:window-buffer window)))
                    (run-hook event-hook window buffer e))
                  (%run-tasks!))
                events)
      (if (pair? events) (handle-all-events (internal:events-from-terminal))))
    (let ((frame-size (and %tui
                           (tui:tui-draw! %tui (windows)))))
      (when (and %tui (not (equal? frame-size %tui-frame-size)))
        (run-hook resize-hook
                  (assoc-ref frame-size 'width)
                  (assoc-ref frame-size 'height))
        (set! %tui-frame-size frame-size))))
  (define (run-event-loop)
    (init-frame-size!)
    (while %tui
      (run-loop-iteration!))
    (quit!))
  (define (cleanup-and-reraise-exception e)
    (quit!)
    (raise-exception e))
  (with-exception-handler cleanup-and-reraise-exception run-event-loop))

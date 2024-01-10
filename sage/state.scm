(define-module (sage state)
  #:export (
            resize-hook
            quit-hook
            add-window-hook
            add-buffer-hook

            buffers
            remove-buffer!
            add-buffer!
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
(use-modules ((sage core buffer)   #:prefix buffer:)
             ((sage core internal) #:prefix internal:)
             ((sage core log)      #:prefix log:)
             ((sage core tui)      #:prefix tui:)
             ((sage core window)   #:prefix window:)
             (srfi srfi-2))

;; Called when the sage will quit.
(define quit-hook (make-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define %tui #f)
(define %tui-frame-size '((width . 80) (height . 24)))

;; Called when the terminal window is resized. The parameters are the
;; new width and height.
(define resize-hook (make-hook 2))

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

(define add-window-hook (make-hook 1))
(define add-buffer-hook (make-hook 1))

(define* (windows)
  "Get a list of the current windows."
  %windows)

(define* (add-window! window
                      #:key
                      (set-focus?       #f))
  "Add a new window and return it.

set-focus? - If true, then the window will automatically be focused.
"
  (unless (member window %windows)
    (set! %windows (cons window %windows))
    (run-hook add-window-hook window))
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
    (set! %buffers (cons buffer %buffers))
    (run-hook add-buffer-hook buffer)))

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
  (define (handle-all-events)
    (let handle-all-events ((events (internal:events-from-terminal)))
      (for-each (lambda (e)
                  (and-let* ((window %focused-window)
                             (buffer (window:window-buffer window)))
                    (run-hook (buffer:buffer-event-hook buffer)
                              window buffer e)))
                events)
      (if (pair? events) (handle-all-events (internal:events-from-terminal)))))
  (define (draw)
    (let ((frame-size (and %tui
                           (tui:tui-draw! %tui (windows)))))
      (when (and %tui (not (equal? frame-size %tui-frame-size)))
        (run-hook resize-hook
                  (assoc-ref frame-size 'width)
                  (assoc-ref frame-size 'height))
        (set! %tui-frame-size frame-size)))  )
  (define (run-loop-iteration!)
    (handle-all-events)
    (draw))
  (define (run-main-loop)
    (init-frame-size!)
    (while %tui
      (run-loop-iteration!))
    (quit!))
  (define (cleanup-and-reraise-exception e)
    (quit!)
    (raise-exception e))
  (with-exception-handler cleanup-and-reraise-exception run-main-loop))

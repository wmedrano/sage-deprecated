(define-module (sage state)
  #:export (
            event-hook
            resize-hook
            quit-hook

            run-event-hook
            run-resize-hook
            run-quit-hook

            quit!
            tui
            set-tui!
            remove-window!
            windows
            focused-window
            add-window!
            set-focused-window!
            ))
(use-modules ((sage core tui) #:prefix tui:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Called for each event.
(define event-hook (make-hook 1))

;; Called when the terminal window is resized. The parameters are the
;; new width and height.
(define resize-hook (make-hook 2))

(define quit-hook (make-hook))

(define* (run-event-hook event)
  (run-hook event-hook event))

(define* (run-resize-hook width height)
  (run-hook resize-hook width height))

(define* (run-quit-hook)
  (run-hook quit-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define %tui #f)

(define* (tui)
  "Return the current TUI."
  %tui)

(define* (quit!)
  "Quit out of sage."
  (when %tui
    (tui:delete-tui! %tui)
    (run-hook quit-hook)
    (set! %tui #f)))

(define* (set-tui! tui)
  (quit!)
  (set! %tui tui))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* %windows '())
(define* %focused-window #f)

(define* (windows)
  %windows)

(define* (add-window! window #:key set-focus?)
  (set! %windows (cons window %windows))
  (when set-focus?
    (set-focused-window! window)))

(define* (remove-window! window)
  (set! %windows
        (filter (lambda (w) (not (eq? w window)))
                %windows))
  (when (eq? focused-window window)
    (set-focused-window! (if (null? %windows)
                             #f
                             (car %windows)))))

(define* (focused-window)
  %focused-window)

(define* (set-focused-window! window)
  (set! %focused-window window))

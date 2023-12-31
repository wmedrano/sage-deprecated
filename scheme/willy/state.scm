(define-module (willy state)
  #:export (
            ;; Hooks
            event-hook
            frame-resize-hook
            window-focus-hook
            window-unfocus-hook

            buffer-by-name
            buffer-for-focused-window
            frame-size
            retain-windows!
            set-focused-window!
            add-window!
            previous-frame-size
            quit!
            focused-window
            tui
            windows
            ))
(use-modules ((willy core log)    #:select (log-buffer log!))
             ((willy core tui)    #:prefix tui:)
             ((willy core buffer) #:prefix buffer:)
             ((willy core window) #:prefix window:)
             ((srfi srfi-1))
             ((srfi srfi-2))
             ((srfi srfi-111)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Runs when the frame is resized. The arguments are the new width and
;; the new height. To get the previous frame size, call (unbox
;; previous-frame-size).
(define* frame-resize-hook (make-hook 2))

;; Called for each event. The event is the only argument to the hook.
(define* event-hook (make-hook 1))

;; Called when a window gains focus.
(define* window-focus-hook (make-hook 1))

;; Called when a window loses focus.
(define* window-unfocus-hook (make-hook 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* buffer-registry
  (box (list
        log-buffer
        (buffer:make-buffer
         #:name "main"
         #:string ";; Welcome to Willy! A Scheme based text environment.\n")
        (buffer:make-buffer
         #:name "*status*"
         #:string "| Willy | Status: OK"))))

(define* (buffer-by-name name)
  "Get a buffer by its name or #f if it does not exist."
  (let buffer-by-name ((buffers (unbox buffer-registry)))
    (and-let* ((is-pair? (pair? buffers))
               (head     (car buffers))
               (head-name (buffer:buffer-name head)))
      (if (equal? head-name name)
          head
          (buffer-by-name (cdr buffers))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* tui (box #f))
(define* previous-frame-size (box '((width . 254) (height . 60))))
(define* frame-size (box '((width . 254) (height . 60))))

(define* (quit!)
  "Exit/quit out of Willy."
  (when (unbox tui)
    (log! "Quitting Willy!")
    (tui:delete-tui (unbox tui))
    (set-box! tui #f)))

(define* (windows)
  "Get all the registered windows."
  (unbox windows-box))

(define* windows-box
  (box
   (let ((width  (assoc-ref (unbox frame-size) 'width))
         (height (assoc-ref (unbox frame-size) 'height)))
     (list
      (window:make-window #:buffer   (buffer-by-name "*status*")
                          #:features '((status-bar? . #t))
                          #:x        0
                          #:y        (- height 1)
                          #:width    width
                          #:height   1)
      (window:make-window #:buffer   (buffer-by-name "main")
                          #:features '((line-numbers   . #t)
                                       (highlight-line . #t)
                                       (cursor         . #t)
                                       (editable?      . #t))
                          #:x        0
                          #:y        0
                          #:width    (/ width 2)
                          #:height   height)
      (window:make-window #:buffer   log-buffer
                          #:features '((line-numbers . #t))
                          #:x        (/ width 2)
                          #:y        0
                          #:width    (/ width 2)
                          #:height   height)))))

(define focused-window-box (box #f))

(define* (set-focused-window! window)
  "Set the newly focused window."
  (let* ((previous-focused-window (unbox focused-window-box)))
    (unless (eq? previous-focused-window window)
      (set-box! focused-window-box window)
      (when previous-focused-window
        (run-hook window-focus-hook previous-focused-window))
      (when window
        (run-hook window-focus-hook window)))))

(define* (focused-window)
  "Get the currently focused window.

The focused window is the one with the focused? feature."
  (unbox focused-window-box))

(define* (buffer-for-focused-window)
  "Get the buffer for the currently focused window."
  (and-let* ((w (focused-window)))
    (window:window-buffer w)))

(define* (retain-windows! pred)
  "Remove all windows that do not pass the predicate."
  (set-box! windows-box
            (filter pred (windows))))

(define* (add-window! window #:key (set-focus? #f))
  "Add window to the set of windows."
  (set-box! windows (cons window (windows)))
  (when set-focus?
    (set-focused-window! window)))

(define-module (willy state)
  #:export (
            buffer-by-name
            buffer-for-selected-window
            event-hook
            frame-resize-hook
            frame-size
            retain-windows!
            add-window!
            previous-frame-size
            quit!
            selected-window
            tui
            windows
            ))
(use-modules ((willy core log)    #:prefix log:)
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
(define event-hook (make-hook 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* buffer-registry
  (box (list
        log:log-buffer
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
    (log:log! "Quitting Willy!")
    (tui:delete-tui (unbox tui))
    (set-box! tui #f)))

(define* windows
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
                                       (selected?      . #t)
                                       (editable?      . #t))
                          #:x        0
                          #:y        0
                          #:width    (/ width 2)
                          #:height   height)
      (window:make-window #:buffer   log:log-buffer
                          #:features '((line-numbers . #t))
                          #:x        (/ width 2)
                          #:y        0
                          #:width    (/ width 2)
                          #:height   height)))))

(define* (selected-window)
  "Get the currently selected window.

The selected window is the one with the selected? feature."
  (find (lambda (w) (window:window-feature w 'selected?))
        (unbox windows)))

(define* (buffer-for-selected-window)
  "Get the buffer for the currently selected window."
  (and-let* ((w (selected-window)))
    (window:window-bufer w)))

(define* (retain-windows! pred)
  "Remove all windows that do not pass the predicate."
  (set-box! windows
            (filter pred (unbox windows))))

(define* (add-window! window)
  "Add window to the set of windows."
  (set-box! windows (cons window (unbox windows))))

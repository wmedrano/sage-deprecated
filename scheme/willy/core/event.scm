(define-module (willy core event)
  #:export (
            backspace-key
            list->event-pump
            next-event-from-terminal
            run-event-loop
            special-key?
            ))
(use-modules
 ((willy core tui)           #:prefix tui:)
 ((willy core log)           #:prefix log:)
 ((willy core internal)      #:prefix internal:)
 ((willy core frame-limiter) #:prefix frame-limiter:)
 ((srfi srfi-1))
 ((srfi srfi-111)))

(define* backspace-key "<backspace>")

(define* (run-event-loop #:key
			 (tui           (tui:make-tui 'test))
			 (should-run-p  (lambda () #f))
			 (make-layout   (lambda () '()))
                         (on-resize     (lambda (w h) #f))
			 (event-pump    (lambda () #f))
			 (event-handler (lambda (e) #f)))
  "Run the Willy text editor.
tui - The terminal UI to use.
should-run-p - Condition to determine if the application should continue running.
make-layout - A function that returns the list of windows to render.
on-resize - Function to run when the window is resized.
event-pump - A function that returns the next event or #f if there are none.
event-handler - A function that handles a single event returned by event-pump."
  (let ((frame-limiter (frame-limiter:make-frame-limiter 60))
        (frame-size    (tui:tui-size tui)))
    (on-resize (assoc-ref frame-size 'width)
               (assoc-ref frame-size 'height))
    (while (should-run-p)
           (tui:tui-draw tui (make-layout))
           (frame-limiter:limit-frames frame-limiter)
           (handle-all-events event-pump event-handler)
           (let ((size (tui:tui-size tui)))
             (when (not (equal? size frame-size))
               (on-resize (assoc-ref size 'width)
                          (assoc-ref size 'height))
               (set! frame-size size))))))

(define* (next-event-from-terminal)
  "Get the next terminal event."
  (internal:--next-event-from-terminal))

(define* (list->event-pump events)
  "Convert a list of events into an event pump."
  (let ((next-events (box events)))
    (lambda ()
      (if (pair? (unbox next-events))
          (let ((event (car (unbox next-events)))
                (rest  (cdr (unbox next-events))))
            (set-box! next-events rest)
            event)
          #f))))

(define* (handle-all-events event-pump event-handler)
  "Handle all events returned by event-pump."
  (let handle-single-event ((event (event-pump)))
    (when event
      (event-handler event)
      (handle-single-event (event-pump)))))

(define* (special-key? key)
  "Returns #t if key is not a normal character.

Example special key: <backspace>"
  (> (string-length key) 1))

(define-module (willy core event)
  #:export (
            backspace-key
            list-to-event-pump
            next-event-from-terminal
            run-event-loop
            ))
(use-modules
 ((willy core tui)           #:prefix tui:)
 ((willy core internal)      #:prefix internal:)
 ((willy core frame-limiter) #:prefix frame-limiter:)
 ((srfi srfi-1)))

(define* backspace-key "<backspace>")

(define* (run-event-loop #:key
			 (tui           (tui:make-tui 'test))
			 (should-run-p  (lambda () #f))
			 (make-layout   empty-layout)
			 (event-pump    (lambda () #f))
			 (event-handler (lambda (e) #f)))
  "Run the Willy text editor.
tui - The terminal UI to use.
should-run-p - Condition to determine if the application should continue running.
make-layout - A function that takes a #:width and #:height and returns a list of windows.
event-pump - A function that returns the next event or #f if there are none.
event-handler - A function that handles a single event returned by event-pump."
  (let ((frame-limiter (frame-limiter:make-frame-limiter 60))
        (layout-fn     (lambda ()
                         (let ((size (tui:tui-size tui)))
                           (make-layout #:width  (assoc-ref size 'width)
                                        #:height (assoc-ref size 'height))))))
    (while (should-run-p)
      (handle-all-events event-pump event-handler)
      (tui:tui-draw tui (layout-fn))
      (frame-limiter:limit-frames frame-limiter))))

(define* (next-event-from-terminal)
  "Get the next terminal event."
  (internal:--next-event-from-terminal))

(define* (list-to-event-pump events)
  "Convert a list of events into an event pump."
  (let ((next events)
	(curr #t))
    (lambda ()
      (if curr
	  (begin
	    (if (pair? next)
		(begin
		  (set! curr (car next))
		  (set! next (cdr next)))
		(set! curr #f))))
      curr)))

(define* (handle-all-events event-pump event-handler)
  "Handle all events returned by event-pump."
  (let handle-single-event ((event (event-pump)))
    (if event
	(begin
	  (event-handler event)
	  (handle-single-event (event-pump))))))

(define* (empty-layout #:key width height)
  '())
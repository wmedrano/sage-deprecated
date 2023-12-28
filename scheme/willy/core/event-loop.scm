(define-module (willy core event-loop)
  #:export (
            backspace-key
            run-event-loop
            next-event-from-terminal
            list-to-event-pump)
  #:use-module (srfi srfi-1)
  #:use-module (willy core tui)
  #:use-module (willy core internal))

(define* backspace-key "<backspace>")

(define* (run-event-loop #:key
			 (tui           (make-tui 'test))
			 (should-run-p  (lambda () #f))
			 (make-layout    empty-layout)
			 (event-pump     (lambda () #f))
			 (event-handler  (lambda (e) #f)))
  "Run the Willy text editor.
tui - The terminal UI to use.
should-run-p - Condition to determine if the application should continue running.
make-layout - A function that takes a #:width and #:height and returns a list of windows.
event-pump - A function that returns the next event or #f if there are none.
event-handler - A function that handles a single event returned by event-pump."
  (while (should-run-p)
    (handle-all-events event-pump event-handler)
    (tui-draw tui
	      (let ((size (tui-size tui)))
		(make-layout #:width (assoc-ref size 'width)
			     #:height (assoc-ref size 'height))))))

(define* (next-event-from-terminal)
  "Get the next terminal event."
  (--next-event-from-terminal))

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

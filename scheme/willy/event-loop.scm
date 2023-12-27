(define-module (willy event-loop))
(use-modules (willy tui)
	     (willy internal))

(export run-event-loop)
(define* (run-event-loop #:key tui should-run-p make-layout event-pump event-handler)
  "Run the Willy text editor.
tui - The terminal UI to use.
should-run-p - Condition to determine if the application should continue running.
make-layout - A function that takes a #:width and #:height and returns a layout.
event-pump - A function that returns the next event or #f if there are none.
event-handler - A function that handles a single event returned by event-pump."
  (while (should-run-p)
    (handle-all-events event-pump event-handler)
    (tui-draw tui
	      (let ((size (tui-size tui)))
		(make-layout #:width (assoc-ref size 'width)
			     #:height (assoc-ref size 'height))))))

(export next-event-from-terminal)
(define* (next-event-from-terminal)
  "Get the next terminal event."
  (--next-event-from-terminal))


(define* (handle-all-events event-pump event-handler)
  "Handle all events returned by event-pump."
  (let handle-single-event ((event (event-pump)))
    (if event
	(begin
	  (event-handler event)
	  (handle-single-event (event-pump))))))

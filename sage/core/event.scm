(define-module (sage core event)
  #:export (
            run-event-loop
            events-from-terminal
            ))
(use-modules ((sage core internal) #:prefix internal:)
             ((sage core rope)     #:prefix rope:)
             ((sage core tui)      #:prefix tui:))

(define empty-rope (rope:make-rope))

(define* (run-event-loop #:key
			 (tui          (tui:make-tui #:backend 'test))
                         (draw-params  (always empty-rope))
			 (should-run-p (always #f))
			 (event-queue  (always '()))
			 (on-event     ignore-event)
                         (on-cleanup   no-op))
  "Run the Sage text editor."
  (define* (cleanup-and-reraise-exception e)
    (on-cleanup)
    (raise-exception e))
  (define* (run-event-loop)
    (while (should-run-p)
      (let handle-all-events ((events (event-queue)))
        (for-each on-event events)
        (if (pair? events) (handle-all-events (event-queue))))
      (tui:tui-draw! tui (draw-params)))
    (on-cleanup))
  (with-exception-handler cleanup-and-reraise-exception run-event-loop))

(define* (events-from-terminal)
  "Returns the queued up events in the terminal."
  (internal:events-from-terminal))

(define* (always val)
  "Makes a procedure that takes 0 parameters and returns val."
  (lambda () val))

(define* (no-op)
  "A procedure that does nothing."
  #f)

(define* (ignore-event event)
  "Does nothing with the event."
  #f)

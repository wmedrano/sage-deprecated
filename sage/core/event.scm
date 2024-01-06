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
                         (windows      (always '()))
			 (should-run-p (always #f))
			 (event-queue  (always '()))
			 (on-event     ignore-event)
                         (on-resize    (lambda (width height) #f))
                         (on-cleanup   no-op))
  "Run the Sage text editor."
  (define* (cleanup-and-reraise-exception e)
    (on-cleanup)
    (raise-exception e))
  (define* (run-event-loop)
    (let ((frame-size (tui:tui-draw! tui '())))
      (on-resize (assoc-ref frame-size 'width)
                 (assoc-ref frame-size 'height))
      (while (should-run-p)
        (let handle-all-events ((events (event-queue)))
          (for-each on-event events)
          (if (pair? events) (handle-all-events (event-queue))))
        (let ((new-frame-size (tui:tui-draw! tui (windows))))
          (unless (equal? new-frame-size frame-size)
            (on-resize (assoc-ref new-frame-size 'width)
                       (assoc-ref new-frame-size 'height))
            (set! frame-size new-frame-size))))
      (on-cleanup)))
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

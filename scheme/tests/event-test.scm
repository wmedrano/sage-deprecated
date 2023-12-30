(use-modules (srfi srfi-64)
	     (willy core event)
	     (willy core tui)
             (srfi srfi-111))

(define %test-suite-name "event")
(test-begin %test-suite-name)

(test-equal "list->event-pump first call returns first value"
  "my event"
  ((list->event-pump '("my event" "other event"))))

(test-equal "list->event-pump nth call returns nth value"
  "nth event"
  (let ((event-pump (list->event-pump '("my event" "other event" "nth event"))))
    (event-pump)
    (event-pump)
    (event-pump)))

(test-assert "run-event-loop terminates on false"
  (begin
    (run-event-loop
     #:should-run-p (lambda () #f))
    "Reached here ok."))

(test-assert "run-event-loop does not execute event-loop when should-run-p is false"
  (begin
    (run-event-loop
     #:should-run-p (lambda () #f)
     #:event-handler (lambda () (throw 'not-rechable)))
    "Reached here ok."))

(test-equal "run-event-loop executes event handler"
  "my custom event"
  (let ((last-event (box #f)))
    (run-event-loop
     #:should-run-p (lambda () (not (unbox last-event)))
     #:event-pump (list->event-pump '("my custom event"))
     #:event-handler (lambda (e) (set-box! last-event e)))
    (unbox last-event)))

(test-end %test-suite-name)

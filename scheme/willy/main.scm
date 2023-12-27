(define-module (willy main))

(use-modules (willy buffer)
	     (willy tui))

(define-public (log-message! message)
  "Log a message to the log buffer."
  (buffer-insert-string log-buffer message)
  (buffer-insert-string log-buffer "\n"))

(define-public (quit!)
  "Exit/quit out of Willy."
  (log-message! "Quitting Willy!")
  (set! should-run #f)
  (log-message! (object->string should-run)))

(define-public (run-willy!)
  "Run the Willy text editor."
  (set! should-run #t)
  (set! main-tui (new-tui))
  (while should-run
    (usleep (round (/ 1000000 60)))
    (tui-draw main-tui
	      (let ((size (tui-size main-tui)))
		(make-layout #:width (assoc-ref size 'width)
			     #:height (assoc-ref size 'height))))
    (handle-all-events!))
  (delete-tui main-tui)
  (set! main-tui #f))

(define should-run #f)
(define main-tui #f)
(define main-buffer (new-scratch-buffer))
(define log-buffer (new-buffer))
(define status-bar-buffer (new-buffer "| Willy | Status: OK |"))
;; The layout of the UI.
(define* (make-layout #:key width height)
  `(
    ((buffer . ,main-buffer)
     (x . 0)
     (y . 0)
     (width . ,(/ width 2))
     (height . ,height))
    ((buffer . ,log-buffer)
     (x . ,(/ width 2))
     (y . 0)
     (width . ,(/ width 2))
     (height . ,height))
    ((buffer . ,status-bar-buffer)
     (x . 0)
     (y . ,(- height 1))
     (width . ,width)
     (height . 1))))

(define (handle-all-events!)
  "Handle all events in the event queue."
  (let handle-single-event ((event (next-event)))
    (if event
	(begin
	  (handle-event! event)
	  (handle-single-event (next-event))))))

(define (handle-event! event)
  "Handle a single event."
  (let ((key    (assoc-ref event 'key))
	(ctrl?  (assoc-ref event 'ctrl?))
	(press? (equal? (assoc-ref event 'event-type) 'press)))
    (log-event event)
    (cond
     ((and ctrl? (equal? key "c"))
      (quit!))
     ((and press? (equal? key backspace-key))
      (buffer-pop-char main-buffer))
     ((and press? key (not ctrl?))
      (buffer-insert-string main-buffer key)))))

(define (log-event event)
  "Log an event."
  (log-message!
   (string-concatenate
    `("Event: " ,(object->string event)))))

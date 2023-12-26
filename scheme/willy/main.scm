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
    (tui-draw main-tui (list main-buffer log-buffer))
    (handle-all-events!)
    (usleep 10000))
  (delete-tui main-tui)
  (set! main-tui #f))

;; The buffer that is currently being edited.
(define main-buffer (new-scratch-buffer))
;; A buffer containing log messages for Willy itself.
(define log-buffer (new-buffer))
;; If Willy should continue to run.
(define should-run #f)
;; The main terminal-ui object. This handles drawing to the terminal.
(define main-tui #f)

(define (handle-all-events!)
  "Handle all events in the event queue."
  (let handle-single-event ((event (next-event)))
    (if event
	(begin
	  (handle-event! event)
	  (handle-single-event (next-event))))))

(define (handle-event! event)
  "Handle a single event."
  (let ((char   (assoc-ref event #:char))
	(ctrl?  (assoc-ref event #:ctrl?))
	(press? (equal? (assoc-ref event #:event-type) 'press)))
    (log-event event)
    (cond
     ((and ctrl? (equal? char "c"))
      (quit!))
     ((and press? (equal? char backspace-char))
      (buffer-pop-char main-buffer))
     ((and press? char (not ctrl?))
      (buffer-insert-string main-buffer char)))))

(define (log-event event)
  "Log an event."
  (log-message!
   (string-concatenate
    `("Event: " ,(object->string event)))))

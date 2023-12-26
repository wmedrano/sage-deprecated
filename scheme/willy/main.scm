(define-module (willy main))

(use-modules (willy buffer)
	     (willy tui))

(define log-buffer (new-buffer))
(define-public (log-message! message)
  (buffer-insert-string log-buffer message)
  (buffer-insert-string log-buffer "\n"))

(define should-run #t)
(define-public (quit!)
  (log-message! "I'm quitting!")
  (set! should-run #f)
  (log-message! (object->string should-run)))


(define main-buffer (new-scratch-buffer))
(define main-tui #f)

(define (log-event event)
  (buffer-insert-string log-buffer "Event: ")
  (buffer-insert-string log-buffer (object->string event))
  (buffer-insert-string log-buffer "\n"))

(define (handle-event! event)
  (let ((char   (assoc-ref event #:char))
	(ctrl?  (assoc-ref event #:ctrl?))
	(press? (equal? (assoc-ref event #:event-type) 'press)))
    (buffer-insert-string log-buffer "Debug: ")
    (buffer-insert-string log-buffer
			  (object->string (equal? char "c")))
    (buffer-insert-string log-buffer ", ")
    (log-event event)
    (if (and ctrl? (equal? char "c"))
	(quit!))
    (if (and press? char (not ctrl?) (not (equal? char backspace-char)))
	(buffer-insert-string main-buffer char))
    (if (and press? (equal? char backspace-char))
	(buffer-pop-char main-buffer))))

(define (handle-all-events!)
  (let handle-single-event ((event (next-event)))
    (if event
	(begin
	  (handle-event! event)
	  (handle-single-event (next-event)))
	'())))

(define-public (run-willy!)
  (set! should-run #t)
  (set! main-tui (new-tui))
  (while should-run
    (draw main-tui main-buffer log-buffer)
    (handle-all-events!)
    (usleep 10000))
  (delete-tui main-tui)
  (set! main-tui #f))

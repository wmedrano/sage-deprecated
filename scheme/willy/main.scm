(define-module (willy main))
(use-modules (willy buffer)
	     (willy tui)
	     (willy event-loop)
	     (srfi srfi-1))

(export run-willy!)
(define* (run-willy!)
  "Run the Willy text editor."
  (set! main-tui (make-tui))
  (run-event-loop
   #:tui main-tui
   #:should-run-p (lambda () main-tui)
   #:make-layout make-layout
   #:event-pump next-event-from-terminal
   #:event-handler handle-event!))

(define main-tui #f)
(define* (quit!)
  "Exit/quit out of Willy."
  (delete-tui main-tui)
  (set! main-tui #f))

(define buffer-registry '())
(define* (register-buffer! buffer)
  "Register a new buffer."
  (set! buffer-registry (cons buffer buffer-registry))
  buffer)
(define* (buffers)
  "Get a list of all the buffers. Buffers consist of an alist of:
- 'buffer - The underlying buffer.
- 'name   - The name of the buffer."
  buffer-registry)
(define* (buffer-by-name name)
  "Get a buffer by its name."
  (let ((found (find (lambda (buffer) (equal? (assoc-ref buffer 'name)
					      name))
		     (buffers))))
    (if found
	found
	(register-buffer! `((buffer . ,(make-buffer-content))
			    (name . ,name))))))
(register-buffer!
 `((name   . "main")
   (buffer . ,(make-buffer-content ";; Welcome to Willy! A Scheme based text environment.\n"))))
(register-buffer!
 `((name   . "*status*")
   (buffer . ,(make-buffer-content "| Willy | Status: OK |"))))

(define* (make-layout #:key width height)
  "Define the layout of the ui."
  `(
    ((buffer . ,(assoc-ref (buffer-by-name "main") 'buffer))
     (x . 0)
     (y . 0)
     (width . ,width)
     (height . ,height))
    ((buffer . ,(assoc-ref (buffer-by-name "*status*") 'buffer))
     (x . 0)
     (y . ,(- height 1))
     (width . ,width)
     (height . 1))))

(define (handle-event! event)
  "Handle a single event."
  (let ((key    (assoc-ref event 'key))
	(ctrl?  (assoc-ref event 'ctrl?))
	(press? (equal? (assoc-ref event 'event-type) 'press)))
    (cond
     ((and ctrl? (equal? key "c"))
      (quit!))
     ((and press? (equal? key backspace-key))
      (buffer-content-pop-char (assoc-ref (buffer-by-name "main") 'buffer)))
     ((and press? key (not ctrl?))
      (buffer-content-insert-string (assoc-ref (buffer-by-name "main") 'buffer) key)))))

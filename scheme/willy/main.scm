(define-module (willy main))

(use-modules (willy buffer)
	     (willy tui)
	     (srfi srfi-1))

(export run-willy!)
(define* (run-willy!)
  "Run the Willy text editor."
  (set! main-tui (make-tui))
  (while main-tui
    (tui-draw main-tui
	      (let ((size (tui-size main-tui)))
		(make-layout #:width (assoc-ref size 'width)
			     #:height (assoc-ref size 'height))))
    (handle-all-events!)))

(define* (quit!)
  "Exit/quit out of Willy."
  (delete-tui main-tui)
  (set! main-tui #f))

(define* (buffers)
  "Get a list of all the buffers. Buffers consist of an alist of:
- 'buffer - The underlying buffer.
- 'name   - The name of the buffer."
  buffer-registry)

(export buffer-by-name)
(define* (buffer-by-name name)
  "Get a buffer by its name."
  (let ((found (find (lambda (buffer) (equal? (assoc-ref buffer 'name)
					      name))
		     (buffers))))
    (if found
	found
	(register-buffer! `((buffer . ,(make-buffer-content))
			    (name . ,name))))))

(define* (register-buffer! buffer)
  "Register a new buffer."
  (set! buffer-registry (cons buffer buffer-registry))
  buffer)

(define buffer-registry '())
(define main-tui #f)
(register-buffer!
 `((name   . "main")
   (buffer . ,(make-buffer-content ";; Welcome to Willy! A Scheme based text environment.\n"))))
(register-buffer!
 `((name   . "*status*")
   (buffer . ,(make-buffer-content "| Willy | Status: OK |"))))
;; The layout of the UI.
(define* (make-layout #:key width height)
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
    (cond
     ((and ctrl? (equal? key "c"))
      (quit!))
     ((and press? (equal? key backspace-key))
      (buffer-content-pop-char (assoc-ref (buffer-by-name "main") 'buffer)))
     ((and press? key (not ctrl?))
      (buffer-content-insert-string (assoc-ref (buffer-by-name "main") 'buffer) key)))))

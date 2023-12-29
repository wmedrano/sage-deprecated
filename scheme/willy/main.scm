(define-module (willy main)
<<<<<<< HEAD
  #:export (run!))
(use-modules
 ((willy core buffer)     #:prefix buffer:)
 ((willy core event-loop) #:prefix event-loop:)
 ((willy core log)        #:prefix log:)
 ((willy core tui)        #:prefix tui:)
 ((willy core window)     #:prefix window:)
 ((srfi srfi-1)))

(define* (run!)
  "Run the Willy text editor."
  (set! main-tui (tui:make-tui 'terminal))
  (log:log! "Starting Willy!")
  (event-loop:run-event-loop
   #:tui main-tui
   #:should-run-p (lambda () main-tui)
   #:make-layout make-layout
   #:event-pump event-loop:next-event-from-terminal
   #:event-handler handle-event!)
  ;; Just in case quit was not called and we need to clean up.
  (quit-tui!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main-tui #f)

(define* (quit-tui!)
  "Exit/quit out of Willy."
  (if main-tui
      (begin
        (tui:delete-tui main-tui)
        (set! main-tui #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define buffer-registry
  (list
   (log:log-buffer)
   (buffer:make-buffer
    #:name "main"
    #:string ";; Welcome to Willy! A Scheme based text environment.\n")
   (buffer:make-buffer
    #:name "*status*"
    #:string "Willy | Status: OK")))

=======
  #:export (run-willy!)
  #:use-module (willy buffer)
  #:use-module (willy tui)
  #:use-module (willy event-loop)
  #:use-module (srfi srfi-1))

(define* (run-willy!)
  "Run the Willy text editor."
  (set! main-tui (make-tui))
  (run-event-loop
   #:tui main-tui
   #:should-run-p (lambda () main-tui)
   #:make-layout make-layout
   #:event-pump next-event-from-terminal
   #:event-handler handle-event!)
  ;; Just in case run-event-loop somehow exit without calling quit.
  (quit!))

(define main-tui #f)
(define* (quit!)
  "Exit/quit out of Willy."
  (if main-tui
      (begin
        (delete-tui main-tui)
        (set! main-tui #f))))

(define buffer-registry '())
>>>>>>> main
(define* (register-buffer! buffer)
  "Register a new buffer."
  (set! buffer-registry (cons buffer buffer-registry))
  buffer)
<<<<<<< HEAD

(define* (remove-buffer-by-name! name)
  (let ((b (buffer-by-name name)))
    (if (and b (pair? buffer-registry))
        (set! buffer-registry
              (filter (lambda (b) (not (equal? (buffer:buffer-name b)
                                               name)))
                      buffer-registry)))
    b))

=======
>>>>>>> main
(define* (buffers)
  "Get a list of all the buffers. Buffers consist of an alist of:
- 'buffer - The underlying buffer.
- 'name   - The name of the buffer."
  buffer-registry)
<<<<<<< HEAD

(define* (buffer-by-name name #:key (allow-create? #f))
  "Get a buffer by its name.

If the buffer does not exist and allow-create? is #t, then a new
buffer will be created and returned."
  (let ((found (find (lambda (buffer) (equal? (buffer:buffer-name buffer)
                                              name))
                     (buffers))))
    (or found
        (if allow-create?
            (register-buffer! (buffer:make-buffer #:name name))
            #f))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (make-layout #:key width height)
  "Define the layout of the ui."
  (list
   (window:make-window #:buffer   (buffer-by-name "main")
                       #:features '((line-numbers   . #t)
                                    (highlight-line . #t)
                                    (cursor         . #t))
                       #:x        0
                       #:y        0
                       #:width    (/ width 2)
                       #:height   height)
   (window:make-window #:buffer   (log:log-buffer)
                       #:features '((line-numbers   . #t))
                       #:x        (/ width 2)
                       #:y        0
                       #:width    (/ width 2)
                       #:height   height)
   (window:make-window #:buffer   (buffer-by-name "*status*")
                       #:features '((border . #t))
                       #:x        0
                       #:y        (- height 3)
                       #:width    width
                       #:height   3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (handle-event! event)
  "Handle a single event."
  (let ((key    (assoc-ref event 'key))
    (ctrl?  (assoc-ref event 'ctrl?))
    (alt?   (assoc-ref event 'alt?))
    (press? (equal? (assoc-ref event 'event-type) 'press)))
    (cond
     ((and ctrl? (not alt?) (equal? key "c"))
      (quit-tui!))
     ((and press? (equal? key event-loop:backspace-key) (not ctrl?) (not alt?))
      (buffer:buffer-pop-char (buffer-by-name "main")))
     ((and press? key (not ctrl?) (not alt?))
      (buffer:buffer-insert-string (buffer-by-name "main") key))
     (else (log:log! "Unhandled event " (object->string event))))))
=======
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
>>>>>>> main

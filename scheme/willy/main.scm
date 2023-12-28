(define-module (willy main)
  #:export (run-willy!)
  #:use-module (willy core buffer)
  #:use-module (willy core event-loop)
  #:use-module (willy core tui)
  #:use-module (srfi srfi-1))
(use-modules ((willy core window) #:prefix window:))

(define* (run-willy!)
  "Run the Willy text editor."
  (set! main-tui (make-tui 'terminal))
  (run-event-loop
   #:tui main-tui
   #:should-run-p (lambda () main-tui)
   #:make-layout make-layout
   #:event-pump next-event-from-terminal
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
        (delete-tui main-tui)
        (set! main-tui #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define buffer-registry
  (list
   (make-buffer #:name "main"
                #:string ";; Welcome to Willy! A Scheme based text environment.\n")
   (make-buffer #:name "*status*"
                #:string "Willy | Status: OK")))

(define* (register-buffer! buffer)
  "Register a new buffer."
  (set! buffer-registry (cons buffer buffer-registry))
  buffer)

(define* (remove-buffer-by-name! name)
  (let ((b (buffer-by-name name)))
    (if (and b (pair? buffer-registry))
        (set! buffer-registry
              (filter (lambda (b) (not (equal? (buffer-name b)
                                               name)))
                      buffer-registry)))
    b))

(define* (buffers)
  "Get a list of all the buffers. Buffers consist of an alist of:
- 'buffer - The underlying buffer.
- 'name   - The name of the buffer."
  buffer-registry)

(define* (buffer-by-name name #:key (allow-create? #f))
  "Get a buffer by its name.

If the buffer does not exist and allow-create? is #t, then a new
buffer will be created and returned."
  (let ((found (find (lambda (buffer) (equal? (buffer-name buffer)
                                              name))
                     (buffers))))
    (or found
        (if allow-create?
            (register-buffer! (make-buffer #:name name))
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
                       #:width    width
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
    (press? (equal? (assoc-ref event 'event-type) 'press)))
    (cond
     ((and ctrl? (equal? key "c"))
      (quit-tui!))
     ((and press? (equal? key backspace-key))
      (buffer-pop-char (buffer-by-name "main")))
     ((and press? key (not ctrl?))
      (buffer-insert-string (buffer-by-name "main") key)))))

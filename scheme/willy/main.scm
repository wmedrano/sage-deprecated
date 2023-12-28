(define-module (willy main)
  #:export (run-willy!)
  #:use-module (willy buffer)
  #:use-module (willy tui)
  #:use-module (willy event-loop)
  #:use-module (srfi srfi-1))

(define* (run-willy!)
  "Run the Willy text editor."
  (run-event-loop
   #:tui (start-tui! (make-tui))
   #:should-run-p tui-is-active?
   #:make-layout make-layout
   #:event-pump next-event-from-terminal
   #:event-handler handle-event!)
  ;; Just in case run-event-loop somehow exited without calling quit.
  (quit-tui!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (make-layout #:key width height)
  "Define the layout of the ui."
  `(
    ((buffer   . ,(buffer-by-name "main"))
     (features . ((line-numbers   . #t)
                  (highlight-line . #t)
                  (cursor         . #t)))
     (x        . 0)
     (y        . 0)
     (width    . ,width)
     (height   . ,height))
    ((buffer   . ,(buffer-by-name "*status*"))
     (features . ((border . #t)))
     (x        . 0)
     (y        . ,(- height 3))
     (width    . ,width)
     (height   . 3))))

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

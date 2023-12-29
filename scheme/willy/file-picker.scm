(define-module (willy file-picker)
  #:export (
            file-picker-buffer
            file-picker-handle-event!
            file-picker-layout
            find-files
            ))
(use-modules ((willy core window) #:prefix window:)
             ((willy core log)    #:prefix log:)
             ((willy core buffer) #:prefix buffer:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define file-picker-buffer #f)
(define* (file-picker-layout windows #:key width height)
  (if file-picker-buffer
      (cons (window:make-window #:buffer   file-picker-buffer
                                #:features '((border . #t))
                                #:x        (* width  1/6)
                                #:y        (* height 1/6)
                                #:width    (* width  4/6)
                                #:height   (* height 4/6))
            windows)
      windows))

(use-modules ((ice-9 ftw) #:prefix ftw:))
(define* (find-files)
  (string-join (cons "Open File: " (ftw:scandir (getcwd)))
               "\n "))

(define* (file-picker-handle-event! event)
  (let ((key    (assoc-ref event 'key))
        (ctrl?  (assoc-ref event 'ctrl?))
        (alt?   (assoc-ref event 'alt?))
        (press? (equal? (assoc-ref event 'event-type) 'press)))
    (cond
     ((and (not file-picker-buffer) press? ctrl? (not alt?) (equal? key "o"))
      (begin
        (set! file-picker-buffer (buffer:make-buffer #:name "*file-picker*"
                                                     #:string (find-files)))
        #f))
     ((not file-picker-buffer) event)
     ((and press? (equal? key "<esc>"))
      (begin
        (set! file-picker-buffer #f)
        #f))
     ((and press? (equal? key "<backspace>"))
      (begin
        (buffer:buffer-pop-char file-picker-buffer 0)
        #f))
     ((or (string-contains key "<") (equal? key "\n") (equal? key "\t"))
      #f)
     ((and press? (not ctrl?) (not alt?))
      (begin
        (buffer:buffer-insert-string file-picker-buffer key 0)
        #f))
     (else event))))

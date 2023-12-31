(define-module (willy open-file)
  #:export (open-file!))
(use-modules ((willy state)       #:prefix state:)
             ((willy core buffer) #:prefix buffer:)
             ((willy core window) #:prefix window:)
             ((ice-9 ftw)         #:prefix ftw:)
             ((srfi srfi-1))
             ((srfi srfi-2))
             ((srfi srfi-111)))

(define* (open-file!)
  (when (unbox open-file-window)
    (open-file-close!))
  (add-hook! state:event-hook open-file-handle-event)
  (set-box! open-file-files (ftw:scandir "./"))
  (let* ((frame-size (unbox state:frame-size))
         (frame-width (assoc-ref frame-size 'width))
         (frame-height (assoc-ref frame-size 'height))
         (buffer (buffer:make-buffer #:name "open-file-dialog"))
         (window (window:make-window #:buffer   buffer
                                     #:features '((border . #t))
                                     #:x        (* frame-width 1/8)
                                     #:y        (* frame-height 1/16)
                                     #:width    (* frame-width 6/8)
                                     #:height   (* frame-height 14/16))))
    (set-box! open-file-window window)
    (open-file-set-query! "")
    (state:add-window! window #:set-focus? #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-file-window (box #f))
(define open-file-files (box '()))
(define open-file-query (box ""))

(define* (open-file-set-query! query)
  (set-box! open-file-query query)
  (let* ((query-display (string-concatenate (list "Open file: " (unbox open-file-query))))
         (buffer-str    (string-join (cons query-display (unbox open-file-files))
                                     "\n  ")))
    (buffer:buffer-set-string (window:window-buffer (unbox open-file-window))
                              buffer-str)))

(define* (open-file-handle-event event)
  (and-let* ((q (unbox open-file-query))
             (window (unbox open-file-window))
             (key (assoc-ref event 'key)))
    (cond
     ((equal? key "\n") (open-file-close!))
     ((equal? key "<esc>") (open-file-close!))
     ((equal? key "<backspace>")
      (unless (equal? (string-length q) 0)
        (open-file-set-query! (substring q 0 (- (string-length q) 1)))))
     (else (open-file-set-query! (string-concatenate
                                  (list q key)))))))

(define* (open-file-close!)
  (remove-hook! state:event-hook open-file-handle-event)
  (state:retain-windows! (lambda (w)
                           (not
                            (eq? (unbox open-file-window)
                                 w))))
  (let* ((is-editable? (lambda (w) (window:window-feature w 'editable?)))
         (any-editable-window (find is-editable?
                                    (state:windows))))
    (state:set-focused-window! (or any-editable-window
                                   (car (state:windows))))
    (set-box! open-file-window #f)))

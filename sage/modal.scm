(define-module (sage modal)
  #:export (run-modal! open-file!))
(use-modules
 ((sage core buffer) #:prefix buffer:)
 ((sage core rope)   #:prefix rope:)
 ((sage core window) #:prefix window:)
 ((sage core rect)   #:prefix rect:)
 ((sage state)       #:prefix state:)
 ((ice-9 ftw)        #:prefix ftw:)
 ((ice-9 textual-ports) #:select (get-string-all))
 (srfi srfi-1))

(define* (modal-area frame-width frame-height)
  (rect:make-rect (* frame-width  1/8)
                  (* frame-height 1/8)
                  (* frame-width  6/8)
                  (* frame-height 6/8)))

(define* (run-modal! #:key prompt items on-select)
  (define query "")
  (define matches items)
  (define target-window (state:focused-window))
  (define rope   (rope:make-rope))
  (define buffer (buffer:make-buffer #:rope rope))
  (define window (window:make-window
                  #:buffer   buffer
                  #:area     (modal-area (state:frame-width) (state:frame-height))
                  #:features '((border? . #t)
                               (title   . "Open File"))))
  (define cleanup-hook (make-hook))
  (define (cleanup!)
    (run-hook cleanup-hook)
    (reset-hook! cleanup-hook))
  (define (set-query! q)
    (set! query q)
    (set! matches (%filter-items-by-query items query))
    (rope:rope-set-string! rope (%make-modal-string prompt query matches)))
  (set-query! "")
  (define (select!)
    (cleanup!)
    (when (pair? matches)
      (on-select (car matches))))
  (define (handle-event! event)
    (let* ((key-code  (assoc-ref event 'key-code))
           (ctrl?     (assoc-ref event 'ctrl?))
           (alt?      (assoc-ref event 'alt?))
           (mod?      (or ctrl? alt?))
           (no-mod?   (not mod?)))
      (when no-mod?
        (cond
         ((equal? key-code #\newline)
          (select!))
         ((char? key-code)
          (set-query! (string-append query
                                     (list->string (list key-code)))))
         ((and (equal? key-code "<backspace>") (> (string-length query) 0))
          (set-query! (substring query
                                 0
                                 (- (string-length query) 1))))
         ((equal? key-code "<esc>")
          (cleanup!))))))
  (add-hook! state:event-hook handle-event!)
  (add-hook! cleanup-hook (lambda ()
                            (remove-hook! state:event-hook handle-event!)))
  (define (handle-resize! width height)
    (window:window-set-area! window (modal-area width height)))
  (add-hook! state:resize-hook handle-resize!)
  (add-hook! cleanup-hook (lambda () (remove-hook! state:resize-hook handle-resize!)))
  (state:add-window! window #:set-focus? #t)
  (add-hook! cleanup-hook (lambda ()
                            (state:remove-window! window)
                            (state:set-focused-window! target-window))))

(define (language-for-path path)
  "Get the language that should be used for the given path."
  (cond
   ((string-suffix? ".rs" path)
    "rust")
   ((string-suffix? ".scm" path)
    "scheme")
   (else
    "")))

(define %debug-var "")

(define* (open-file!)
  (define (on-select! file-path)
    (let ((window   (state:focused-window))
          (language (language-for-path file-path))
          (text     (call-with-input-file file-path get-string-all)))
      (window:window-set-feature! window 'title file-path)
      (window:window-set-buffer!
       window
       (buffer:make-buffer #:file-path file-path
                           #:cursor 0
                           #:rope (rope:make-rope #:text text
                                                  #:language language)))))
  (define (deferred-on-select! file-path)
    (state:add-task! (lambda () (on-select! file-path))))
  (run-modal! #:prompt "Open File: "
              #:items (%discover-files ".")
              #:on-select deferred-on-select!))

(define* (%filter-items-by-query items query)
  (define (item-matches-all-subqueries? item subqueries)
    (every (lambda (subquery) (string-contains item subquery))
           subqueries))
  (let* ((subqueries (string-split query #\space)))
    (filter (lambda (item) (item-matches-all-subqueries? item subqueries))
            items)))

(define* (%make-modal-string prompt query matches)
  (define (format-item item)
    (string-concatenate (list " " item)))
  (let ((prompt-line (string-concatenate (list prompt query)))
        (item-lines  (map format-item matches)))
    (string-join (cons prompt-line
                       (if (null? item-lines) '("") item-lines))
                 "\n")))

(define* (%discover-files root-dir)
  (let* ((file-name root-dir)
         (init      '())
         (enter?    (lambda (path stat result)
                      (not (member (basename path) '(".git" ".svn" "CVS" "target")))))
         (down      (lambda (path stat result) result))
         (up        (lambda (path stat result) result))
         (skip      (lambda (path stat result) result))
         (error     (lambda (path stat errno result) result))
         (leaf      (lambda (path stat result)
                      (cons path result))))
    (ftw:file-system-fold enter? leaf down up skip error init file-name)))

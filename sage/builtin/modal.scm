(define-module (sage builtin modal)
  #:export (run-modal! open-file! switch-buffer! select-command!))
(use-modules
 ((sage core buffer) #:prefix buffer:)
 ((sage core rope)   #:prefix rope:)
 ((sage core window) #:prefix window:)
 ((sage core rect)   #:prefix rect:)
 ((sage state)       #:prefix state:)
 ((ice-9 ftw)        #:prefix ftw:)
 ((ice-9 textual-ports) #:select (get-string-all))
 (srfi srfi-1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base modal framework.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (modal-area frame-width frame-height)
  (rect:make-rect (* frame-width  1/8)
                  (* frame-height 1/8)
                  (* frame-width  6/8)
                  (* frame-height 6/8)))

(define* (run-modal! #:key prompt items on-select (item->string identity))
  (define* (%filter-items-by-query items query)
    (define (item-matches-all-subqueries? item-string subqueries)
      (every (lambda (subquery) (string-contains item-string subquery))
             subqueries))
    (let* ((subqueries (string-split query #\space)))
      (filter (lambda (item) (item-matches-all-subqueries? (item->string item) subqueries))
              items)))
  (define* (%make-modal-string prompt query matches)
    (define (format-item item)
      (string-concatenate (list " " (item->string item))))
    (let ((prompt-line (string-concatenate (list prompt query)))
          (item-lines  (map format-item matches)))
      (string-join (cons prompt-line
                         (if (null? item-lines) '("") item-lines))
                   "\n")))
  (define query "")
  (define matches items)
  (define target-window (state:focused-window))
  (define rope   (rope:make-rope))
  (define buffer (buffer:make-buffer #:name "**modal**" #:rope rope))
  (define window (window:make-window
                  #:buffer   buffer
                  #:area     (modal-area (state:frame-width) (state:frame-height))
                  #:features '((border? . #t))))
  (define cleanup-hook (make-hook))
  (define (cleanup!)
    (run-hook cleanup-hook)
    (reset-hook! cleanup-hook))
  (define (set-query! q)
    (set! query q)
    (set! matches (%filter-items-by-query items query))
    (rope:rope-set-string! rope (%make-modal-string prompt query matches))
    (buffer:buffer-set-cursor! buffer
                               (+ (string-length prompt) (string-length query))))
  (set-query! "")
  (define (select!)
    (cleanup!)
    (when (pair? matches)
      (on-select (car matches))))
  (define (handle-event! w b event)
    (when (and (eq? window w) (eq? buffer b))
      (let* ((key-code  (assoc-ref event 'key-code))
             (ctrl?     (assoc-ref event 'ctrl?))
             (alt?      (assoc-ref event 'alt?))
             (mod?      (or ctrl? alt?))
             (no-mod?   (not mod?)))
        (when no-mod?
          (cond
           ((equal? key-code #\newline)
            (when (pair? matches) (select!)))
           ((char? key-code)
            (set-query! (string-append query
                                       (list->string (list key-code)))))
           ((and (equal? key-code "<backspace>") (> (string-length query) 0))
            (set-query! (substring query
                                   0
                                   (- (string-length query) 1))))
           ((equal? key-code "<esc>")
            (cleanup!)))))))
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
                            (state:remove-buffer! buffer)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open file.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (open-file!)
  (define (on-select! file-path)
    (let* ((window   (state:focused-window))
           (language (language-for-path file-path))
           (text     (call-with-input-file file-path get-string-all))
           (buffer   (buffer:make-buffer
                      #:name      file-path
                      #:file-path file-path
                      #:cursor 0
                      #:rope (rope:make-rope #:text text
                                             #:language language))))
      (state:add-buffer! buffer)
      ;; TODO: Move this to a hook.
      (window:window-set-feature! window 'title file-path)
      (window:window-set-buffer!
       window
       buffer)))
  (define (deferred-on-select! file-path)
    (state:add-task! (lambda () (on-select! file-path))))
  (run-modal! #:prompt "Open File: "
              #:items (%discover-files ".")
              #:on-select deferred-on-select!))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switch Buffer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (switch-buffer!)
  (define (on-select! buffer)
    (let* ((window   (state:focused-window)))
      (window:window-set-buffer! window buffer)))
  (define (deferred-on-select! buffer)
    (state:add-task! (lambda () (on-select! buffer))))
  (define (buffer->string buffer)
    (let ((name (buffer:buffer-name buffer)))
      (if (equal? name "")
          (buffer:buffer-file-path buffer)
          name)))
  (run-modal! #:prompt "Switch Buffer: "
              #:items (state:buffers)
              #:on-select deferred-on-select!
              #:item->string buffer->string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select a command.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (select-command! command-to-proc)
  "Run a modal that selects a command. command-to-proc must be an
alist that maps a string to a procedure."
  (define (on-select! cmd-to-proc)
    ((cdr cmd-to-proc)))
  (define (deferred-on-select! cmd-to-proc)
    (on-select! cmd-to-proc))
  (run-modal! #:prompt "Run Command: "
              #:items command-to-proc
              #:on-select deferred-on-select!
              #:item->string car))

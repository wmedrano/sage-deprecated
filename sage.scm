#!/usr/bin/guile -s
!#
(add-to-load-path ".")
(use-modules
 ((sage core rect)     #:prefix rect:)
 ((sage core buffer)   #:prefix buffer:)
 ((sage core rope)     #:prefix rope:)
 ((sage core tui)      #:prefix tui:)
 ((sage core window)   #:prefix window:)
 ((sage builtin modal) #:prefix modal:)
 ((sage state)         #:prefix state:)
 ((sage core log)      #:prefix log:)
 (srfi srfi-1))

(define* (resize-windows! width height)
  (window:window-set-area! (first (state:windows))
                           (rect:make-rect 0 (- height 1) width 1))
  (window:window-set-area! (second (state:windows))
                           (rect:make-rect 0 0 width (- height 1))))

(define* (switch-to-log-buffer!)
  (window:window-set-buffer! (state:focused-window)
                             (log:log-buffer)))

(define* (handle-editing-events! window buffer event)
  "Handle all events."
  (let* ((editable? (window:window-feature window 'editable?))
         (cursor    (buffer:buffer-cursor buffer))
         (rope      (buffer:buffer-rope buffer))
         (key-code  (assoc-ref event 'key-code))
         (ctrl?     (assoc-ref event 'ctrl?))
         (alt?      (assoc-ref event 'alt?))
         (mod?      (or ctrl? alt?))
         (no-mod?   (not mod?)))
    (when (and cursor editable? no-mod?)
      (cond
       ((equal? key-code #\tab)
        (buffer:buffer-insert-at-cursor! buffer "    "))
       ((char? key-code)
        (buffer:buffer-insert-at-cursor! buffer key-code))
       ((equal? key-code "<backspace>")
        (buffer:buffer-delete-char-before-cursor! buffer))
       ((equal? key-code "<up>")
        (buffer:buffer-scroll-row! buffer -1))
       ((equal? key-code "<down>")
        (buffer:buffer-scroll-row! buffer 1))
       ((and (equal? key-code "<left>") (> cursor 0))
        (buffer:buffer-scroll-column! buffer -1))
       ((and (equal? key-code "<right>") (< cursor (rope:rope-length rope)))
        (buffer:buffer-scroll-column! buffer 1))))))

(define* (handle-special-events! window buffer event)
  "Handle all events."
  (let* ((editable? (window:window-feature window 'editable?))
         (cursor    (buffer:buffer-cursor buffer))
         (rope      (buffer:buffer-rope buffer))
         (key-code  (assoc-ref event 'key-code))
         (ctrl?     (assoc-ref event 'ctrl?))
         (alt?      (assoc-ref event 'alt?))
         (mod?      (or ctrl? alt?))
         (no-mod?   (not mod?)))
    (when (and ctrl? (not alt?))
      (cond
       ((equal? key-code #\p) (modal:select-command!
                               `(("open-file"     . ,modal:open-file!)
                                 ("switch-buffer" . ,modal:switch-buffer!)
                                 ("view-logs"     . ,switch-to-log-buffer!)
                                 ("clear-logs"    . ,log:clear-logs!)
                                 ("quit"          . ,state:quit!))))
       ((equal? key-code #\o) (modal:open-file!))
       ((equal? key-code #\c) (state:quit!))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (initialize!)
  ;; Resize
  (reset-hook! state:resize-hook)
  (add-hook! state:resize-hook resize-windows!)
  (add-hook! state:resize-hook (lambda (width height)
                                 (log:log! "Resized window to " width " " height ".")))

  ;; Quit
  (reset-hook! state:quit-hook)

  ;; Buffers
  (add-hook! state:add-buffer-hook (lambda (buffer)
                                     (unless (equal? "" (buffer:buffer-file-path buffer))
                                       (add-hook! (buffer:buffer-event-hook buffer) handle-editing-events!))
                                     (add-hook! (buffer:buffer-event-hook buffer) handle-special-events!)))
  (for-each state:remove-buffer! (state:buffers))
  (state:add-buffer! (log:log-buffer))

  ;; Create initial windows.
  (for-each state:remove-window! (state:windows))
  (state:add-window!
   (window:make-window #:buffer (buffer:make-buffer
                                 #:name "**scratch**"
                                 #:cursor 0
                                 #:rope   (rope:make-rope #:text     ";; Welcome to Sage!\n\n"
                                                          #:language "scheme"))
                       #:area     (rect:make-rect 0 1 80 24)
                       #:features '((editable?     . #t)
                                    (border?       . #t)
                                    (title         . "*scratch*")
                                    (line-numbers? . #t)))
   #:set-focus? #t)
  (state:add-window!
   (window:make-window
    #:buffer (buffer:make-buffer #:name "**status-bar**"
                                 #:rope (rope:make-rope #:text "Sage | Status OK"))
    #:area     '(rect:make-rect 0 0 0 0))))

(define* (run!)
  (state:run! (tui:make-tui #:backend 'terminal)))

(define (main . args)
  "Run sage."
  (initialize!)
  (run!))

#!/usr/bin/guile -s
!#
(add-to-load-path ".")
(use-modules
 ((sage builtin editor)   #:prefix editor:)
 ((sage builtin modal)    #:prefix modal:)
 ((sage builtin tutorial) #:prefix tutorial:)
 ((sage core buffer)      #:prefix buffer:)
 ((sage core log)         #:prefix log:)
 ((sage core rect)        #:prefix rect:)
 ((sage core rope)        #:prefix rope:)
 ((sage core tui)         #:prefix tui:)
 ((sage core window)      #:prefix window:)
 ((sage state)            #:prefix state:)
 (srfi srfi-1))

(define* (resize-windows! width height)
  (window:window-set-area! (first (state:windows))
                           (rect:make-rect 0 (- height 1) width 1))
  (window:window-set-area! (second (state:windows))
                           (rect:make-rect 0 0 width (- height 1))))

(define* (switch-to-log-buffer!)
  (window:window-set-buffer! (state:focused-window)
                             (log:log-buffer)))

(define* (handle-special-events! window buffer event)
  "Handle all events."
  (let* ((key-code  (assoc-ref event 'key-code))
         (ctrl?     (assoc-ref event 'ctrl?))
         (alt?      (assoc-ref event 'alt?)))
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
                                       (add-hook! (buffer:buffer-event-hook buffer) editor:edit-buffer!))
                                     (add-hook! (buffer:buffer-event-hook buffer) handle-special-events!)))
  (for-each state:remove-buffer! (state:buffers))
  (state:add-buffer! (log:log-buffer))

  ;; Create initial windows.
  (for-each state:remove-window! (state:windows))
  (state:add-window!
   (window:make-window #:buffer   (tutorial:make-tutorial-buffer)
                       #:area     (rect:make-rect 0 1 80 24)
                       #:features '((border?       . #t)
                                    (title         . "*tutorial*")
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

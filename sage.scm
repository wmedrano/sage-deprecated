#!/usr/bin/guile -s
!#
(add-to-load-path ".")
(use-modules
 ((sage core rect)   #:prefix rect:)
 ((sage core buffer) #:prefix buffer:)
 ((sage core rope)   #:prefix rope:)
 ((sage core tui)    #:prefix tui:)
 ((sage core window) #:prefix window:)
 ((sage modal)       #:prefix modal:)
 ((sage state)       #:prefix state:)
 (srfi srfi-1))

(define* (resize-windows! width height)
  (window:window-set-area! (first (state:windows))
                           (rect:make-rect 0 (- height 1) width 1))
  (window:window-set-area! (second (state:windows))
                           (rect:make-rect 0 0 width (- height 1))))

(define* (handle-events! event)
  "Handle all ctrl keys."
  (let* ((window    (state:focused-window))
         (editable? (window:window-feature window 'editable?))
         (buffer    (window:window-buffer window))
         (rope      (buffer:buffer-rope buffer))
         (key-code  (assoc-ref event 'key-code))
         (ctrl?     (assoc-ref event 'ctrl?))
         (alt?      (assoc-ref event 'alt?))
         (mod?      (or ctrl? alt?))
         (no-mod?   (not mod?)))
    (when (and editable? no-mod?)
      (cond
       ((char? key-code) (rope:rope-append! rope key-code))
       ((equal? key-code "<backspace>") (rope:rope-pop! rope))))
    (when (and ctrl? (not alt?))
      (cond
       ((equal? key-code #\c) (state:quit!))
       ((equal? key-code #\o) (modal:open-file!))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (initialize!)
  (reset-hook! state:event-hook)
  (reset-hook! state:resize-hook)
  (reset-hook! state:quit-hook)
  (for-each state:remove-window! (state:windows))

  (state:add-window!
   (window:make-window #:buffer (buffer:make-buffer
                                 #:rope (rope:make-rope #:text     ";; Welcome to Sage!\n\n"
                                                        #:language "scheme"))
                       #:area     (rect:make-rect 0 1 80 24)
                       #:features '((editable?     . #t)
                                    (border?       . #t)
                                    (title         . "*scratch*")
                                    (line-numbers? . #t)
                                    (cursor?       . #t)))
   #:set-focus? #t)
  (state:add-window!
   (window:make-window
    #:buffer (buffer:make-buffer #:rope (rope:make-rope #:text "Sage | Status OK"))
    #:area     '(rect:make-rect 0 0 0 0)))
  (add-hook! state:event-hook handle-events!)
  (add-hook! state:resize-hook resize-windows!))

(define (main . args)
  "Run sage."
  (initialize!)
  (state:run! (tui:make-tui #:backend 'terminal)))

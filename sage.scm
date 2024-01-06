#!/usr/bin/guile -s
!#
(add-to-load-path ".")
(use-modules ((sage core tui)    #:prefix tui:)
             ((sage core window) #:prefix window:)
             ((sage state)       #:prefix state:)
             ((sage core event)  #:prefix event:)
             ((sage core rope)   #:prefix rope:)
             (srfi srfi-1))

(define* (resize-windows! width height)
  (window:window-set-position! (first (state:windows))
                               #:x 0
                               #:y (- height 1)
                               #:width width
                               #:height 1)
  (window:window-set-position! (second (state:windows))
                               #:x 0
                               #:y 0
                               #:width width
                               #:height (- height 1)))

(define* (handle-events! event)
  "Handle all ctrl keys."
  (let* ((rope     (window:window-rope (state:focused-window)))
         (key-code (assoc-ref event 'key-code))
         (ctrl?    (assoc-ref event 'ctrl?))
         (alt?     (assoc-ref event 'alt?))
         (mod?     (or ctrl? alt?))
         (no-mod?  (not mod?)))
    (when (and no-mod? (char? key-code))
      (rope:rope-append! rope key-code))
    (when (and no-mod? (equal? key-code "<backspace>"))
      (rope:rope-pop! rope))
    (when (and ctrl? (equal? key-code #\c))
      (state:quit!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (initialize!)
  (reset-hook! state:event-hook)
  (reset-hook! state:resize-hook)
  (reset-hook! state:quit-hook)
  (for-each state:remove-window! (state:windows))

  (state:set-tui! (tui:make-tui #:backend 'terminal))
  (state:add-window!
   (window:make-window #:rope (rope:make-rope #:text     "fn main() {\n}"
                                              #:language "rust")
                       #:position (window:make-position 0 1 80 24))
   #:set-focus? #t)
  (state:add-window!
   (window:make-window
    #:rope (rope:make-rope #:text "Sage | Status OK")
    #:position '(window:make-position 0 0 0 0)))
  (add-hook! state:event-hook handle-events!)
  (add-hook! state:resize-hook resize-windows!))

(define (main . args)
  "Run sage."
  (initialize!)
  (event:run-event-loop #:tui          (state:tui)
                        #:windows      state:windows
                        #:should-run-p state:tui
                        #:event-queue  event:events-from-terminal
                        #:on-event     state:run-event-hook
                        #:on-resize    state:run-resize-hook
                        #:on-cleanup   state:quit!))

(main)

#!/usr/bin/guile -s
!#
(add-to-load-path ".")
(use-modules ((sage core tui)   #:prefix tui:)
             ((sage core event) #:prefix event:)
             ((sage core rope)  #:prefix rope:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TUI.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tui #f)
(define* (quit!)
  (when tui
    (tui:delete-tui! tui)
    (set! tui #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define main-rope (rope:make-rope #:language "rust"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define event-hook (make-hook 1))

(define* (run-event-hook event)
  "Run the event hook with the given event."
  (run-hook event-hook event))

(define* (handle-events! event)
  "Handle all ctrl keys."
  (let* ((key-code (assoc-ref event 'key-code))
         (ctrl?    (assoc-ref event 'ctrl?))
         (alt?     (assoc-ref event 'alt?))
         (mod?     (or ctrl? alt?))
         (no-mod?  (not mod?)))
    (when (and no-mod? (char? key-code))
      (rope:rope-append! main-rope key-code))
    (when (and no-mod? (equal? key-code "<backspace>"))
      (rope:rope-pop! main-rope))
    (when (and ctrl? (equal? key-code #\c))
      (quit!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (initialize!)
  (quit!)
  (set! tui (tui:make-tui #:backend 'terminal))
  (reset-hook! event-hook)
  (add-hook! event-hook handle-events!))

(define (main . args)
  "Run sage."
  (initialize!)
  (event:run-event-loop #:tui tui
                        #:draw-params  (lambda () main-rope)
                        #:should-run-p (lambda () tui)
                        #:event-queue  event:events-from-terminal
                        #:on-event     run-event-hook
                        #:on-cleanup   quit!))
(main)

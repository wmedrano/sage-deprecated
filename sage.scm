(add-to-load-path ".")
(use-modules ((sage core tui) #:prefix tui:))

(define (main . args)
  "Run sage."
  (let ((tui (tui:make-tui #:backend 'terminal)))
    (sleep 3)
    (tui:delete-tui! tui)
    (sleep 3)))

(main)

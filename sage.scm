#!/usr/bin/guile -s
!#
(add-to-load-path ".")
(use-modules ((sage core tui)  #:prefix tui:)
             ((sage core rope) #:prefix rope:))

(define (main . args)
  "Run sage."
  (let ((tui    (tui:make-tui #:backend 'terminal))
        (buffer (rope:make-rope #:text ";; Welcome to Sage!")))
    (tui:tui-draw! tui buffer)
    (sleep 1)
    (rope:rope-insert! buffer
                       (rope:rope-byte-length buffer)
                       "\nWill exit soon.")
    (tui:tui-draw! tui buffer)
    (sleep 1)
    (tui:delete-tui! tui)
    (sleep 1)))

(main)

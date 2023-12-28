(define-module (willy core window)
  #:export (make-window))

(define* (make-window #:key
                      buffer
                      (features '())
                      (x        0)
                      (y        0)
                      (width    0)
                      (height   0))
  `(
    (buffer   . ,buffer)
    (features . ,features)
    (x        . ,x)
    (y        . ,y)
    (width    . ,width)
    (height   . ,height)
    ))

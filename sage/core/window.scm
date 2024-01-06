(define-module (sage core window)
  #:export (
            make-window
            window-rope
            window-set-position!
            make-position
            ))
(use-modules ((sage core rope) #:prefix rope:))

(define* (make-window #:key
                      (rope (rope:make-rope))
                      (position (make-position 0 0 0 0)))
  "Make a new window."
  `((rope     . ,rope)
    (position . ,position)))

(define* (window-rope window)
  "Get the rope associated with the window."
  (assoc-ref window 'rope))

(define* (window-set-position! window #:key x y width height)
  "Set the position of the window."
  (assoc-set! window 'position (make-position x y width height)))

(define* (make-position x y width height)
  "Make a new position alist."
  `((x . ,x) (y . ,y) (width . ,width) (height . ,height)))

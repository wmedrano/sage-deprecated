(define-module (sage core window)
  #:export (
            make-window
            window-set-rope!
            window-rope
            window-set-position!
            make-position
            window-feature
            window-features
            ))
(use-modules ((sage core rope) #:prefix rope:))

(define* (make-window #:key
                      (rope (rope:make-rope))
                      (position (make-position 0 0 0 0))
                      (features '()))
  "Make a new window."
  `((rope     . ,rope)
    (position . ,position)
    (features . ,features)))

(define* (window-rope window)
  "Get the rope associated with the window."
  (assoc-ref window 'rope))

(define* (window-set-rope! window rope)
  "Set the window's rope."
  (assoc-set! window 'rope rope))

(define* (window-set-position! window position)
  "Set the position of the window."
  (assoc-set! window 'position position))

(define* (make-position x y width height)
  "Make a new position alist."
  `((x . ,x) (y . ,y) (width . ,width) (height . ,height)))

(define* (window-features window)
  "Get the features associated with the window."
  (assoc-ref window 'features))

(define* (window-feature window feature)
  "Get the value of a specific feature for a window or #f if it is not present."
  (assoc-ref (window-features window)
             feature))

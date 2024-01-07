(define-module (sage core window)
  #:export (
            make-window
            window-set-buffer!
            window-buffer
            window-set-position!
            make-position
            window-feature
            window-features
            window-set-feature!
            ))
(use-modules ((sage core rope)   #:prefix rope:)
             ((sage core buffer) #:prefix buffer:))

(define* (make-window #:key
                      buffer
                      (position (make-position 0 0 0 0))
                      (features '()))
  "Make a new window."
  (let ((mutable-features (map (lambda (p) (cons (car p) (cdr p))) features)))
    `((buffer   . ,buffer)
      (position . ,position)
      (features . ,mutable-features))))

(define* (window-buffer window)
  "Get the buffer associated buffer for window."
  (assoc-ref window 'buffer))

(define* (window-set-buffer! window buffer)
  "Set the buffer for window."
  (assoc-set! window 'buffer buffer))

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

(define* (window-set-feature! window feature value)
  "Set the value of a feature in window."
  (assoc-set! window
              'features
              (assoc-set! (window-features window) feature value)))

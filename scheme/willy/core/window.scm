(define-module (willy core window)
  #:export (
            make-window
            window-set-position!
            window-set-feature!
            window-buffer
            window-feature
            ))
(use-modules (srfi srfi-1)
             (srfi srfi-2))

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
    (height   . ,height)))

(define* (window-features window)
  "Get all the window features."
  (assoc-ref window 'features))

(define* (window-feature window feature)
  "Get the value for a specific window feature."
  (and-let* ((all-features (window-features window))
             (target-feature? (lambda (feature-value-pair)
                                (and (pair? feature-value-pair)
                                     (equal? feature (car feature-value-pair)))))
             (feature (and (pair? all-features)
                           (find target-feature? all-features))))
    (cdr feature)))

(define* (window-buffer window)
  "Get the buffer for the window."
  (assoc-ref window 'buffer))

(define* (window-set-feature! window feature value)
  "Set the feature in window to value."
  (assoc-set! window 'features
              (assoc-set! (window-features window) feature value)))

(define* (window-set-position! window #:key x y width height)
  (assoc-set! window 'x x)
  (assoc-set! window 'y y)
  (assoc-set! window 'width width)
  (assoc-set! window 'height height))

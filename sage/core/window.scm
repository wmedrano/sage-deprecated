(define-module (sage core window)
  #:export (
            <window>
            make-window
            window-set-buffer!
            window-buffer
            window-set-area!
            window-feature
            window-features
            window-set-feature!
            ))
(use-modules ((sage core rope)   #:prefix rope:)
             ((sage core buffer) #:prefix buffer:)
             ((sage core rect)   #:prefix rect:)
             (srfi srfi-9))

(define-record-type <window>
  (%make-window buffer area features)
  window?
  (buffer   window-buffer   window-set-buffer!)
  (area     window-area     window-set-area!)
  (features window-features window-set-features!))

(define* (make-window #:key
                      buffer
                      (area (rect:make-rect 0 0 0 0))
                      (features '()))
  "Make a new window."
  (define mutable-features (map (lambda (p) (cons (car p) (cdr p)))
                                features))
  (%make-window buffer area mutable-features))

(define* (window-feature window feature)
  "Get the value of a specific feature for a window or #f if it is not present."
  (assoc-ref (window-features window)
             feature))

(define* (window-set-feature! window feature value)
  "Set the value of a feature in window."
  (assoc-set! window
              'features
              (assoc-set! (window-features window) feature value)))

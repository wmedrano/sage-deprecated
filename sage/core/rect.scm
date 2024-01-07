(define-module (sage core rect)
  #:export (
            <rect>
            make-rect
            rect-x
            rect-set-x!
            rect-y
            rect-set-y!
            rect-width
            rect-set-width!
            rect-height
            rect-set-height!
            ))
(use-modules ((sage core rope)   #:prefix rope:)
             ((sage core buffer) #:prefix buffer:)
             (srfi srfi-9))

(define-record-type <rect>
  (make-rect x y width height)
  rect?
  (x      rect-x      rect-set-x!)
  (y      rect-y      rect-set-y!)
  (width  rect-width  rect-set-width!)
  (height rect-height rect-set-height!))

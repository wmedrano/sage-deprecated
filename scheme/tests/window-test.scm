(use-modules (srfi srfi-64)
             (ice-9 hash-table)
	     (willy core window))

(define %test-suite-name "window")
(test-begin %test-suite-name)

(test-equal "make-window sets fields"
            '(
              (buffer   . "my buffer")
              (features . ((a . 1) (b . "value")  (c . #t)))
              (x        . 1)
              (y        . 2)
              (width    . 3)
              (height   . 4))
            (make-window #:buffer "my buffer"
                         #:features '((a . 1)
                                      (b . "value")
                                      (c . #t))
                         #:x 1
                         #:y 2
                         #:width 3
                         #:height 4))

(test-equal "can get value for specific feature"
            "value"
            (window-feature (make-window #:buffer "my buffer"
                                         #:features '((a . 1)
                                                      (b . "value")
                                                      (c . #t)))
                            'b))

(test-end %test-suite-name)

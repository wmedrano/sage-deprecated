#!/bin/guile -s
!#
(use-modules ((willy main) #:prefix willy:))

(define (main)
  (willy:run!))

(main)

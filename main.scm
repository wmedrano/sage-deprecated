#!/bin/guile -s
!#
(define-module main)
(use-modules ((willy main) #:prefix willy))

(define (main)
  (willy:run!))

(main)

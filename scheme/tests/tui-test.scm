(use-modules (srfi srfi-64)
	     (willy tui)
	     (willy buffer))

(define %test-suite-name "tui")
(test-begin %test-suite-name)

(test-error "make-tui with bad backend is err"
  (make-tui 'bad-backend-type))

(test-equal "tui test size is fixed value"
  '((width . 80) (height . 24))
  (tui-size (make-tui 'test)))

(test-equal "make-tui is ok"
  (string-concatenate
   '(
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                "))
  (tui-state-for-test (make-tui 'test)))

(test-equal "can render layout"
  (string-concatenate
   '(
     "  1 example text                                                                \n"
     "  2   pass: true                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                "))
  (tui-state-for-test
   (tui-draw
    (make-tui 'test)
    `((
       (buffer . ,(make-buffer #:string "example text\n  pass: true "))
       (x      . 0)
       (y      . 0)
       (width  . 80)
       (height . 24))))))

(test-equal "can render multiple layouts"
  (string-concatenate
   '(
     "top left                                top right                               \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "  1 multiline middle                                                            \n"
     "  2 has line numbers                                                            \n"
     "                                                                                \n"
     "                                                                                "))
  (tui-state-for-test
   (tui-draw
    (make-tui 'test)
    `(
      ((buffer . ,(make-buffer #:string "top left"))
       (x      . 0)
       (y      . 0)
       (width  . 40)
       (height . 20))
      ((buffer . ,(make-buffer #:string "top right"))
       (x      . 40)
       (y      . 0)
       (width  . 40)
       (height . 20))
      ((buffer . ,(make-buffer #:string "multiline middle\nhas line numbers"))
       (x      . 0)
       (y      . 20)
       (width  . 40)
       (height . 4))
      ))))

(test-equal "out of range layouts not rendered"
  (string-concatenate
   '(
     "good                                                                            \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                "))
  (tui-state-for-test
   (tui-draw
    (make-tui 'test)
    `(
      ((buffer . ,(make-buffer #:string "partial out of range in x"))
       (x      . 40)
       (y      . 0)
       (width  . 80)
       (height . 24))
      ((buffer . ,(make-buffer #:string "partial out of range in y"))
       (x      . 0)
       (y      . 20)
       (width  . 80)
       (height . 24))
      ((buffer . ,(make-buffer #:string "completely out of range in x"))
       (x      . 100)
       (y      . 0)
       (width  . 80)
       (height . 24))
      ((buffer . ,(make-buffer #:string "completely out of range in y"))
       (x      . 0)
       (y      . 100)
       (width  . 80)
       (height . 24))
      ((buffer . ,(make-buffer #:string "good"))
       (x      . 0)
       (y      . 0)
       (width  . 80)
       (height . 24))))
   ))

(test-equal "layouts render on top of each other"
  (string-concatenate
   '(
     "1112222111                                                                      \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                "))
  (tui-state-for-test
   (tui-draw
    (make-tui 'test)
    `(
      ((buffer . ,(make-buffer #:string "1111111111"))
       (x      . 0)
       (y      . 0)
       (width  . 80)
       (height . 24))
      ((buffer . ,(make-buffer #:string "2222222222222222"))
       (x      . 3)
       (y      . 0)
       (width  . 4)
       (height . 24))))))

(test-end %test-suite-name)

(use-modules (srfi srfi-64)
	     (willy core tui)
	     (willy core buffer))

(define %test-suite-name "tui")
(test-begin %test-suite-name)

(test-error "make-tui with bad backend is err"
  (make-tui 'bad-backend-type))

(test-equal "tui test size is fixed value"
  '((width . 80) (height . 24))
<<<<<<< HEAD
  (tui-size (make-tui)))
=======
  (tui-size (make-tui 'test)))
>>>>>>> main

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
<<<<<<< HEAD
  (tui-state-for-test (make-tui)))
=======
  (tui-state-for-test (make-tui 'test)))
>>>>>>> main

(test-equal "can render window"
  (string-concatenate
   '(
     "example text                                                                    \n"
     "  pass: true                                                                    \n"
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
<<<<<<< HEAD
    (make-tui)
    `((
       (buffer . ,(make-buffer #:string "example text\n  pass: true "))
=======
    (make-tui 'test)
    `((
       (buffer . ,(make-buffer-content "example text\n  pass: true "))
>>>>>>> main
       (x      . 0)
       (y      . 0)
       (width  . 80)
       (height . 24))))))

(test-equal "can render multiple windows"
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
     "multiline string                                                                \n"
     "has multiple lines                                                              \n"
     "                                                                                \n"
     "                                                                                "))
  (tui-state-for-test
   (tui-draw
<<<<<<< HEAD
    (make-tui)
    `(
      ((buffer . ,(make-buffer #:string "top left"))
=======
    (make-tui 'test)
    `(
      ((buffer . ,(make-buffer-content "top left"))
>>>>>>> main
       (x      . 0)
       (y      . 0)
       (width  . 40)
       (height . 20))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "top right"))
=======
      ((buffer . ,(make-buffer-content "top right"))
>>>>>>> main
       (x      . 40)
       (y      . 0)
       (width  . 40)
       (height . 20))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "multiline string\nhas multiple lines"))
=======
      ((buffer . ,(make-buffer-content "multiline middle\nhas line numbers"))
>>>>>>> main
       (x      . 0)
       (y      . 20)
       (width  . 40)
       (height . 4))
      ))))

(test-equal "out of range windows not rendered"
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
<<<<<<< HEAD
    (make-tui)
    `(
      ((buffer . ,(make-buffer #:string "partial out of range in x"))
=======
    (make-tui 'test)
    `(
      ((buffer . ,(make-buffer-content "partial out of range in x"))
>>>>>>> main
       (x      . 40)
       (y      . 0)
       (width  . 80)
       (height . 24))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "partial out of range in y"))
=======
      ((buffer . ,(make-buffer-content "partial out of range in y"))
>>>>>>> main
       (x      . 0)
       (y      . 20)
       (width  . 80)
       (height . 24))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "completely out of range in x"))
=======
      ((buffer . ,(make-buffer-content "completely out of range in x"))
>>>>>>> main
       (x      . 100)
       (y      . 0)
       (width  . 80)
       (height . 24))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "completely out of range in y"))
=======
      ((buffer . ,(make-buffer-content "completely out of range in y"))
>>>>>>> main
       (x      . 0)
       (y      . 100)
       (width  . 80)
       (height . 24))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "good"))
=======
      ((buffer . ,(make-buffer-content "good"))
>>>>>>> main
       (x      . 0)
       (y      . 0)
       (width  . 80)
       (height . 24))))
   ))

(test-equal "windows render on top of each other"
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
<<<<<<< HEAD
    (make-tui)
    `(
      ((buffer . ,(make-buffer #:string "1111111111"))
=======
    (make-tui 'test)
    `(
      ((buffer . ,(make-buffer-content "1111111111"))
>>>>>>> main
       (x      . 0)
       (y      . 0)
       (width  . 80)
       (height . 24))
<<<<<<< HEAD
      ((buffer . ,(make-buffer #:string "2222222222222222"))
=======
      ((buffer . ,(make-buffer-content "2222222222222222"))
>>>>>>> main
       (x      . 3)
       (y      . 0)
       (width  . 4)
       (height . 24))))))

(test-end %test-suite-name)

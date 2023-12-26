(use-modules (srfi srfi-64)
	     (willy tui)
	     (willy buffer))

(define %test-suite-name "willy-tui")
(test-begin %test-suite-name)

(test-error "new-tui with bad backend is err"
  (new-tui 'bad-backend-type))

(test-equal "tui test size is fixed value"
  '((width . 80) (height . 24))
  (tui-size (new-tui 'test)))

(test-equal "new-tui is ok"
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
  (tui-state-for-test (new-tui 'test)))

(test-equal "can render buffer"
  (string-concatenate
   '(
     "example text                                                                    \n"
     "  pass: true _                                                                  \n"
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
     "Looking at a buffer in Willy!                                                   "))
  (let ((tui (new-tui 'test)))
    (tui-draw tui (list (new-buffer "example text\n  pass: true ")))
    (tui-state-for-test tui)))

(test-equal "can render multiple buffers"
  (string-concatenate
   '(
     "buffer a_                                                                       \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "buffer b_                                                                       \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "buffer c_                                                                       \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "                                                                                \n"
     "Looking at a buffer in Willy!                                                   "))
  (let ((tui   (new-tui 'test))
	(buf-a (new-buffer "buffer a"))
	(buf-b (new-buffer "buffer b"))
	(buf-c (new-buffer "buffer c")))
    (tui-draw tui (list buf-a buf-b buf-c))
    (tui-state-for-test tui)))

(test-end %test-suite-name)

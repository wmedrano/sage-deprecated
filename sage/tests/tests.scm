(use-modules ((sage core rope) #:prefix  rope:)
             ((sage core tui)  #:prefix  tui:)
             (srfi srfi-64))

(define %test-suite "sage-tests")

(test-begin %test-suite)

(test-equal "new rope has empty string"
  ""
  (rope:rope->string (rope:make-rope)))

(test-equal "new tui has empty buffer"
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
  (tui:tui->string (tui:make-tui)))

(test-assert "can delete tui"
  (tui:delete-tui! (tui:make-tui)))

(test-end %test-suite)

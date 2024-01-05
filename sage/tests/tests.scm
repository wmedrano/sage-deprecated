(use-modules ((sage core rope) #:prefix  rope:)
             ((sage core tui)  #:prefix  tui:)
             (srfi srfi-64))

(define %test-suite "sage-tests")

(test-begin %test-suite)

(test-equal "new rope has empty string"
  ""
  (rope:rope->string (rope:make-rope)))

(test-equal "new rope can initialize with text"
  "my text\nis here\n "
  (rope:rope->string (rope:make-rope #:text "my text\nis here\n ")))

(test-equal "can delete text from rope"
  "Hello World!"
  (let ((rope (rope:make-rope #:text "Hello12 World!")))
    (rope:rope-delete! rope 5 7)
    (rope:rope->string rope)))

(test-equal "can clear rope"
  ""
  (let ((rope (rope:make-rope #:text "This should be deleted!")))
    (rope:rope-clear! rope)
    (rope:rope->string rope)))

(test-equal "can clear empty rope"
  ""
  (let ((rope (rope:make-rope #:text "")))
    (rope:rope-clear! rope)
    (rope:rope->string rope)))

(test-equal "can insert onto rope"
  "Have a very happy day."
  (let ((rope (rope:make-rope #:text "Have a happy day.")))
    (rope:rope-insert! rope 7 "very ")
    (rope:rope->string rope)))

(test-equal "can replace onto rope"
  "Terminals are the future."
  (let ((rope (rope:make-rope #:text "Terminals are the past.")))
    (rope:rope-replace! rope 18 22 "future")
    (rope:rope->string rope)))

(test-equal "pop deletes final character"
  "Pop the letter "
  (let ((rope (rope:make-rope #:text "Pop the letter x")))
    (rope:rope-pop! rope)
    (rope:rope->string rope)))

(test-equal "pop on empty rope does nothing"
  ""
  (let ((rope (rope:make-rope #:text "")))
    (rope:rope-pop! rope)
    (rope:rope->string rope)))

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

(test-equal "can render rope on terminal"
  (string-concatenate
   '(
     "my rope                                                                         \n"
     "has 3                                                                           \n"
     " lines                                                                          \n"
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
  (tui:tui->string (tui:tui-draw!
                    (tui:make-tui)
                    (rope:make-rope #:text "my rope  \nhas 3\n lines\n\n"))))

(test-assert "can delete tui"
  (tui:delete-tui! (tui:make-tui)))

(test-end %test-suite)

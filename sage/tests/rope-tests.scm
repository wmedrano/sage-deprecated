(use-modules (sage core rope)
             (srfi srfi-64))

(define %test-suite-name "rope-tests")

(test-begin %test-suite-name)

(test-equal "make-rope sets initial string"
  "my first string"
  (rope->string
   (make-rope #:text "my first string")))

(test-equal "rope-insert! can insert in middle of string"
  '(4 . "ropes" )
  (let* ((rope (make-rope #:text "rs"))
         (end  (rope-insert! rope 1 "ope")))
    (cons end (rope->string rope))))

(test-equal "rope-insert! can insert at start of string"
  '(3 . "stars" )
  (let* ((rope (make-rope #:text "rs"))
         (end  (rope-insert! rope 0 "sta")))
    (cons end (rope->string rope))))

(test-equal "rope-insert! can insert at end of string"
  '(5 . "rs7rs" )
  (let* ((rope (make-rope #:text "rs"))
         (end  (rope-insert! rope 2 "7rs")))
    (cons end (rope->string rope))))

(test-equal "rope-replace! replaces contents between points and returns new endpoint"
  '( "hello world!" . 5)
  (let* ((rope      (make-rope #:text "hellase world!"))
         (end-point (rope-replace! rope 4 7 "o")))
    (cons (rope->string rope) end-point)))

(test-equal "rope-replace! handles unicode"
  '("🤖robots🤖" . 4)
  (let* ((rope     (make-rope #:text "🤖ro🤖🤖ots🤖"))
         (end-point (rope-replace! rope 3 5 "b")))
    (cons (rope->string rope) end-point)))

(test-equal "rope-length gives length"
  11
  (rope-length (make-rope #:text "123\n567\n9\n\n")))

(test-equal "rope-set-language! is ok"
  "(scheme-procedure (list example list for test )))))"
  (rope->string (make-rope #:text "(scheme-procedure (list example list for test )))))"
                           #:language "scheme")))

(test-equal "rope-line-count on empty string is 1"
  1
  (rope-line-count (make-rope #:text "")))

(test-equal "rope-line-count on string with no lines is 1"
  1
  (rope-line-count (make-rope #:text "my line")))

(test-equal "rope-line-count on multiline string gives line count"
  4
  (rope-line-count (make-rope #:text "line 1\nline 2\n line 3\nline 4")))

(test-equal "rope-line-length for empty line is 0"
  0
  (rope-line-length (make-rope #:text "")
                    0))

(test-error "rope-line-length out of bounds is error"
  (rope-line-length (make-rope #:text "my line\nother line\n")
                    100))

(test-equal "rope-line-length on trailing line is 0"
  0
  (rope-line-length (make-rope #:text "my line\nother line\n")
                    2))

(test-equal "rope-line-length on final line is accurate"
  4
  (rope-line-length (make-rope #:text "my line\nother line\n1234")
                    2))

(test-equal "rope-line-length on a middle line is ok"
  8
  (rope-line-length (make-rope #:text "my line\nother line\n1234567\nthis is the last one\n")
                    2))

(test-equal "rope-line-length on line before trailing new line includes newline"
  5
  (rope-line-length (make-rope #:text "my line\nother line\n1234\n")
                    2))

(test-end %test-suite-name)

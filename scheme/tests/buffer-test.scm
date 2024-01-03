(use-modules (srfi srfi-64)
	     (willy core buffer))

(define %test-suite-name "buffer")
(test-begin %test-suite-name)

;; (test-equal "New buffers have empty tests."
;;   ""
;;   (buffer->string (make-buffer)))

;; (test-equal "Make buffer with name has valid name"
;;   "my custom name"
;;   (buffer-name (make-buffer #:name "my custom name")))

;; (test-equal "New buffers have empty tests."
;;   ""
;;   (buffer->string (make-buffer)))

;; (test-equal "Make buffer with string has valid string"
;;   "my text"
;;   (buffer->string (make-buffer #:string "my text")))

;; (test-equal "Can insert string to buffer."
;;   "name: Willy\n"
;;   (buffer->string
;;    (buffer-insert-string! (make-buffer #:string "name: ")
;;                          "Willy\n")))

;; (test-equal "Can insert char to buffer."
;;   "Hello World!"
;;   (buffer->string
;;    (buffer-insert-string! (make-buffer #:string "Hello World")
;;                          #\!)))

;; (test-equal "buffer-pop-char! removes last character."
;;   "my tex"
;;   (let ((b (make-buffer #:string "my text")))
;;     (buffer-pop-char! b)
;;     (buffer->string b)))

;; (test-equal "buffer-pop-char! returns last character."
;;   #\t
;;   (buffer-pop-char! (make-buffer #:string "my text")))

;; (test-equal "buffer-clear! clears the buffer"
;;   ""
;;   (let ((b (make-buffer #:string "Delete me.")))
;;     (buffer-clear! b)
;;     (buffer->string b)))

;; (test-assert "can make buffer with language"
;;   (make-buffer #:name "rust-buffer"
;;                #:string "pub fn main () { }"
;;                #:language "rust"))

(test-end %test-suite-name)

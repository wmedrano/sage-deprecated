(define-module (willy open-file)
  #:export (open-file!))
(use-modules ((willy state)       #:prefix state:)
             ((willy core buffer) #:prefix buffer:)
             ((willy core window) #:prefix window:)
             ((willy modal)       #:select (run-modal!))
             ((ice-9 ftw)         #:prefix ftw:)
             ((ice-9 textual-ports))
             ((srfi srfi-1))
             ((srfi srfi-2)))

(define (on-select-file file)
  (and-let* ((window (state:focused-window))
             (buffer (window:window-buffer window))
             (text   (call-with-input-file file get-string-all)))
    (buffer:buffer-set-string buffer text)))

(define* (open-file!)
  "Open a new file on the current window."
  (run-modal! #:modal-name "open-file"
              #:prompt     "Open File: "
              #:items      (ftw:scandir "./")
              #:on-select  on-select-file))

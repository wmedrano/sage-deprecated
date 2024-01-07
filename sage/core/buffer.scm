(define-module (sage core buffer)
  #:export (
            make-buffer
            buffer-rope
            buffer-set-rope!
            buffer-file-path
            buffer-set-file-path!
            ))
(use-modules
 ;; ((sage core rope) #:prefix rope:)
 (srfi srfi-9)
 )

(define* (make-buffer #:key
                      rope
                      (file-path ""))
  "Create a new record."
  `((rope      . ,rope)
    (file-path . ,file-path)))

(define* (buffer-rope buffer)
  "Get the rope for this buffer."
  (assoc-ref buffer 'rope))

(define* (buffer-set-rope! buffer rope)
  "Set the rope for buffer."
  (assoc-set! buffer 'rope rope))

(define* (buffer-file-path buffer)
  "Get the file path for the buffer or #f if the buffer does not
correspond to a physical file."
  (assoc-ref buffer 'file-path))

(define* (buffer-set-file-path! buffer file-path)
  "Set the associated file-path for buffer."
  (assoc-set! buffer 'file-path file-path))

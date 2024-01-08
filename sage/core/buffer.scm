(define-module (sage core buffer)
  #:export (
            make-buffer
            buffer-rope
            buffer-set-rope!
            buffer-file-path
            buffer-set-file-path!
            buffer-insert-at-cursor!
            buffer-cursor
            buffer-set-cursor!
            buffer-delete-char-before-cursor!
            buffer-advance-cursor-vertical!
            ))
(use-modules ((sage core rope) #:prefix rope:)
             (srfi srfi-2)
             (srfi srfi-9))

(define-record-type <buffer>
  (%make-buffer rope cursor file-path)
  buffer?
  (rope      buffer-rope      buffer-set-rope!)
  (cursor    buffer-cursor    buffer-set-cursor!)
  (file-path buffer-file-path buffer-set-file-path!))

(define* (make-buffer #:key
                      rope
                      (cursor    #f)
                      (file-path ""))
  "Make a new buffer."
  (%make-buffer rope cursor file-path))

(define* (buffer-insert-at-cursor! buffer string-or-char)
  "Insert a string or character at the cursor position."
  (let ((rope   (buffer-rope buffer))
        (cursor (buffer-cursor buffer)))
    (buffer-set-cursor! buffer
                        (rope:rope-insert! rope cursor string-or-char))))

(define* (buffer-delete-char-before-cursor! buffer)
  (and-let* ((rope   (buffer-rope buffer))
             (cursor (buffer-cursor buffer))
             (valid? (> cursor 0))
             (start  (- cursor 1))
             (end    cursor))
    (rope:rope-replace! rope start end "")
    (buffer-set-cursor! buffer (- cursor 1))))

(define* (buffer-advance-cursor-vertical! buffer offset)
  (buffer-set-cursor! buffer
                      (rope:rope-cursor-line-offset
                       (buffer-rope buffer)
                       (buffer-cursor buffer)
                       offset)))

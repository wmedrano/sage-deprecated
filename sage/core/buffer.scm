(define-module (sage core buffer)
  #:export (
            make-buffer
            buffer-rope
            buffer-set-rope!
            buffer-file-path
            buffer-set-file-path!
            buffer-insert-at-cursor!
            buffer-cursor
            buffer-scroll-column!
            buffer-set-cursor!
            buffer-delete-char-before-cursor!
            buffer-scroll-row!
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
  "Delete the cursor that is right before the cursor."
  (and-let* ((rope   (buffer-rope buffer))
             (cursor (buffer-cursor buffer))
             (valid? (> cursor 0))
             (start  (- cursor 1))
             (end    cursor))
    (rope:rope-replace! rope start end "")
    (buffer-set-cursor! buffer (- cursor 1))))

(define* (buffer-scroll-column! buffer col-count)
  "Scroll the cursor by col-count number of characters.

If this reaches past the current row, then it continues to the next
row."
  (and-let* ((raw-cursor  (+ (buffer-cursor buffer) col-count))
             (rope-length (rope:rope-length (buffer-rope buffer)))
             (cursor      (cond
                           ((< raw-cursor 0) 0)
                           ((< raw-cursor rope-length) raw-cursor)
                           ((> rope-length 0) (- rope-length 1))
                           (else 0))))
    (buffer-set-cursor! buffer cursor)))

(define* (buffer-scroll-row! buffer row-count)
  "Move the cursor up and down the lines."
  (and-let* ((current-cursor   (buffer-cursor buffer))
             (rope             (buffer-rope buffer))
             (current-position (rope:rope-cursor->position rope current-cursor))
             (raw-row-idx      (+ (car current-position) row-count))
             (line-count       (rope:rope-line-count rope))
             (row-idx          (cond
                                ((< raw-row-idx 0) 0)
                                ((< raw-row-idx line-count) raw-row-idx)
                                ((equal? line-count 0) 0)
                                (else (- line-count 1))))
             (line-length      (rope:rope-line-length rope row-idx))
             (raw-col-idx      (cdr current-position))
             (col-idx          (cond
                                ((< raw-col-idx line-length) raw-col-idx)
                                ((equal? line-length 0) 0)
                                (else (- line-length 1))))
             (new-position     (cons row-idx col-idx))
             (cursor           (rope:rope-position->cursor rope new-position)))
    (buffer-set-cursor! buffer cursor)))

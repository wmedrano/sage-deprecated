(define-module (sage core buffer)
  #:export (
            make-buffer
            buffer-rope
            buffer-set-rope!
            buffer-file-path
            buffer-set-file-path!
            buffer-insert-at-cursor!
            buffer-cursor
            buffer-event-hook
            buffer-scroll-column!
            buffer-name
            buffer-set-name!
            buffer-set-cursor!
            buffer-delete-char-before-cursor!
            buffer-scroll-row!
            ))
(use-modules ((sage core rope) #:prefix rope:)
             (srfi srfi-2)
             (srfi srfi-9))

(define-record-type <buffer>
  (%make-buffer rope cursor file-path name event-hook)
  buffer?
  (rope       buffer-rope      buffer-set-rope!)
  (cursor     buffer-cursor    buffer-set-cursor!)
  (file-path  buffer-file-path buffer-set-file-path!)
  (name       buffer-name      buffer-set-name!)
  (event-hook buffer-event-hook))

(define* (make-buffer #:key
                      name
                      (rope       (rope:make-rope))
                      (cursor     #f)
                      (file-path  "")
                      (event-hook (make-hook 3)))
  "Make a new buffer."
  (%make-buffer rope cursor file-path name event-hook))

(define* (buffer-insert-at-cursor! buffer string-or-char)
  "Insert a string or character at the cursor position."
  (define rope (buffer-rope buffer))
  (define cursor (buffer-cursor buffer))
  (buffer-set-cursor! buffer
                      (rope:rope-insert! rope cursor string-or-char)))

(define* (buffer-delete-char-before-cursor! buffer)
  "Delete the cursor that is right before the cursor."
  (and-let* ((cursor (buffer-cursor buffer))
             (valid? (> cursor 0)))
    (define rope (buffer-rope buffer))
    (define start (- cursor 1))
    (define end cursor)
    (rope:rope-replace! rope start end "")
    (buffer-set-cursor! buffer (- cursor 1))))

(define* (buffer-scroll-column! buffer col-count)
  "Scroll the cursor by col-count number of characters.

If this reaches past the current row, then it continues to the next
row."
  (and-let* ((raw-cursor  (+ (buffer-cursor buffer) col-count)))
    (define cursor (cond
                    ((< raw-cursor 0) 0)
                    ((< raw-cursor rope-length) raw-cursor)
                    ((> rope-length 0) (- rope-length 1))
                    (else 0)))
    (define rope-length (rope:rope-length (buffer-rope buffer)))
    (buffer-set-cursor! buffer cursor)))

(define* (%clamp value min-value max-value)
  (cond
   ((< value min-value) min-value)
   ((> value max-value) max-value)
   (else value)))

(define* (buffer-scroll-row! buffer row-count)
  "Move the cursor up and down the lines.

TODO: Preserve the column when switching to a line that is shorter
than the current column."
  (and-let* ((rope             (buffer-rope buffer))
             (current-cursor   (buffer-cursor buffer)))
    (define current-row      (rope:rope-cursor->line rope current-cursor))
    (define current-col      (- current-cursor
                                (rope:rope-line->cursor rope current-row)))
    (define row              (%clamp (+ current-row row-count)
                                     0
                                     (max 0 (- (rope:rope-line-count rope) 1))))
    (define row-length       (rope:rope-line-length rope row))
    (define col              (%clamp current-col
                                     0
                                     (max 0 (- row-length 1))))
    (define cursor           (+ (rope:rope-line->cursor rope row)
                                col))
    (buffer-set-cursor! buffer cursor)))

(define-module (sage builtin editor)
  #:export (edit-buffer!))
(use-modules
 ((sage core buffer)    #:prefix buffer:)
 ((sage core rope)      #:prefix rope:)
 ((sage state)          #:prefix state:)
 (srfi srfi-1))

(define* (edit-buffer! window buffer event)
  "Performs editing on buffer according to event.

This mostly involves:
  - Inserting characters onto the buffer.
  - Deleting characters from the buffer.
  - Navigating the cursor with the arrow keys."
  (define cursor    (buffer:buffer-cursor buffer))
  (define rope      (buffer:buffer-rope buffer))
  (define key-code  (assoc-ref event 'key-code))
  (define ctrl?     (assoc-ref event 'ctrl?))
  (define alt?      (assoc-ref event 'alt?))
  (define mod?      (or ctrl? alt?))
  (define no-mod?   (not mod?))
  (when (and cursor no-mod?)
    (cond
     ((equal? key-code #\tab)
      (buffer:buffer-insert-at-cursor! buffer "    "))
     ((char? key-code)
      (buffer:buffer-insert-at-cursor! buffer key-code))
     ((equal? key-code "<backspace>")
      (buffer:buffer-delete-char-before-cursor! buffer))
     ((equal? key-code "<up>")
      (buffer:buffer-scroll-row! buffer -1))
     ((equal? key-code "<down>")
      (buffer:buffer-scroll-row! buffer 1))
     ((and (equal? key-code "<left>") (> cursor 0))
      (buffer:buffer-scroll-column! buffer -1))
     ((and (equal? key-code "<right>") (< cursor (rope:rope-length rope)))
      (buffer:buffer-scroll-column! buffer 1))))))

(define-module (willy modal)
  #:export (run-modal!))
(use-modules ((willy state)       #:prefix state:)
             ((willy core log)    #:select (log!))
             ((willy core buffer) #:prefix buffer:)
             ((willy core event)  #:prefix event:)
             ((willy core window) #:prefix window:)
             ((srfi srfi-1))
             ((srfi srfi-2))
             ((srfi srfi-111)))

(define* (run-modal! #:key
                     modal-name
                     prompt
                     items
                     on-select)
  "Run a modal interactively select an item from items.

modal-name - A unique name for this modal.
prompt     - Prompt to ask user for item.
items      - The list of items to select from.
on-select  - Procedure that takes three arguments once the user has
             selected a choice. The arguments are (window query item)"
  (let* ((frame-size    (unbox state:frame-size))
         (frame-width   (assoc-ref frame-size 'width))
         (frame-height  (assoc-ref frame-size 'height))
         (buffer        (buffer:make-buffer #:name     (string-concatenate
                                                        (list "*modal-" modal-name "*"))))
         (window        (window:make-window #:buffer   buffer
                                            #:features '((highlight-line . 1)
                                                         (border         . #t))
                                            #:x        (* frame-width  1/8)
                                            #:y        (* frame-height 1/16)
                                            #:width    (* frame-width  6/8)
                                            #:height   (* frame-height 14/16)))
         (target-window (state:focused-window))
         (all-items     items)
         (matches       all-items)
         (query         "")
         (restore-hook  (make-hook)))
    (define (update-query! new-query)
      (set! query new-query)
      (set! matches (filter-by-query all-items new-query))
      (reset-buffer! buffer prompt query matches (- frame-height 1)))
    (update-query! "")
    (define (restore!)
      (add-hook! state:post-event-hook
                 (lambda () (run-hook restore-hook))))
    (define (select-match!)
      (and-let* ((valid? (pair? matches))
                 (item   (car matches)))
        (on-select target-window query item)
        (restore!)))
    (define (handle-event! event)
      (and-let* ((no-mod? (not (or (assoc-ref event 'ctrl?)
                                   (assoc-ref event 'alt?))))
                 (key (assoc-ref event 'key)))
        (cond
         ((equal? key #\newline) (begin (restore!)
                                        (add-hook! state:post-event-hook select-match!)))
         ((equal? key "<esc>") (restore!))
         ((equal? key "<backspace>")
          (unless (equal? (string-length query) 0)
            (update-query! (substring query 0 (- (string-length query) 1)))))
         ((char? key)
          (update-query! (string-append-char query key))))))
    (add-hook! state:event-hook handle-event! #t)
    (add-hook! restore-hook (lambda () (remove-hook! state:event-hook handle-event!)))
    (state:add-window! window #:set-focus? #t)
    (add-hook! restore-hook (lambda ()
                              (state:remove-window! window)
                              (state:set-focused-window! target-window)))))

(define* (reset-buffer! buffer prompt query items limit)
  "Reset the contents of buffer to represent the given prompt with the
given items."
  (buffer:buffer-set-string!
   buffer
   (make-buffer-string buffer prompt query items limit)))

(define* (make-buffer-string buffer prompt query items limit)
  (let* ((query-line       (string-concatenate (list prompt query)))
         (lines            (cons query-line items))
         (final-line-count (min (length lines) limit)))
    (string-join (take lines final-line-count)
                 "\n")))

(define* (item->string item)
  "Convert an item to its string representation."
  (string-concatenate (list " " item "\n")))

(define* (item-matches-query? subqueries-lst item)
  "Returns #t if the item is a match for all strings in subqueries-lst."
  (every (lambda (sub-query) (string-contains item sub-query))
         subqueries-lst))

(define* (filter-by-query items query)
  "Returns all items that match the given query."
  (let ((subqueries-lst (string-split query #\space)))
    (filter (lambda (item) (item-matches-query? subqueries-lst item))
            items)))

(define* (string-append-char s c)
  "Return a string that is s appended by the character c."
  (let ((char-as-string (list->string (list c))))
    (string-concatenate (list s char-as-string))))

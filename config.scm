#!/bin/guile -s
!#
(define-module (config))
(use-modules ((willy main)        #:prefix willy:)
             ((willy core buffer) #:prefix buffer:)
             ((willy core event)  #:prefix event:)
             ((willy core log)    #:prefix log:)
             ((willy core window) #:prefix window:)
             ((willy state)       #:prefix state:)
             ((srfi srfi-1))
             ((srfi srfi-111)))

(define* (insert-char-to-selected-window event)
  "Insert the character from the event onto the buffer of the selected
window.

Only runs if the selected window has the feature 'editable? and the
event is a basic key press."
  (let ((w         (state:selected-window))
        (b         (state:buffer-for-selected-window))
        (modifier? (or (assoc-ref event 'ctrl?)
                       (assoc-ref event 'alt?)))
        (key       (assoc-ref event 'key)))
    (and b
         (window:window-feature w 'editable?)
         (not modifier?)
         (not (event:special-key? key))
         (buffer:buffer-insert-string b key))))

(define* (delete-char-from-selected-window event)
  "Delete the last character from the buffer of the selected window.

Only runs if the selected window has the 'editable? feature and the
event is a backspace key press."
  (let ((w         (state:selected-window))
        (b         (state:buffer-for-selected-window))
        (modifier? (or (assoc-ref event 'ctrl?)
                       (assoc-ref event 'alt?)))
        (key       (assoc-ref event 'key)))
    (and b
         (window:window-feature w 'editable?)
         (not modifier?)
         (equal? key event:backspace-key)
         (buffer:buffer-pop-char b))))

(define* (handle-ctrl-keys event)
  "Handle any operations having to do with the ctrl keys."
  (let ((key   (assoc-ref event 'key))
        (ctrl? (assoc-ref event 'ctrl?))
        (alt?  (assoc-ref event 'alt?)))
    (cond
     ((and ctrl? (not alt?) (equal? key "c")) (state:quit!)))))

(define* (reposition-status-bar width height)
  (define (status-bar-window? w)
    (window:window-feature w 'status-bar?))
  (define status-bar-window (find status-bar-window? (unbox state:windows)))
  (define (fix-position window)
    (window:window-set-position! window
                                 #:x 0 #:y (- height 1)
                                 #:width width #:height 1))
  (if status-bar-window (fix-position status-bar-window)))

(define* (resize-by-proportion width height)
  (define (at-least-1 x)
    (if (< x 1) 1 x))
  (let* ((previous-size   (unbox state:previous-frame-size))
         (previous-width  (assoc-ref previous-size 'width))
         (previous-height (assoc-ref previous-size 'height))
         (x-scale         (/ width previous-width))
         (y-scale         (/ height previous-height)))
    (define (fix-by-proportion window)
      (let* ((w-x      (* (assoc-ref window 'x) x-scale))
             (w-y      (* (assoc-ref window 'y) y-scale))
             (w-width  (* (assoc-ref window 'width) x-scale))
             (w-height (* (assoc-ref window 'height) y-scale)))
        (window:window-set-position! window
                                     #:x      w-x
                                     #:y      w-y
                                     #:width  (at-least-1 w-width)
                                     #:height (at-least-1 w-height))))
    (for-each fix-by-proportion
              (unbox state:windows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (main)
  (add-hook! state:event-hook insert-char-to-selected-window #t)
  (add-hook! state:event-hook delete-char-from-selected-window #t)
  (add-hook! state:event-hook handle-ctrl-keys #t)
  (add-hook! state:frame-resize-hook resize-by-proportion #t)
  (add-hook! state:frame-resize-hook reposition-status-bar #t)
  (willy:run!))
(main)

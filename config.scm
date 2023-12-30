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
             ((srfi srfi-2))
             ((srfi srfi-111)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (insert-char-to-selected-window event)
  "Insert the character from the event onto the buffer of the selected
window.

Only runs if the selected window has the feature 'editable? and the
event is a basic key press."
  (and-let* ((w (state:selected-window))
             (b (window:window-buffer w))
             (editable? (window:window-feature w 'editable?))
             (no-mod? (not (or (assoc-ref event 'ctrl?)
                               (assoc-ref event 'alt?))))
             (key (assoc-ref event 'key))
             (simple-key? (not (event:special-key? key))))
    (buffer:buffer-insert-string b key)))

(define* (delete-char-from-selected-window event)
  "Delete the last character from the buffer of the selected window.

Only runs if the selected window has the 'editable? feature and the
event is a backspace key press."
  (and-let* ((w (state:selected-window))
             (b (window:window-buffer w))
             (editable? (window:window-feature w 'editable?))
             (no-mod? (not (or (assoc-ref event 'ctrl?)
                               (assoc-ref event 'alt?))))
             (is-backspace (equal? (assoc-ref event 'key)
                                   event:backspace-key)))
    (buffer:buffer-pop-char b)))

(define* (handle-ctrl-keys event)
  "Handle any operations having to do with the ctrl keys."
  (and-let* ((key        (assoc-ref event 'key))
             (ctrl?      (and (assoc-ref event 'ctrl?)
                              (not (assoc-ref event 'alt?)))))
    (cond
     ((equal? key "c") (state:quit!)))))

(define* (reposition-status-bar width height)
  (and-let* ((status-bar-window? (lambda (w) (window:window-feature w 'status-bar?)))
             (status-bar-window  (find status-bar-window? (unbox state:windows))))
    (window:window-set-position!
     status-bar-window
     #:x 0 #:y (- height 1)
     #:width width #:height 1)))

(define* (resize-by-proportion width height)
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
                                     #:x     w-x     #:y      w-y
                                     #:width w-width #:height w-height)))
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

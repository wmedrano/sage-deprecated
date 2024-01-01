#!/bin/guile -s
!#
(use-modules ((willy main)         #:prefix willy:)
             ((willy core buffer)  #:prefix buffer:)
             ((willy core event)   #:prefix event:)
             ((willy core log)     #:select (log!))
             ((willy core window)  #:prefix window:)
             ((willy state)        #:prefix state:)
             ((willy open-file)    #:select (open-file!))
             ((srfi srfi-1))
             ((srfi srfi-2))
             ((srfi srfi-111)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (edit-focused-window-buffer event)
  "Possibly edit the focused buffer.

Based on the event, it may insert or delete characters from the
focused window buffer."
  (and-let* ((w  (state:focused-window))
             (editable? (window:window-feature w 'editable?))
             (b (window:window-buffer w))
             (key (assoc-ref event 'key)))
    (let ((ctrl? (assoc-ref event 'ctrl?))
          (alt?  (assoc-ref event 'alt?)))
      (cond
       ((and (not ctrl?) (not alt?) (char? key))
        (buffer:buffer-insert-string! b key))
       ((and (not ctrl?) (not alt?) (equal? key event:backspace-key))
        (buffer:buffer-pop-char! b))))))

(define* (handle-ctrl-keys event)
  "Handle any operations having to do with the ctrl keys."
  (let* ((key        (assoc-ref event 'key))
         (ctrl?      (and (assoc-ref event 'ctrl?)
                          (not (assoc-ref event 'alt?)))))
    (cond
     ((and ctrl? (equal? key #\o)) (open-file!))
     ((and ctrl? (equal? key #\c)) (state:quit!)))))

(define* (reposition-status-bar width height)
  (and-let* ((status-bar-window? (lambda (w) (window:window-feature w 'status-bar?)))
             (status-bar-window  (find status-bar-window? (state:windows))))
    (window:window-set-position! status-bar-window
                                 #:x 0
                                 #:y (- height 1)
                                 #:width width
                                 #:height 1)))

(define* (resize-by-proportion width height)
  (let* ((previous-size   (unbox state:previous-frame-size))
         (previous-width  (assoc-ref previous-size 'width))
         (previous-height (assoc-ref previous-size 'height))
         (x-scale         (/ width previous-width))
         (y-scale         (/ height previous-height)))
    (define (fix-by-proportion window)
      (let* ((w-x          (* (assoc-ref window 'x) x-scale))
             (w-y          (* (assoc-ref window 'y) y-scale))
             (w-width      (* (assoc-ref window 'width) x-scale))
             (w-height     (* (assoc-ref window 'height) y-scale)))
        (window:window-set-position! window
                                     #:x     w-x     #:y      w-y
                                     #:width w-width #:height w-height)))
    (for-each fix-by-proportion
              (state:windows))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (main . args)
  (log! "Args: " args)
  (state:set-focused-window! (find (lambda (w) (window:window-feature w 'editable?))
                                   (state:windows)))
  (add-hook! state:event-hook handle-ctrl-keys)
  (add-hook! state:event-hook edit-focused-window-buffer)
  ;; (add-hook! state:event-hook log!) ;; Noisy, but can be useful for debugging.

  (add-hook! state:frame-resize-hook resize-by-proportion)
  ;; Order is important. We want to reposition the status bar as the
  ;; last action.
  (add-hook! state:frame-resize-hook reposition-status-bar #t)

  (willy:run!))

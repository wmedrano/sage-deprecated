#!/bin/guile -s
!#
(define-module (config))
(use-modules ((willy main)        #:prefix willy:)
             ((willy core buffer) #:prefix buffer:)
             ((willy core event)  #:prefix event:)
             ((willy core log)    #:select (log!))
             ((willy core window) #:prefix window:)
             ((willy state)       #:prefix state:)
             ((ice-9 ftw)         #:prefix ftw:)
             ((srfi srfi-1))
             ((srfi srfi-2))
             ((srfi srfi-111)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Dialog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define open-file-window (box #f))
(define open-file-files (box '()))
(define open-file-query (box ""))

(define* (open-file-set-query! query)
  (set-box! open-file-query query)
  (let* ((query-display (string-concatenate (list "Open file: " (unbox open-file-query))))
         (buffer-str    (string-join (cons query-display (unbox open-file-files))
                                     "\n  ")))
    (buffer:buffer-set-string (window:window-buffer (unbox open-file-window))
                              buffer-str)))

(define* (open-file-handle-event event)
  (and-let* ((q (unbox open-file-query))
             (window (unbox open-file-window))
             (key (assoc-ref event 'key)))
    (cond
     ((equal? key "\n") #f)
     ((equal? key "<backspace>")
      (unless (equal? (string-length q) 0)
        (open-file-set-query! (substring q 0 (- (string-length q) 1)))))
     (else (open-file-set-query! (string-concatenate
                                  (list q key)))))))

(define* (open-file-open!)
  (add-hook! state:event-hook open-file-handle-event)
  (set-box! open-file-files (ftw:scandir "./"))
  (let* ((frame-size (unbox state:frame-size))
         (frame-width (assoc-ref frame-size 'width))
         (frame-height (assoc-ref frame-size 'height))
         (buffer (buffer:make-buffer #:name "open-file-dialog"))
         (window (window:make-window #:buffer   buffer
                                     #:features '((border . #t))
                                     #:x        (* frame-width 1/8)
                                     #:y        (* frame-height 1/16)
                                     #:width    (* frame-width 6/8)
                                     #:height   (* frame-height 14/16))))
    (set-box! open-file-window window)
    (open-file-set-query! "")
    (state:add-window! window #:set-focus? #t)))

(define* (open-file-close!)
  (remove-hook! state:event-hook open-file-handle-event)
  (state:retain-windows! (lambda (w)
                           (not
                            (eq? (unbox open-file-window)
                                 w))))
  (let* ((is-editable? (lambda (w) (window:window-feature w 'editable?)))
         (any-editable-window (find is-editable?
                                    (unbox state:windows))))
    (state:set-focused-window! (or any-editable-window
                                   (car (unbox state:windows))))
    (set-box! open-file-window #f)))

(define* (open-file!)
  (if (unbox open-file-window)
      (open-file-close!)
      (open-file-open!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (main)
  (add-hook! state:event-hook insert-char-to-focused-window #t)
  (add-hook! state:event-hook delete-char-from-focused-window #t)
  (add-hook! state:event-hook handle-ctrl-keys #t)

  (add-hook! state:frame-resize-hook resize-by-proportion #t)
  (add-hook! state:frame-resize-hook reposition-status-bar #t)

  (willy:run!))
(main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (insert-char-to-focused-window event)
  "Insert the character from the event onto the buffer of the focused
window.

Only runs if the focused window has the feature 'editable? and the
event is a basic key press."
  (and-let* ((w (state:focused-window))
             (b (window:window-buffer w))
             (editable? (window:window-feature w 'editable?))
             (no-mod? (not (or (assoc-ref event 'ctrl?)
                               (assoc-ref event 'alt?))))
             (key (assoc-ref event 'key))
             (simple-key? (not (event:special-key? key))))
    (buffer:buffer-insert-string b key)))

(define* (delete-char-from-focused-window event)
  "Delete the last character from the buffer of the focused window.

Only runs if the focused window has the 'editable? feature and the
event is a backspace key press."
  (and-let* ((w (state:focused-window))
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
     ((equal? key "o") (open-file!))
     ((equal? key "h") (for-each  log!
                                  (hook->list state:event-hook)))
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

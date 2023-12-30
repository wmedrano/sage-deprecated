(define-module (willy main)
  #:export (run!))
(use-modules
 ((willy core buffer) #:prefix buffer:)
 ((willy core event)  #:prefix event:)
 ((willy core log)    #:prefix log:)
 ((willy core tui)    #:prefix tui:)
 ((willy core window) #:prefix window:)
 ((willy state)       #:prefix state:)
 ((srfi srfi-1))
 ((srfi srfi-111)))

(define* (run!)
  "Run the Willy text editor."
  (with-exception-handler
   cleanup-and-reraise-exception
   run-impl!))

(define* (run-impl!)
  "Run the Willy text editor."
  (set-box! state:tui (tui:make-tui 'terminal))
  (log:log! "Starting Willy!")
  (event:run-event-loop
   #:tui           (unbox state:tui)
   #:should-run-p  (lambda () (unbox state:tui))
   #:make-layout   (lambda () (unbox state:windows))
   #:on-resize     state:update-frame-size!
   #:event-pump    event:next-event-from-terminal
   #:event-handler handle-event!)
  ;; Just in case quit was not called and we need to clean up.
  (state:quit!))

(define* (cleanup-and-reraise-exception exception)
  "Performs cleanups and raises exception.

This is important as not cleaning up will keep the terminal in an unusable state."
  (state:quit!)
  (raise-exception exception))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define* (handle-event! event)
  "Handle a single event."
  (run-hook state:event-hook event))

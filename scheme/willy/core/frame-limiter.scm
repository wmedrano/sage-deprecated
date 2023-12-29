(define-module (willy core frame-limiter)
  #:export (
            limit-frames
            make-frame-limiter
            ))
(use-modules ((willy core internal) #:prefix internal:))

(define* (make-frame-limiter target-fps)
  "Make a new frame limiter with the given target fps."
  (internal:--make-frame-limiter target-fps))

(define* (limit-frames frame-limiter)
  "Limit the frames to match the fps.

If the frame timings are larger than calls to limit-frames, then
nothing will happen."
  (internal:--limit-frames frame-limiter))

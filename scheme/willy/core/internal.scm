(define-module (willy core internal)
  #:export (
            --buffer-content-insert-string
            --buffer-content-pop-char
            --buffer-content-clear
            --buffer-content-to-string
            --delete-tui
            --limit-frames
            --make-buffer-content
            --make-frame-limiter
            --make-tui
            --next-event-from-terminal
            --tui-draw
            --tui-size
            --tui-state-for-test
            ))

(load-extension "libwilly" "scm_init_willy_core_internal_module")

(define-module (sage core internal)
  #:export (
            make-rope
            rope->string
            make-tui
            tui-draw!
            delete-tui!
            ))

(load-extension "libsage" "sage_core_internal_init")

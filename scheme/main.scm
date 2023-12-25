(use-modules (willy buffer)
	     (willy tui))

(define log-buffer (new-buffer))
(define buffers '((new-scratch-buffer) log-buffer))

(buffer-insert-string log-buffer "Running TUI for 1 seconds.\n")
(define main-tui (new-tui))
(sleep 1)

(delete-tui main-tui)
(buffer-insert-string log-buffer "Deleting TUI for 1 seconds.\n")
(sleep 1)

(buffer-insert-string log-buffer "Pausing for 1 seconds.\n")
(sleep 1)

(display (buffer-to-text log-buffer))

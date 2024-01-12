(use-modules (sage core tree-sitter)
             ((sage core rope) #:prefix rope:)
             (srfi srfi-64))

(define %test-suite-name "treesitter-tests")

(test-begin %test-suite-name)

(test-assert "Rust parser is supported."
  (make-parser 'rust))

(test-assert "Scheme parser is supported."
  (make-parser 'scheme))

(test-error "Unsupported language produces error"
            (make-parser 'lisp))

(test-assert "Can parse good text file"
  (parser-parse!
   (make-parser 'scheme)
   (rope:make-rope #:text "(proc a b c)")))

(test-assert "Can parse invalid text file"
  ;; Although invalid, tree-sitter still attempts to parse and produce
  ;; a syntax tree.
  (parser-parse!
   (make-parser 'scheme)
   (rope:make-rope #:text "let language = not_at_all_scheme))")))

(test-end %test-suite-name)

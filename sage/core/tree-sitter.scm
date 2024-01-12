(define-module (sage core tree-sitter)
  #:export (
            make-parser
            parser-parse!
            tree-edit!
            ))
(use-modules ((sage core internal) #:prefix ffi:)
             (srfi srfi-2))

(define* (make-parser language)
  "Make a parser for the given language."
  (ffi:make-parser language))

(define* (parser-parse! parser rope
                       #:key
                       (previous-tree #f))
  "Parse a rope into a tree. If previous-tree is set, the tree is
parsed incrementally."
  (ffi:parser-parse! parser rope previous-tree))

(define* (tree-edit! tree #:key start-byte old-end-byte new-end-byte)
  "Mark an area as edited. It will be incrementally reparsed when
calling parser-parse.

To produce the new syntax tree, call parser-parse!"
  (ffi:tree-edit! tree start-byte old-end-byte new-end-byte))

#lang rosette

; Script to run the synthesizer on a given attribute grammar.

(require racket/cmdline
         racket/pretty
         "grammar/parse.rkt"
         "grammar/syntax.rkt"
         "grammar/tree.rkt"
         "schedule/parse.rkt"
         "tracing/synthesizer.rkt")

;(define verbose? (make-parameter #f))
(define root-name (make-parameter 'Root))

; FIXME: This is a horrid hack and only supports sequential schedule sketches.
(define (parse-schedule-sketch G S0)
  (define traversals
    (reverse
     (map (compose (curry grammar-traversal G) string->symbol)
          (filter non-empty-string?
                  (map string-trim (string-split S0 ";"))))))
  (foldr (curry list 'seq) (first traversals) (rest traversals)))

(command-line
 #:program "synthesize"
 #:once-each
 ;[("-v" "--verbose") "Display verbose intermediate information"
 ;                    (verbose? #t)]
 [("-R" "--root") classname "Name of the attribute grammar's root interface"
                   (root-name (string->symbol classname))]
 #:args (schedule-sketch grammar-filename)
 (let* ([G (file->grammar grammar-filename)]
        [E (tree-examples G (root-name))]
        [S (parse-schedule-sketch G schedule-sketch)]
        [S* (complete-sketch G S E)])
   (when S*
     (displayln (schedule->string S*)))))

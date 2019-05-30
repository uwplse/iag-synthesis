#lang rosette

; Script to test on margin collapse.

(require racket/pretty
         "../grammar/parse.rkt"
         "../schedule/parse.rkt"
         "../tree.rkt"
         "../tracing/synthesizer.rkt"
         ;"../backend/rust/builder.rkt"
         ;"../backend/rust/printer.rkt"
         )

(displayln "Parsing attribute grammar...")
(define collapse-grammar (file->grammar "benchmarks/css/collapse.grammar"))

(displayln "Generating attribute trees...")
(define collapse-examples (tree-examples collapse-grammar 'Tree))

; The schedule sketch for which to synthesize a completion.
(define collapse-sketch
  '(trav seq
         ((Root
           (hole)
           (recur root))
          (Node
           (hole)
           (iter-left kids ((hole) (recur kids)))
           (iter-right kids ((hole)))
           (hole))
          (Leaf
           (hole)))))

; Synthesis with dataflow-tracing symbolic evaluation.
(define (test-collapse)
  (complete-sketch collapse-grammar collapse-sketch collapse-examples))

(newline)
(displayln "Synthesizing a schedule to complete sketch...")
(define collapse-schedule (test-collapse))
(when collapse-schedule
  (displayln (schedule->string collapse-schedule)))

#lang rosette

; Script to test on HVBox.

(require racket/pretty
         "../grammar/parse.rkt"
         "../grammar/tree.rkt"
         "../schedule/parse.rkt"
         "../tracing/synthesizer.rkt"
         ;"../backend/rust/builder.rkt"
         ;"../backend/rust/printer.rkt"
         )

(displayln "Parsing attribute grammar...")
(define hvbox-grammar (file->grammar "benchmarks/hvbox/hvbox.grammar"))

(displayln "Generating attribute trees...")
(define hvbox-examples (tree-examples hvbox-grammar 'Root))

; The schedule sketch for which to synthesize a completion.
(define hvbox-sketch
  `(seq (trav post
              ((Top (recur root) (hole))
               (HBox (recur childs) (iter-left childs ((hole))) (hole))
               (VBox (recur childs) (iter-left childs ((hole))) (hole))
               (Leaf (hole))))
        (trav pre
              ((Top (hole) (recur root))
               (HBox (hole) (iter-left childs ((hole))) (recur childs))
               (VBox (hole) (iter-left childs ((hole))) (recur childs))
               (Leaf (hole))))))


; Another schedule sketch, this time using parallel composition.
(define hvbox-sketch-parallel
  `(par ,hvbox-sketch ,hvbox-sketch))

; Synthesis with dataflow-tracing symbolic evaluation.
(define (test-hvbox)
  (complete-sketch hvbox-grammar hvbox-sketch hvbox-examples))

(newline)
(displayln "Synthesizing a schedule to complete sketch...")
(define hvbox-schedule (test-hvbox))
(when hvbox-schedule
  (displayln (schedule->string hvbox-schedule))
;  (newline)
;  (displayln "Elaborating synthesized schedule...")
;  (let ([hvbox-program (elaborate-schedule hvbox-grammar hvbox-schedule)])
;    (pretty-print hvbox-program)
;    (newline)
;    (displayln "Generating a Rust program to implement synthesized schedule...")
;    (rust:print (build-program hvbox-grammar hvbox-program)))
  )

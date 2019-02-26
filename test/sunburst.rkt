#lang rosette

; Script to test on Sunburst.

(require "../engine.rkt")

(displayln "Parsing attribute grammar...")
(define sunburst-grammar (read-grammar "benchmarks/sunburst/sunburst.grammar" 'IRoot))

(displayln "Generating attribute trees...")
(define sunburst-examples (tree-examples sunburst-grammar))

; The schedule sketch for which to synthesize a completion.
(define sunburst-sketch
  (sched-sequential (sched-traversal 'zip
                                     `((Root ,(sched-hole))
                                       (Radial ,(sched-hole) ,(sched-hole) ,(sched-hole))
                                       (Leaf ,(sched-hole))))
                    (sched-traversal 'pre
                                     `((Root ,(sched-hole))
                                       (Radial ,(sched-hole) ,(sched-hole))
                                       (Leaf ,(sched-hole))))))

; Synthesis with dataflow-tracing symbolic evaluation.
(define (tracing:test-sunburst)
  (tracing:complete-sketch sunburst-grammar (make-hole-range sunburst-grammar)
                           sunburst-sketch sunburst-examples))

; Synthesis with general-purpose symbolic evaluation (Rosette's engine).
(define (checking:test-sunburst)
  (checking:complete-sketch sunburst-grammar (make-hole-range sunburst-grammar)
                            sunburst-sketch sunburst-examples))

(displayln "Synthesizing a schedule from sequential sketch...")
(define sunburst-schedule (tracing:test-sunburst))
(when sunburst-schedule
  (display-schedule sunburst-schedule))
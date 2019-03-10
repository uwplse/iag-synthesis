#lang rosette

; Script to test on miniCSS.

(require "../engine.rkt")

(displayln "Parsing attribute grammar...")
(define minicss-grammar (read-grammar "benchmarks/css/minicss.grammar" 'Root))

(displayln "Generating attribute trees...")
(define minicss-examples (tree-examples minicss-grammar))

; The schedule sketch for which to synthesize a completion.
(define minicss-sketch
  (let* ([visitors `((Document ,(sched-hole))
                     (BlockFlow ,(sched-hole) ,(sched-hole) ,(sched-hole) ,(sched-hole))
                     (ImageFlow ,(sched-hole))
                     (InlineFlow ,(sched-hole) ,(sched-hole) ,(sched-hole) ,(sched-hole))
                     (InlineText))]
         [traversal (lambda (ord) (sched-traversal ord visitors))])
    (sched-sequential (sched-traversal 'inter_pre_order visitors)
      (sched-sequential (sched-traversal 'inter_post_order visitors)
        (sched-sequential (sched-traversal 'pre_order visitors)
          (sched-sequential (sched-traversal 'post_order visitors)
            (sched-sequential (sched-traversal 'in_order visitors)
              (sched-traversal 'pre_order visitors))))))))

; Synthesis with dataflow-tracing symbolic evaluation.
(define (tracing:test-minicss)
  (tracing:complete-sketch minicss-grammar (make-hole-range minicss-grammar)
                           minicss-sketch minicss-examples))

; Synthesis with general-purpose symbolic evaluation (Rosette's engine).
(define (checking:test-minicss)
  (checking:complete-sketch minicss-grammar (make-hole-range minicss-grammar)
                            minicss-sketch minicss-examples))

(displayln "Synthesizing a schedule from sequential sketch...")
(define minicss-schedule (tracing:test-minicss))
(when minicss-schedule
  (display-schedule minicss-schedule))

#lang rosette

; Script to test on HVBox.

(require racket/pretty
         "../engine.rkt"
         "../schedule/elaborate.rkt"
         "../backend/rust/builder.rkt"
         "../backend/rust/printer.rkt")

(displayln "Parsing attribute grammar...")
(define hvbox-grammar (read-grammar "benchmarks/hvbox/hvbox.grammar" 'Top))

(displayln "Generating attribute trees...")
(define hvbox-examples (tree-examples hvbox-grammar))

; The schedule sketch for which to synthesize a completion.
(define hvbox-sketch
  (sched-sequential (sched-traversal 'post
                                     `((Root ,(sched-hole))
                                       (HBox ,(sched-hole) ,(sched-hole))
                                       (VBox ,(sched-hole) ,(sched-hole))
                                       (Leaf ,(sched-hole))))
                    (sched-traversal 'pre
                                     `((Root ,(sched-hole))
                                       (HBox ,(sched-hole))
                                       (VBox ,(sched-hole))
                                       (Leaf ,(sched-hole))))))


; Another schedule sketch, this time using parallel composition.
(define hvbox-sketch-parallel
  (sched-parallel hvbox-sketch hvbox-sketch))

; Synthesis with dataflow-tracing symbolic evaluation.
(define (tracing:test-hvbox)
  (tracing:complete-sketch hvbox-grammar (make-hole-range hvbox-grammar)
                           hvbox-sketch hvbox-examples))

; Synthesis with general-purpose symbolic evaluation (Rosette's engine).
(define (checking:test-hvbox)
  (checking:complete-sketch hvbox-grammar (make-hole-range hvbox-grammar)
                            hvbox-sketch hvbox-examples))

(newline)
(displayln "Synthesizing a schedule to complete sketch...")
(define hvbox-schedule (tracing:test-hvbox))
(when hvbox-schedule
  (display-schedule hvbox-schedule)
  (newline)
  (displayln "Elaborating synthesized schedule...")
  (let ([hvbox-program (elaborate-schedule hvbox-grammar hvbox-schedule)])
    (pretty-print hvbox-program)
    (newline)
    (displayln "Generating a Rust program to implement synthesized schedule...")
    (rust:print (build-program hvbox-grammar hvbox-program))))

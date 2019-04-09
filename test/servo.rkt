#lang rosette

; Script to test Servo layout.

(require racket/pretty
         "../engine.rkt"
         "../schedule/elaborate.rkt"
         "../backend/rust/builder.rkt"
         "../backend/rust/printer.rkt")

(displayln "Parsing attribute grammar...")
(define servo-grammar (read-grammar "benchmarks/servo/servo.grammar" 'Flow))

(displayln "Parsing sample schedule...")
(define servo-schedule (read-schedule "benchmarks/servo/servo.sched"))

;(displayln "Generating attribute trees...")
;(define servo-examples (tree-examples servo-grammar))

; The schedule sketch for which to synthesize a completion.
;(define servo-sketch
;  (sched-sequential (sched-traversal 'post
;                                     `((Root ,(sched-hole))
;                                       (HBox ,(sched-hole) ,(sched-hole))
;                                       (VBox ,(sched-hole) ,(sched-hole))
;                                       (Leaf ,(sched-hole))))
;                    (sched-traversal 'pre
;                                     `((Root ,(sched-hole))
;                                       (HBox ,(sched-hole))
;                                       (VBox ,(sched-hole))
;                                       (Leaf ,(sched-hole))))))


; Another schedule sketch, this time using parallel composition.
;(define servo-sketch-parallel
;  (sched-parallel servo-sketch servo-sketch))

; Synthesis with dataflow-tracing symbolic evaluation.
;(define (tracing:test-servo)
;  (tracing:complete-sketch servo-grammar (make-hole-range servo-grammar)
;                           servo-sketch servo-examples))

; Synthesis with general-purpose symbolic evaluation (Rosette's engine).
;(define (checking:test-servo)
;  (checking:complete-sketch servo-grammar (make-hole-range servo-grammar)
;                            servo-sketch servo-examples))

(newline)
;(displayln "Synthesizing a schedule to complete sketch...")
;(define servo-schedule (tracing:test-servo))
(when servo-schedule
  (display-schedule servo-schedule)
  (newline)
  (displayln "Elaborating sample schedule...")
  (let ([servo-program (elaborate-schedule servo-grammar servo-schedule)])
    (pretty-print servo-program)
    (newline)
    (displayln "Generating Rust visitors to implement schedule...")
    (rust:print (build-program servo-grammar servo-program))))

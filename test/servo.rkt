#lang rosette

; Script to test Servo layout.

(require "../grammar/parse.rkt"
         "../schedule/parse.rkt"
         "../tree.rkt"
         "../backend/rust/builder.rkt"
         "../backend/rust/printer.rkt")

(displayln "Parsing attribute grammar...")
(define servo-grammar (file->grammar "benchmarks/servo/servo.grammar"))

(displayln "Parsing sample schedule...")
(define servo-schedule (file->schedule "benchmarks/servo/servo.sched"))

(displayln "Generating attribute trees...")
(define servo-examples (tree-examples servo-grammar 'Flow))

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
  (displayln (serialize-schedule servo-schedule))
  (newline)
  (displayln "Compiling traversal schedule into Rust (file 'synthesized.rs')...")
  (parameterize ([current-output-port (open-output-file "synthesized.rs" #:mode 'text)])
    (rust:print (build-program servo-schedule)))
  (displayln "To use the generated source file, move it to 'servo/components/layout/'."))
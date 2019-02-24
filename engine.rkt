#lang rosette

; Benchmark driver for schedule synthesis and verification.

(require "grammar/parse.rkt"
         "grammar/syntax.rkt"
         "grammar/typecheck.rkt"
         "schedule/syntax.rkt"
         "schedule/parse.rkt"
         "schedule/enumerate.rkt"
         "tree.rkt"
         (prefix-in tracing: "tracing/interpreter.rkt")
         (prefix-in tracing: "tracing/synthesizer.rkt")
         (prefix-in checking: "checking/interpreter.rkt")
         (prefix-in checking: "checking/synthesizer.rkt"))

(provide (all-from-out "grammar/syntax.rkt")
         (all-from-out "schedule/syntax.rkt")
         (all-from-out "schedule/enumerate.rkt")
         (all-from-out "tree.rkt")
         (all-from-out "checking/interpreter.rkt")
         (all-from-out "checking/synthesizer.rkt")
         (all-from-out "tracing/interpreter.rkt")
         (all-from-out "tracing/synthesizer.rkt")
         (all-defined-out))

; Read and parse the attribute grammar in the named file, returning the list of
; class ASTs (interface ASTs unnecessary for interpretation).
(define (read-grammar filename [root 'Root])
  (let* ([port (open-input-file filename #:mode 'text)]
         [grammar (ag-normalize root (ag-parse port))])
    (close-input-port port)
    (ag-typecheck grammar)
    grammar))

; Read and parse the tree traversal schedule in the named file, returning the
; AST.
(define (read-schedule filename)
  (let* ([port (open-input-file filename #:mode 'text)]
         [schedule (sched-parse port)])
    (close-input-port port)
    schedule))

(define (read-tree grammar filename)
  (xml->tree grammar (file->string filename #:mode 'text)))

(define (synthesize-schedules grammar examples passes synthesizer)
  (let ([hole-range (make-hole-range grammar)]
        [sketches (enumerate-sketches grammar passes)])
    (for ([sketch (shuffle sketches)])
      (printf "\n\n\n--------------------------------\n")
      (displayln sketch)
      (synthesizer grammar hole-range sketch examples)
      (clear-asserts!))))

;; (define (search grammar examples passes))

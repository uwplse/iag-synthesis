#lang rosette

(require "tree.rkt")
(require "statements.rkt")
(require "trace.rkt")

(provide traverse)

(trace-init statements reads writes)
(define traverse1 (trace-slot))
(define traverse2 (trace-slot))
(define traverse3 (trace-slot))
(define traverse4 (trace-slot))
(define (traverse self)
  (when (inner? self)
    (trace-step self traverse1)
    (traverse (inner-left self))
    (trace-step self traverse2)
    (traverse (inner-right self))
    (trace-step self traverse3)
    (trace-step self traverse4)))

(define (test)
  (traverse large-tree)
  (let ([schedule (trace-exit)])
    (printf "traverse[1]: ~a\n" (first schedule))
    (printf "traverse[2]: ~a\n" (second schedule))
    (printf "traverse[3]: ~a\n" (third schedule))
    (printf "traverse[4]: ~a\n" (fourth schedule))))

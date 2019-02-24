#lang rosette

(provide init-tracer)

; traces as relational constraints (compositional inverse uninterpreted functions)
;
; Encode the trace using mutually inverted uninterpreted functions in Z3. Since
; Rosette does not support custom axioms, we emit appropriate assertions to
; guarantee the inversion.
;
; Is it necessary to map possible statements to a contiguous range of integers?
; How difficult will it be to compute the range(s) of the integer image of the
; space of possible statements?

; Generate functions to encode/decode schedule attributes as integer identifiers
; and to map between these integer identifiers and step numbers.
(define (init-tracer trace-inverts-sched sched-inverts-trace hint-uniqueness tasks)
  ; take an integer identifier of a task to a step number
  (define-symbolic sched (~> integer? integer?))
  ; take a step number to an integer identifier of a task
  (define-symbolic trace (~> integer? integer?))

  ; encode a task symbol as an integer identifier
  (define (encode task)
    (vector-memq task tasks))
  ; decode an integer identifier as a task symbol
  (define (decode ident)
    (vector-ref tasks ident))

  ; constrain unstep to be the inverse of step
  (when trace-inverts-sched
    (for ([ident (in-range (vector-length tasks))])
      (assert (eq? (trace (sched ident)) ident))))

  ; constrain step to be the inverse of unstep
  (when sched-inverts-trace
    (for ([step (in-range (vector-length tasks))])
      (assert (eq? (sched (trace step)) step))))

  ; bound the range of step
  (for ([ident (in-range (vector-length tasks))])
    ; splitting this conjunction is somehow bad for performance
    (assert (and (>= (sched ident) 0)
                 (< (sched ident) (vector-length tasks)))))

  ; explicitly constrain every step to be unique
  (when hint-uniqueness
    (for* ([ident0 (in-range (vector-length tasks))]
           [ident1 (in-range ident0)])
      (assert (not (eq? (sched ident0) (sched ident1))))))

  (list encode decode sched trace))

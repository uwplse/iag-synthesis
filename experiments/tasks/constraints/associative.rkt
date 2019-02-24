#lang rosette

(provide init-tracer)

; schedule synthesis with explicit traces (list associating attributes with
; existential steps)
;
; Implements our idea for schedule synthesis from traces, based on constructing
; a concrete trace data structure (really, a mapping from tasks to program
; points) to query during synthesis or validation. Crucially, this implementation
; does not encode the trace as an invertible uninterpreted function, and so we
; get some ugly constraints, growing quadratically with the number of attributes.

; Generate functions to encode/decode schedule attributes as integer identifiers
; and to map between these integer identifiers and step numbers. The index
; argument should be either 'step or 'task.
(define (init-tracer index hint-uniqueness tasks)
  ; a useful version of memq
  (define (memq v xs)
    (define (rec i xs)
      (cond
        [(null? xs) #f]
        [(eq? v (car xs)) i]
        [else (rec (+ i 1) (cdr xs))]))
    (rec 0 xs))

  ; take an integer identifier of a task to a step number
  (define (sched ident)
    (match index
      ['step (memq ident mapping)]
      ['task (list-ref mapping ident)]))
  ; take a step number to an integer identifier of a task
  (define (trace step)
    (match index
      ['step (list-ref mapping step)]
      ['task (memq step mapping)]))

  ; encode a task symbol as an integer identifier
  (define (encode task)
    (vector-memq task tasks))
  ; decode an integer identifier as a task symbol
  (define (decode ident)
    (vector-ref tasks ident))

  ; create a unique, bounded existential variable for every step or task
  ; assignment
  (define mapping
    (for/fold ([mapping null])
              ([x (in-range (vector-length tasks))])
      (define-symbolic* y integer?)

      ; weirdly, splitting this conjunction totally wrecks performance
      (assert (and (<= 0 y)
                   (< y (vector-length tasks))))

      ; explicitly constrain every step to be unique
      (when hint-uniqueness
        (for ([y0 mapping])
          (assert (not (eq? y y0)))))

      (cons y mapping)))

    (list encode decode sched trace))

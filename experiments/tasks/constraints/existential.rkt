#lang rosette

(provide init-tracer)

; traces as relational constraints (existential inverse uninterpreted functions)
;
; Encode the trace using mutually inverted uninterpreted functions in Z3. Since
; Rosette does not support custom axioms, we emit appropriate assertions to
; guarantee the inversion. One may equivalently use existential variables for
; step values or for task assignment values (i.e., integer identifiers).

; Generate functions to encode/decode schedule attributes as integer identifiers
; and to map between these integer identifiers and step numbers.
(define (init-tracer existential-steps existential-task hint-uniqueness tasks)
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

  ; create a unique, bounded existential variable for every step
  (when existential-steps
    (for/fold ([steps null])
              ([ident (in-range (vector-length tasks))])

      (define-symbolic* step integer?)

      ; weirdly, splitting this conjunction totally wrecks performance
      (assert (and (<= 0 step)
                   (< step (vector-length tasks))))

      ; big performance win by conjoining these two as well
      (assert (and (eq? (sched ident) step)
                   (eq? (trace step) ident)))

      ; explicitly constrain every step to be unique
      (when hint-uniqueness
        (for ([prev steps])
          (assert (not (eq? step prev)))))
      (cons step steps)))

  ; create a unique, bounded existential variable for every identifier assignment
  (when existential-task
    (for/fold ([idents null])
              ([step (in-range (vector-length tasks))])

      (define-symbolic* ident integer?)

      ; weirdly, splitting this conjunction totally wrecks performance
      (assert (and (<= 0 ident)
                   (< ident (vector-length tasks))))

      ; big performance win by conjoining these two as well
      (assert (and (eq? (sched ident) step)
                   (eq? (trace step) ident)))

      ; explicitly constrain every identifier assignment to be unique
      (when hint-uniqueness
        (for ([prev idents])
          (assert (not (eq? ident prev)))))
      (cons ident idents)))

  (list encode decode sched trace))

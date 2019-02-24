#lang rosette

(require rosette/lib/synthax
         rosette/lib/angelic)

(provide trace-symbolic
         trace-schedule
         check-schedule
         verify-schedule)

; schedule synthesis with parametric tracer

; use the theory of integers for scalability, since this is really integer
; linear programming (ILP)
(current-bitwidth #f)

; symbolically trace all feasible schedules
(define (trace-symbolic tracer depends)
  (match-define (list encode _ sched _) tracer)

  ; constrain each attribute's step to succeed its dependencies'
  (for ([task-dependencies depends])
    (let* ([task (car task-dependencies)]
           [step (sched (encode task))]
           [dependencies (cdr task-dependencies)])
      (for ([dependency dependencies])
        (assert (< (sched (encode dependency)) step))))))

; concretely trace a schedule
(define (trace-schedule tracer schedule)
  (match-define (list encode _ sched trace) tracer)

  ; record which attribute we compute at each step of the schedule
  (for ([slot schedule]
        [step (in-range (length schedule))])
    (let ([ident (encode slot)])
      (assert (eq? (sched ident) step))
      (assert (eq? (trace step) ident)))))

; Assert that the schedule can produce the global trace and return the schedule.
; This method is not optimal, but it is presented here as the "obvious" way of
; checking a schedule against a trace.
(define (check-schedule tracer schedule)
  (match-define (list encode decode _ trace) tracer)

  (for ([slot schedule]
        [step (in-range (length schedule))])
    ; If the slot is a symbolic choice, then this constraint will be the
    ; disjunction of equalities for every guarded value! You could also write the
    ; constraint like below, but you will have the same problem.
    ;(assert (eq? (encode slot) (trace step)))
    (assert (eq? slot (decode (trace step)))))

  schedule)

; Assert that the syntactic sketch is consistent with the global trace and return
; a symbolic schedule whose solution will be consistent with sketch. N.B. that
; this is equivalent to check-schedule but modified to avoid equality constraints
; with symbolic unions, which expand into large disjunctions.
(define (verify-schedule tracer sketch)
  (match-define (list encode decode _ trace) tracer)

  (for/list ([slot sketch]
             [step (in-range (length sketch))])
    (let ([fill (trace step)])
      ; maybe generate an assertion
      (cond
        [(list? slot)
         (assert (apply ||
                        (for/list ([task slot])
                          (eq? (encode task) fill))))]
        [(not (eq? slot '?))
         ; the overall solution must be consistent with the sketch
         (for/all ([task slot])
           (assert (eq? (encode task) fill)))])
      fill)))

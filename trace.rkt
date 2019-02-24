#lang rosette

; A symbolic data structure for static scheduling of statements with varying
; static and dynamic contexts.

(require "utility.rkt"
         rosette/solver/mip/cplex
         rosette/solver/smt/z3
         data/interval-map)

(define solver
  (if (cplex-available?)
      (cplex)
      (z3 #:logic 'QF_LIA)))

(provide (rename-out [choose trace-choose]
                     [choice? trace-choice?]
                     [fork-trace trace-fork]
                     [start-trace trace-start]
                     [thread-trace trace-thread]
                     [step-trace trace-step]
                     [abstract-alloc trace-alloc]
                     [abstract-read trace-read]
                     [abstract-write trace-write]
                     [stop-trace trace-stop]
                     [solve-sketch trace-solve]))

; TODO: Implement a "symbolic evaluation" with respect to the abstract memory
; contents, so that the interpretation of statements need not be modified for
; this language. At the simplest, this would mean any primitive operation on
; these abstract values results in another abstract value (preserving zero new
; information from the operation), requiring all paths to be explored. This feels
; like legitimate abstract interpretation, so it may be useful to read up on that.

; Interaction with the symbolic trace happens as follows:
; trace-choose *
; {
;   trace-start
;   [
;     trace-thread
;     trace-alloc
;     trace-read
;     trace-write
;     trace-step
;     trace-fork
;   ] *
;   trace-stop
; } *
; trace-solve *

; List of guard variables such that this path executes iff they all equal 1.
(define guard-condition null)

; The next program counter.
(define program-counter 0)

; Interval map that associates each program counter to another interval map that
; associates other program counters to #t if in parallel with the first.
(define parallel-table (void))

; Mutable hash associating each memory location to a list of a pair of a program
; counter and guard variable for every possible read. These read possibilities
; are not disjoint; multiple may happen to the same memory location.
(define read-table (void))

; Mutable hash associating each memory location to a list of a pair of a program
; counter and guard variable for every possible write. Each write possibility is
; disjoint; at most one write may ever happen to a particular memory location.
(define write-table (void))

; Symbolic choice, e.g., a program hole, where the alternative whose associated
; binary variable, its guard variable, is equal to 1 is chosen. At creation time,
; the sum of these guard variables is asserted to be 1.
(struct choice (alternatives guards) #:transparent)

; Symbolically choose one of the statements.
(define (choose . alternatives)
  (define guards
    (for/list ([_ alternatives])
      (define-symbolic* g integer?)
      (assert (>= g 0)) ; NOTE: This is the default lower bound in CPLEX.
      g))
  (assert (= 1 (apply + guards)))
  (choice alternatives guards))

; Concretize the symbolic choice according to the provided model.
(define (concretize model choice)
  (if (choice? choice)
      (for/last ([alternative (choice-alternatives choice)]
                 [guard (choice-guards choice)]
                 #:final (= (evaluate guard model) 1))
        alternative)
      choice))

; Perform abstract interpretation for each symbolic branch of the program hole.
(define-syntax fork-trace ; let/choice
  (syntax-rules ()
    [(fork-trace ([x e]) body ...)
     (let ([choice e])
       (if (choice? choice)
           (for ([value (choice-alternatives choice)]
                 [guard (choice-guards choice)])
             (set! guard-condition (cons guard guard-condition))
             (let ([x value])
               body ...)
             (set! guard-condition (rest guard-condition)))
           (let ([x choice])
             body ...)))]
    [(fork-trace (binder bindings ...) body ...)
     (fork-trace (binder)
                 (fork-trace (bindings ...) body ...))]))

; The conjunction of a list of binary variables.
(define conjunction-cache (make-hash))
(define (conjoin . bs)
  (let ([n (length bs)])
    (cond
      [(= n 0) 1]
      [(= n 1) (first bs)]
      [else
       ; Since Rosette symbolic variables are only distinguishable by `eqv?`, a
       ; `seteqv` is used as the key for the conjunction cache.
       (hash-ref! conjunction-cache
                  (apply seteqv bs)
                  (thunk
                   (define-symbolic* bc integer?)
                   (assert (>= bc 0)) ; NOTE: This is the default lower bound in CPLEX.
                   (for ([b bs])
                     (assert (<= bc b)))
                   (assert (>= bc (- (apply + (cons 1 bs)) n)))
                   bc))])))

; Initialize internal state to start a new trace of the current sketch.
(define (start-trace)
  (set! program-counter 0)
  (set! guard-condition null)
  (set! parallel-table (make-interval-map))
  (set! read-table (make-hasheq))
  (set! write-table (make-hasheq)))

; Record that two threads ran in parallel, to later check data independence.
(define-syntax-rule (thread-trace e1 e2)
  (let ([start program-counter])
    e1
    (let ([split program-counter])
      e2
      (let ([stop program-counter])
        ; We only need to record parallel exclusion in one direction, because the
        ; other is already handled by the usual temporal ordering.
        (interval-map-update*! parallel-table
                               split
                               stop
                               (λ (m) (interval-map-set! m start split #t) m)
                               make-interval-map)))))

; Advance the program by one step.
(define (step-trace)
  (set! program-counter (+ program-counter 1)))

; Generate an opaque, abstract memory location.
(define (abstract-alloc)
  (box (void)))

; Record an abstract memory read at this program counter under this guard condition.
(define (abstract-read location)
  (hash-update! read-table
                location
                (curry cons (cons program-counter
                                  (apply conjoin guard-condition)))
                null))

; Record an abstract memory write at this program counter under this guard condition.
(define (abstract-write location)
  (hash-update! write-table
                location
                (curry cons (cons program-counter
                                  (apply conjoin guard-condition)))
                null))

; Finish the current trace of the sketch and generate all resultant constraints,
; taking a list of input and (desired) output memory locations.
(define (stop-trace inputs outputs) ; TODO: Remove input designation.
  (let ([output-set (list->seteq outputs)]
        [input-set  (list->seteq inputs)])

    ; Assert that necessary writes happen the correct number of times.
    (for ([(location possible-writes) write-table])
      (let ([write-possibility (apply + (map cdr possible-writes))])
        (cond
          ; Assert that input memory locations are never overwritten.
          [(set-member? input-set location)
           (assert (= write-possibility 0))]
          ; Assert that an output memory location are written exactly once.
          [(set-member? output-set location)
           (assert (= write-possibility 1))]

          ; Assert that an optional memory location is written at most once.
          [else
           (assert (<= write-possibility 1))])))

    (for ([(location possible-reads) read-table]
          #:unless (set-member? input-set location)
          [possible-read possible-reads])
      (match-let ([(cons program-counter guard-variable) possible-read]
                  [possible-writes (hash-ref write-table location null)])

        ; For each possible read, find all its antidependncies.
        ; Find the interval map of parallel exclusion.
        (define parallel
          (interval-map-ref parallel-table program-counter make-interval-map))

        ; Whether a write possibility is safe w.r.t. this read possibility.
        (define (safe? possible-write)
          (and (< (car possible-write) program-counter)
               (not (interval-map-ref parallel (car possible-write) #f))))

        ; Partition all possible writes by safety w.r.t. this read possibility.
        (define-values (dependences antidependences)
          (partition safe? possible-writes))

        ; Assert that the set of dependences is implied by this read
        ; possibility, unless the location is an output location.
        (cond
          [(null? dependences)
           (assert (= guard-variable 0))] ; clearly this read is impossible
          [(not (set-member? output-set location))
           (let ([implied-variables (map cdr dependences)])
             (assert (>= (apply + implied-variables) guard-variable)))])

        ; Assert that the set of antidependences is avoided.
        (unless (null? antidependences) ; skip degenerate cases
          (let ([conflict-variables (map cdr antidependences)])
            (assert (<= (apply + (cons guard-variable conflict-variables)) 1))))))))

; After one or more traces (following the construction of a sketch), solve for an
; assignment of holes to sketches, where #f is interpreted as a no-op (and
; optionally replaced by such a statement if desired).
(define (solve-sketch)
  (define-symbolic* objective integer?)
  (solver-clear solver)
  (solver-minimize solver (list objective))
  (solver-assert solver (asserts))
  (define model (solver-check solver))

  ; Extract the solved schedule, a mapping from holes to statements.
  (and (sat? model) (curry concretize model)))

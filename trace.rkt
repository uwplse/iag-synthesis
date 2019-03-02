#lang rosette

; A symbolic data structure for static scheduling of statements with varying
; static and dynamic contexts.

(require "utility.rkt"
         rosette/solver/mip/cplex
         rosette/solver/smt/z3)

(provide permute iter-permuted for/permuted join-threads join
         table? make-table table-ref! table-def! break (rename-out [solve-trace solve]))

; Activate an ILP solver (IBM CPLEX if available, otherwise Z3 in ILP mode)
(current-solver
  (if #f ;(cplex-available?) ; Rosette encodes for CPLEX very, very slowly...
      (cplex)
      (z3 #:logic 'QF_LIA)))

(struct traced (dependency antidependency) #:transparent)

(define store (make-hash)) ; TODO: Maybe split into dependency and antidependency maps

; Next available memory location for a table entry.
(define next-location 0)

; List of assumed bit-encoded conditions.
(define path-condition null)

; Generate a fresh binary integer symbol.
(define (bit*)
  (define-symbolic* b integer?)
  (assert (<= 0 b 1))
  b)

; Symbolic permutation of length at most k
(struct permuted (guards elements) #:transparent)

; Symbolically choose a permutation of length at most k.
(define (permute k elements)
  (let* ([n (length elements)]
         [matrix (build-matrix k n (const* bit*))])
    (for ([row (matrix-rows matrix)])
      (assert (<= (vector-sum row) 1))) ; distinct choice
    (for ([column (matrix-columns matrix)])
      (assert (<= (vector-sum column) 1))) ; unique choice
    (permuted matrix elements)))

; Instantiate a symbolic value (to include a symbolic permutation) according
; to a solved model.
(define (concretize model)
  (define (subst value)
    (match value
      [(permuted guard-matrix elements)
       (for/list ([guards (matrix-rows guard-matrix)]
                  #:when #t
                  [guard guards]
                  [element elements]
                  #:when (= (evaluate guard model) 1))
         element)]
      [(? list?) (map subst value)]
      [(? vector?) (vector-map subst value)]
      [(? struct?) (struct-map subst value)]
      [_ (evaluate value model)]))
  subst)


; Opaque, user-facing handle on a table.
(struct table (contents) #:mutable)

; Reset the table for a fresh, disjoint execution path.
(struct cmd:alloc (table) #:transparent)

; Abstract read from the named entry in the table.
(struct cmd:read (table name) #:transparent)

; Abstract write to the named entry in the table.
(struct cmd:write (table name) #:transparent)

; Compose two residual programs in parallel.
(struct cmd:join (left right) #:transparent)

; Assume a guard condition inside a body residual program.
(struct cmd:assume (guard body) #:transparent)

; Reverse block of residualizing program.
(define residue null)

; Quiescence is when the program is in a concrete, sequential state.
(define quiescent? #t)

(define ((residualize do-proc) . args)
  (shadow! ([quiescent? #f]
            [residue null])
    (apply do-proc args)
    (reverse residue)))

; Symbolically evaluate the body for each alternative of each position of the
; permutation.
(define (iter-permuted perm do-body)
  ; Evaluate the body for each alternative of the permutation, saving
  ; the resulting residual program frames, duplicating each for each
  ; possible occurrence in a concrete permutation.
  (let ([blocks (map (residualize do-body) (permuted-elements perm))])
    (for ([guards (matrix-rows (permuted-guards perm))])
      (for ([guard guards]
            [block blocks])
        (push! residue (cmd:assume guard block))))
    (flush-residue!)))

; Syntactic sugar for iter-permuted.
(define-syntax for/permuted
  (syntax-rules ()
    [(for/permuted ([name expr]) body ...)
     (let ([value expr])
       (if (permuted? value)
           (iter-permuted value (λ (name) body ...))
           (for ([name value])
             body ...)))]
    [(for/permuted (binder bindings ...) body ...)
     (for/permuted (binder)
       (for/permuted (bindings ...)
         body ...))]))

; Record that two threads ran concurrently, to later check data independence.
(define (join-threads do-left do-right)
  (let ([left-block ((residualize do-left))]
        [right-block ((residualize do-right))])
    (push! residue (cmd:join left-block right-block))
    (flush-residue!)))

; Syntactic sugar for join-threads.
(define-syntax join
  (syntax-rules ()
    [(join body)
     body]
    [(join body bodies ...)
     (join-threads (thunk body) (thunk (join bodies ...)))]))

; Create an empty table.
(define (make-table)
  (let ([table (table -1)])
    (push! residue (cmd:alloc table))
    (flush-residue!)
    table))

; Reference the named entry in the table.
(define (table-ref! table name)
  (push! residue (cmd:read table name))
  (flush-residue!))

; Define (and initialize) the named entry on the table.
(define (table-def! table name)
  (push! residue (cmd:write table name))
  (flush-residue!))

; Create the conjunction of a list of binary variables.
(define conjoin*
  ; Since Rosette symbolic variables are only distinguishable by `eqv?`, we use
  ; `seteqv` to key the memo table.
  (let ([memo (make-hash)])
    (match-lambda
      [(list) 1]
      [(list x) x]
      [(list x xs ...)
       (hash-ref! memo
                  (apply seteqv xs)
                  (thunk
                   (let ([y (conjoin* xs)]
                         [w (bit*)]
                         [z (bit*)])
                     (assert (<= z x))
                     (assert (<= z y))
                     (assert (= (+ w z) (+ x y)))
                     z)))])))

(define (trace-read! location assumption store)
  (match-let ([(traced dependency antidependency) (hash-ref store location (traced 0 1))])
    (when (eqv? assumption 1)
      (assert (= dependency 1)))
    (assert (<= 0 dependency 1))
    (assert (<= assumption dependency))))

(define (trace-write! location assumption store)
  (hash-update! store
                location
                (match-lambda
                  [(traced dependency antidependency)
                   (traced (+ dependency assumption)
                           (- antidependency assumption))])
                (traced 0 1)))

(define (trace-exit! store)
  (for ([tr (hash-values store)])
    (match-let ([(traced dependency antidependency) tr])
      (assert (<= 0 dependency 1))
      (assert (<= 0 antidependency 1)))))

; Evaluate residual program commands.
(define (evaluate-residue! program [shared-stores null])
  (for ([command program])
    (match command
      [(cmd:assume guard body)
       (push! path-condition guard)
       (evaluate-residue! body shared-stores)
       (pop! path-condition)]
      [(cmd:alloc table)
       (set-table-contents! table (make-hash))]
      [(cmd:read (table contents) name)
       (let ([location (hash-ref! contents name (thunk (++ next-location)))]
             [assumption (conjoin* path-condition)])
         (trace-read! location assumption store))]
      [(cmd:write (table contents) name)
       (let ([location (hash-ref! contents name (thunk (++ next-location)))]
             [assumption (conjoin* path-condition)])
         (trace-write! location assumption store)
         (for-each (curry trace-write! location assumption) shared-stores))]
      [(cmd:join left right)
       (let ([initial (hash-copy store)])
         (evaluate-residue! left shared-stores)
         (let ([final store])
           (shadow! ([store initial])
             (evaluate-residue! right (cons final shared-stores)))))])))

; Evaluate the accumulated residual program if in a quiescent state
(define (flush-residue!)
  (when quiescent?
    (evaluate-residue! (reverse residue))
    (set! residue null)))

; Reset the state of symbolic tracing.
(define (break)
  (evaluate-residue! (reverse residue))
  (set! residue null)
  (unless quiescent?
    (error 'break "Cannot break trace within nested operation"))
  ;(trace-exit! store)
  (set! store (make-hash))
  (set! next-location 0))

; After one or more traces, solve for an assignment of each multichoice to an
; appropriately sized list of its alternatives.
(define (solve-trace)
  (let ([model (optimize #:minimize (list (bit*)) #:guarantee #t)])
    ; Extract the solved schedule, a mapping from holes to statements
    (and (sat? model) (concretize model))))

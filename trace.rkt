#lang rosette

; A symbolic data structure for static scheduling of statements with varying
; static and dynamic contexts.

(require "utility.rkt" "partition.rkt"
         rosette/solver/mip/cplex
         rosette/solver/smt/z3
         data/interval-map)

(provide multichoose multichoice? multifork for/multichoice
         step thread empty read write output break (rename-out [solve-trace solve]))

; Activate an ILP solver (IBM CPLEX if available, otherwise Z3 in ILP mode)
(current-solver
  (if (cplex-available?)
      (cplex)
      (z3 #:logic 'QF_LIA)))

; Dummy objective variable for the ILP solver
(define-symbolic objective integer?)

; Interaction with the symbolic trace should match the following state machine:
; multichoose *
; (
;   ( multifork | thread | empty | read | write | step ) *
;   output *
;   break
; ) *
; solve *

(struct memory (inflow outflow timestamp) #:mutable #:transparent)

(define store-count 0)

(define read-table (make-hash))
(define write-table (make-hash))

(define output-set (mutable-set))

(define program-counter 0)

(define path-condition null)

(define parallel-spawns null)

(define parallel-table (make-interval-map))

(define parallel-stack null)

;(define parallel-frames null)

; Symbolic multichoice, e.g., a sequence of program holes, where alternative i is
; chosen for position j iff the binary variable v_(j,i) is equal to 1. The matrix
; of binary variables are stored in row-major form (i.e., indexed by position and
; then by alternative).
(struct multichoice (alternatives variables) #:transparent)

(define (vector-sum vect)
  (for/fold ([sum 0])
            ([elem vect])
    (+ elem sum)))

; Symbolically choose n of the given values.
(define (multichoose n options) ; FIXME: Separate default option
  (define k (length options))
  
  ; Construct the guard matrix of binary variables.
  (define matrix
    (for/vector #:length n ([_ (in-range n)])
      (for/vector #:length k ([_ options])
        ; If this guard variable is 1, then this alternative is chosen for this
        ; position of the list.
        (define-symbolic* guard integer?)
        (assert (<= guard 1)) ; NOTE: technically redundant
        (assert (>= guard 0)) ; NOTE: technically redundant to CPLEX but not to Z3
        guard)))

  ; Each row must sum to 1 so that each position is assigned exactly one
  ; alternative.
  (for ([i (in-range n)])
    (let ([row (vector-ref matrix i)])
      (assert (= (vector-sum row) 1))))

  ; Each (non-default) column must sum to 1 so that each position is assigned either
  ; a unique alternative or the default.
  (for ([j (in-range 1 k)])
    (let ([column (for/vector ([row matrix]) (vector-ref row j))])
      (assert (<= (vector-sum column) 1))))

  (multichoice options matrix))

; Given a multichoice, return the list of alternatives chosen by the model.
(define (multichosen model)
  (match-lambda
    [(multichoice options guard-matrix)
     (for/list ([guards guard-matrix])
       (for/last ([guard guards]
                  [option options]
                  #:final (= (evaluate guard model) 1))
         option))]
    [value value]))


; User-facing handle on a store.
(struct handle (index) #:mutable)

; Initialize the store for use (i.e., grab an index).
(struct residual-alloc (store) #:transparent)

; Abstract read from the named location in the store.
(struct residual-read (store name) #:transparent)

; Abstract write to the named location in the store.
(struct residual-write (store name) #:transparent)

; Advance the program counter.
(struct residual-step () #:transparent)

; Begin a parallel block, i.e., a sequence of thread spawns followed by a join.
(struct residual-parallel () #:transparent)

; Spawn a new thread as part of the most recent parallel block.
(struct residual-spawn () #:transparent)

; Join all threads in the most recent parallel block.
(struct residual-join () #:transparent)

; Enter a guarded section for the given guard variable.
(struct residual-enter (variable) #:transparent)

; Exit the most recent guarded section.
(struct residual-exit () #:transparent)


; A frame of the residual program is the queue of pending operations under
; symbolic evaluation for the innermost symbolic fork.
(define residual-frame null)

; Stack of frames of the residual program for the outer symbolic forks.
(define residual-stack null)

(define (append-residual . residual-commands)
  (set! residual-frame (append residual-commands residual-frame)))


(define (multifork multichoice continuation)
  ; Push a new frame of the residual program
  (set! residual-stack (cons residual-frame residual-stack))

  ; Evaluate the body for each alternative of the multichoice, saving
  ; the resulting residual program frames.
  (define frames
    (for/list ([alternative (multichoice-alternatives multichoice)])
      ; New frame of the residual program.
      (set! residual-frame null)
      ; Evaluate the program for this particular alternative.
      (continuation alternative)
      ; Accumulate the residual program frame.
      residual-frame))

  ; Pop the old frame of the residual program.
  (set! residual-frame (first residual-stack))
  (set! residual-stack (rest residual-stack))

  ; Duplicate the residual program frames for each alternative of the
  ; multichoice at each position in the multichoice sequence under an
  ; appropriate guard.
  (for ([variables (multichoice-variables multichoice)])
    (for ([variable variables]
          [frame frames])
      (append-residual (residual-enter variable))
      (apply append-residual frame)
      (append-residual (residual-exit))))
  (interpret-residual))

; Symbolically evaluate the body for each alternative of each position of the
; multichoice.
(define-syntax for/multichoice
  (syntax-rules ()
    [(for/multichoice ([name expr]) body ...)
     (let ([value expr])
       (if (multichoice? value)
           (multifork value (λ (name) body ...))
           (let ([name value])
             body ...)))]
    [(for/multichoice (binder bindings ...) body ...)
     (for/multichoice (binder)
                      (for/multichoice (bindings ...)
                                       body ...))]))

; Advance the program by one step.
(define (step)
  (append-residual (residual-step))
  (interpret-residual))

; Record that two threads ran in parallel, to later check data independence.
(define-syntax-rule (thread expr ...)
  (begin
    (append-residual (residual-parallel))
    (for ([cont (list (thunk expr) ...)])
      (append-residual (residual-spawn))
      (cont))
    (append-residual (residual-join))
    (interpret-residual)))

; Create an empty store.
(define (empty)
  (let ([store (handle -1)])
    (append-residual (residual-alloc store))
    (interpret-residual)
    store))

; Register a read from the named location in the store.
(define (read store name)
  (append-residual (residual-read store name))
  (interpret-residual))

; Register a write to the named location in the store.
(define (write store name)
  (append-residual (residual-write store name))
  (interpret-residual))

; Designate the named location in the store as a global program output.
(define (output store name)
  (unless (and (null? path-condition) (null? residual-stack))
    (raise-user-error 'trace "cannot output location inside symbolic fork"))
  (set-add! output-set (cons (handle-index store) name)))


; The conjunction of a list of binary variables.
(define conjunction-cache (make-hash))
(define (conjoin . variables)
  (match (length variables)
    [0 1]
    [1 (first variables)]
    [count
     ; Since Rosette symbolic variables are only distinguishable by `eqv?`, a
     ; `seteqv` is used as the key for the conjunction cache.
     (hash-ref! conjunction-cache
                (apply seteqv variables)
                (thunk
                 (define-symbolic* conjunct integer?)
                 (assert (<= conjunct 1)) ; NOTE: technically redundant
                 (assert (>= conjunct 0)) ; NOTE: technically redundant to CPLEX but not to Z3
                 (for ([variable variables])
                   (assert (<= conjunct variable)))
                 (assert (>= (+ conjunct count) (cons 1 (apply + variables))))
                 conjunct))]))


; Evaluate the residual program.
(define (interpret-residual)
  (when (null? residual-stack)
    (for ([residual-command (reverse residual-frame)])
      (match residual-command
        [(residual-enter guard)
         (set! path-condition (cons guard path-condition))]
        [(residual-exit)
         (set! path-condition (rest path-condition))]
        [(residual-alloc store)
         (set-handle-index! store (++ store-count))]
        [(residual-read (handle index) name)
         (hash-update! read-table
                       (cons index name)
                       (curry cons (cons program-counter
                                         (apply conjoin path-condition)))
                       null)]
        [(residual-write (handle index) name)
         (hash-update! write-table
                       (cons index name)
                       (curry cons (cons program-counter
                                         (apply conjoin path-condition)))
                       null)]
        [(residual-parallel)
         (set! parallel-stack (cons parallel-spawns parallel-stack))
         (set! parallel-spawns null)]
        [(residual-spawn)
         (set! parallel-spawns (cons program-counter parallel-spawns))]
        [(residual-join)
         ; Record that each "later" thread cannot depend on any "earlier" thread.
         (let ([start (last parallel-spawns)])
           (for/fold ([stop program-counter])
                     ([split parallel-spawns]
                      #:break (= split start))
             (interval-map-update*! parallel-table
                                    split
                                    stop
                                    (λ (m) (interval-map-set! m start split #t) m)
                                    make-interval-map)))
         (set! parallel-spawns (first parallel-stack))
         (set! parallel-stack (rest parallel-stack))]
        [(residual-step)
         (set! program-counter (++ program-counter))]))
    (set! residual-frame null)))

(define (analyze-writes location counter)
  (let ([possible-writes (hash-ref write-table location null)]
        [parallel (interval-map-ref parallel-table counter make-interval-map)])
    (for/fold ([dependences null]
               [antidependences null])
              ([possible-write possible-writes])
      (if (and (< (car possible-write) counter)
               (not (interval-map-ref parallel (car possible-write) #f)))
          (values (cons (cdr possible-write) dependences)
                  antidependences)
          (values dependences
                  (cons (cdr possible-write) antidependences))))))

; Finish the current trace of the sketch and generate all resultant constraints,
; taking a list of input and (desired) output memory locations.
(define (generate-constraints)
  ; Assert that necessary writes happen the correct number of times.
  (for ([(location possible-writes) write-table])
    (let ([write-possibility (apply + (map cdr possible-writes))])
      ; Assert that each memory location is written at most once.
      (if (set-member? output-set location)
          (assert (=  write-possibility 1))
          (assert (<= write-possibility 1)))))

  (for* ([(location possible-reads) read-table]
         [possible-read possible-reads])
    (let*-values ([(counter) (car possible-read)]
                  [(guard) (cdr possible-read)]
                  [(dependences antidependences) (analyze-writes location counter)])

      ; Each read to a location must succeed some write to that location
      (unless (or (set-member? output-set location)
                  (eqv? dependences (list 1)))
        (assert (>= (apply + dependences) guard)))

      ; Each read must not precede any write to that location.
      (unless (null? antidependences) ; (<= guard 1) is unnecessary
        (assert (<= (apply + (cons guard antidependences)) 1))))))

(define (break)
  (generate-constraints)
  ; Reset the interpreter state.
  (set! program-counter 0)
  (set! path-condition null))

; After one or more traces, solve for an assignment of each multichoice to an
; appropriately sized list of its alternatives.
(define (solve-trace)
  (let ([model (optimize #:minimize (list objective) #:guarantee (>= objective 0))])
    ; Extract the solved schedule, a mapping from holes to statements
    (and (sat? model) (multichosen model))))

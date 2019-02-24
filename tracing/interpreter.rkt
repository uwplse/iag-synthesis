#lang rosette

; Tracing interpreter for language of tree traversal schedules

(require "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "../trace.rkt")

(provide (all-defined-out))

(define (interpret grammar schedule tree)
  (match schedule
    [(sched-comp 'seq left-schedule right-schedule)
     (interpret grammar left-schedule tree)
     (interpret grammar right-schedule tree)]
    [(sched-comp 'par left-schedule right-schedule)
     (trace-thread (interpret grammar left-schedule tree)
                   (interpret grammar right-schedule tree))]
    [(sched-trav 'pre visits)
     (preorder (make-visitor grammar visits) tree)]
    [(sched-trav 'post visits)
     (postorder (make-visitor grammar visits) tree)]))

(define (preorder visitor tree)
  (visitor tree)
  (for* ([child (tree-children tree)]
         [node (listify (cdr child))])
    (preorder visitor node)))

(define (postorder visitor tree)
  (for* ([child (tree-children tree)]
         [node (listify (cdr child))])
    (postorder visitor node))
  (visitor tree))

(define ((make-visitor grammar visits) self)
  (let ([slots (cdr (assq (tree-class self) visits))]
        [class-ast (get-class grammar (tree-class self))])
    (for ([slot* slots])
      ; This is a domain-specific version of Rosette's for/all
      (trace-fork ([slot slot*])
        (interpret-slot class-ast self slot)))))

; Record an assignment to the local attribute name label (e.g., 'y) on the node
; named by object (e.g., 'left) relative to a particular node self, where the
; currently indexed node is provided (if this assignment happens within a loop).
(define (assign self object label indexed)
  (let ([target (ag-expr-reference object #f label)]
        [child (and indexed object)])
    (trace-write (tree-load self child #f indexed target))
    (trace-step))) ; Increment the program counter.

; Interpret a schedule slot, which is either 'nop or (object . label) for some
; node name object (e.g., 'self) and local attribute name label (e.g., 'x).
(define (interpret-slot class-ast self slot)
  (unless (eq? slot 'nop)
    (match-let ([(cons object target) slot])
      (if (list? target)
          (interpret-loop class-ast self object target)
          (interpret-stmt class-ast self object target)))))

; Interpret a statement loop, which is a list of attributes corresponding to
; iterated (fold) evaluation rules.
(define (interpret-loop class-ast self loop-object subslots)
  (let ([children (tree-object self loop-object)]
        [virtual (make-virtual-node self loop-object)])
    ; TODO: The virtual state should be an association list, where each
    ; individual subslot allocates its initial and intermediate accumulators
    ; and has some mechanism to resolve any name to a unique but potentially
    ; unallocated memory location...

    ; What if we recast the symbolic state as attempting to find satisfying
    ; solutions for executions inducing a dependency relation on resource
    ; allocation and initialization? What about events more generally?

    ; The problem that we've run into comes down to insufficient abstraction of
    ; allocation. Currently, we've assumed that the trace's abstraction of
    ; allocation will simply record that the memory location can only be written
    ; (and subsequently read) after its allocation; allocation and initialization
    ; essentially implement dependencies with degenerate versions. We need to
    ; completely abstract the details of allocation under symbolic evaluation
    ; from the object interpreter. There's a few, perhaps complementary
    ; approaches to take:
    ;   1. Provide a *symbolic environment* abstraction that manages the mapping
    ;      from names to memory locations. Allocation is the creation (and
    ;      initialization?) of a new name.
    ;   2. Provide versioned writes, so that a single memory location, presumably
    ;      the final destination, can carry all intermediate dependencies through
    ;      itself. This will require checking real antidependencies, caused by
    ;      reads and/or writes of the wrong version (unlike symbolic
    ;      environments?).
    ;
    ; In short, sufficiently abstracting the cross-iteration interaction permits
    ; the symbolic trace to reason automatically about the object program's loop
    ; invariants concerning dependencies among reads and writes to abstract
    ; memory locations. With this ability, the symbolic trace need only evaluate
    ; each possible iteration once in order to deduce its precise pre- and post-
    ; conditions.
    ;
    ; At the level of implementation, what matters is that the program under
    ; symbolic evaluation can speak to the symbolic trace in terms of a global
    ; set of names.
    ;
    ; Given that the above works, we still need a way to construct a multichoice
    ; of *independent* multichoices...

    ; Example snippet of fold implementation using the abstractions described
    ; above?
    ;; (define initial (trace-environment))
    ;; (trace-multifork ([statement initial-statement-list])
    ;;   ...
    ;;   (trace-alloc initial attribute)
    ;;   (trace-write (trace-lookup initial attribute))
    ;;   ...)
    ;; (for/fold ([previous initial]
    ;;           ([indexed children])
    ;;   (define virtual (trace-environment))
    ;;   (trace-multifork ([statement step-statement-list])
    ;;     ...
    ;;     (trace-read (trace-lookup previous 'x))
    ;;     (trace-write (trace-lookup virtual 'x))
    ;;     (when (has? indexed attribute)
    ;;       (trace-read (trace-lookup virtual 'x)))
    ;;       (trace-write (node-lookup indexed 'x)))
    ;;     ...)
    ;;   virtual)

    ; Initialize the virtual node of initial accumulators
    (for ([subslot* subslots])
      (trace-fork ([subslot subslot*])
        (unless (equal? subslot 'nop)
          (match-let ([(cons object label) subslot])
            (interpret-stmt class-ast self object label #f virtual)))))

    ; Perform the actual iterated evaluation.
    (for/fold ([previous virtual])
              ([indexed children])
      (for ([subslot* subslots])
        (trace-fork ([subslot subslot*])
          (unless (equal? subslot 'nop)
            (match-let ([(cons object label) subslot])
              (interpret-stmt class-ast self object label previous indexed)))))
      indexed)))

; Interpret a statement, which is an attribute corresponding to an evaluation
; rule. If the evaluation rule is iterated, then the previous and indexed
; child nodes are supplied as optional arguments.
(define (interpret-stmt class-ast self object label [previous #f] [indexed #f])
  (let ([dereference (curry tree-load self (and indexed object) previous indexed)])
    (match (ag-rule-right (lookup-rule (ag-class-rules class-ast) object label))
      [(ag-loop _ (ag-fold init-expr iter-expr))
       (interpret-expr dereference (if previous iter-expr init-expr))
       (assign self object label indexed)]
      [(or (ag-loop _ expr) expr)
       (unless (and previous (not indexed))
         (interpret-expr dereference expr)
         (assign self object label indexed))])))

; Abstract interpretation of an expression using the supplied function to
; resolve attributes to locations.
(define (interpret-expr dereference expression)
  (let ([recur (curry interpret-expr dereference)])
    (match expression
      [(ag-expr-unary _ operand)
       (recur operand)]
      [(ag-expr-binary left _ right)
       (recur left)
       (recur right)]
      [(ag-expr-condition cond then else)
       (recur cond)
       (recur then)
       (recur else)]
      [(ag-expr-call _ arguments)
       (for-each recur arguments)]
      [(? ag-expr-reference?)
       (trace-read (dereference expression))]
      [(or (? number?) (? boolean?) (? string?))
       (void)])))

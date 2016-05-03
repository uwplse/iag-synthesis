#lang rosette

; Functional Tree Language (FTL) intepreter
; Schedules

(require "../core/utility.rkt"
         "../core/grammar.rkt"
         "../core/tree.rkt"
         "../compile/parse.rkt"
         "../compile/generate.rkt"
         "syntax.rkt")

(provide ftl-schedule-interpret
         ftl-schedule-evaluate
         ftl-schedule-validate)

; Incremental grammars may simply require the following inference rule in
; combination with a dirty bit predicate expression:
;         Γ |- t.α : τ
; --------------------------
; Γ |- ι[t.α] : τ, Δ[t.α] : 2

; NOTE: validation of update/partial schedules will be interesting...
;       is it as simple as taking the non-incremental schedule, interpreting it
;       for update with fully precise dirty expressions, and saying that anything
;       untouched is an "input?"

; ------------------
; Schedule execution
; ------------------

; test with
; > (ftl-schedule-interpret ftl-base-runtime (open-input-file "../examples/points.ftl") 'Root example-deriv example-sched)

; interpret : pi_{g:L(G_FTL)} L([[g]]) * L(G_sched) -> L([[g]])
(define (ftl-schedule-interpret runtime ftl-port root tree schedule)
  (current-bitwidth #f)
  (ftl-schedule-evaluate (ftl-ir-generate (parse-ftl ftl-port) runtime)
                         tree
                         schedule))

; evaluate the attributes on the tree of the grammar according to the schedule
(define/match (ftl-schedule-evaluate grammar tree schedule)
  [(_ _ (ftl-sched-par left right))
   ; TODO: parallelism
   (ftl-schedule-evaluate grammar tree (ftl-sched-seq left right))]
  [(_ _ (ftl-sched-seq left right))
   (ftl-schedule-evaluate grammar tree left)
   (ftl-schedule-evaluate grammar tree right)]
  ; TODO: nested schedules
  [(_ _ (ftl-sched-trav order visit-steps))
   (let ([traverse (match order
                     ['pre ftl-tree-preorder]
                     ['post ftl-tree-postorder])]
         [visit (curry ftl-visit! grammar visit-steps)])
     (traverse visit tree))])

; To better understand the desired semantics of incremental/partial tree
; traversal, keep in mind these two theorems, which can be used to reason about
; the affected region of propagation of any single change throughout the tree.
;
; Theorem: For any attribute grammar given in the semantics of FTL, a post-order
; (bottom-up) traversal can propagate information from any particular attribute
; on any particular node at most one level down the tree and any number of levels
; up the tree.
; Theorem: For any attribute grammar given in the semantics of FTL, a pre-order
; (top-down) traversal can propagate information from any particular attribute on
; any particular node at most one level up the tree and any number of levels down
; the tree.
; Corollary: For any attribute grammar given in the semantics of FTL, the effect,
; or collection of all transitive outgoing dependencies, of any particular
; attribute on any particular node is encapsulated by a static region on any
; particular tree, such that every node in the region has a dependent attribute
; or the attribute itself.

; What if we could synthesize specialized incremental traversals according to
; the specification of equivalence between executions with the candidate and
; normal traversal functions on the same initial tree, update, and update
; schedule.

; TODO
; 1. dirtiness predicates in incremental attribute grammar
; 2. schedule fast-forward to first traversal dependent on some attribute
; 3. explore partitioning of input attributes for synthesis of specialized
;    update schedules, perhaps such that their dependency graphs share no
;    vertices (i.e., exhibit orthogonality)

; evaluate the attributes requiring updates on a fully annotated tree of the
; grammar according to the schedule, with implicit but naive incrementality
(define (ftl-schedule-incremental-evaluate grammar tree schedule)
  (match schedule
    [(ftl-sched-par left right)
     (ftl-schedule-evaluate grammar tree (ftl-sched-seq left right))]
    [(ftl-sched-seq left right)
     (ftl-schedule-evaluate grammar tree left)
     (ftl-schedule-evaluate grammar tree right)]
    ; TODO: nested schedules
    [(ftl-sched-trav order visit-steps)
     (let ([traverse (match order
                       ['pre ftl-tree-partial-preorder]
                       ['post ftl-tree-partial-postorder])]
           [visit (curry ftl-visit! grammar visit-steps)])
       ; TODO: recursive traversals
       (traverse visit tree))]))

; ---------------
; Tree Traversals
; ---------------

; pre-order traversal of a tree
(define (ftl-tree-preorder visit tree)
  (visit tree)
  (for ([child (ftl-tree-children tree)])
    (if (list? (cdr child))
        (for-each (curry ftl-tree-preorder visit) (cdr child))
        (ftl-tree-preorder visit (cdr child)))))

; post-order traversal of a tree
(define (ftl-tree-postorder visit tree)
  (for ([child (ftl-tree-children tree)])
    (if (list? (cdr child))
        (for-each (curry ftl-tree-postorder visit) (cdr child))
        (ftl-tree-postorder visit (cdr child))))
  (visit tree))

; partial pre-order traversal of a tree, returns new affected region
(define (ftl-tree-partial-preorder ascend? descend? depth visit tree)
  ; TODO: actually ascend to parent if necessary and return new root+depth
  ; (when (ascend? tree)
  ;     (visit parent))
  ; pseudocode:
  ;   if parent dirty:
  ;      ascend to parent, expanding affected region
  ;   if depth >= 0:
  ;      incrementally visit self
  ;      for child in children:
  ;          if depth > 0 or child dirty:
  ;             recurse down to child
  ;   return new root and relative depth of affected region
  (when (>= depth 0)
    (visit tree)
    (let ([listify (λ (l) (if (list? l) l (list l)))]
          [depth (- depth 1)]
          [ascend? (λ (_) #f)])
      (for ([child (map listify (ftl-tree-children tree))])
        (ftl-tree-partial-preorder ascend? depth visit child)))))

; partial post-order traversal of a tree, returns new affected region
(define (ftl-tree-partial-postorder ascend? descend? depth visit tree)
  ; TODO: actually ascend to parent if necessary and return new root+depth
  ; pseudocode:
  ;   if depth >= 0:
  ;      incrementally visit self
  ;      for child in children:
  ;          if depth > 0 or child dirty:
  ;             descend to child
  ;   if parent dirty:
  ;      ascend to parent, expanding affected region
  ;   return new root and relative depth of affected region
  (for ([child (ftl-tree-children tree)])
    (if (list? (cdr child))
        (for-each (curry ftl-tree-postorder visit) (cdr child))
        (ftl-tree-postorder visit (cdr child)))))

; ---------------------
; Tree Traversal Visits
; ---------------------

; evaluate some attributes at the each node in the subtree rooted at the given
; node
(define (ftl-visit! grammar visit-steps self)
  (match-let* ([(ftl-tree symbol option _ _) self]
               [production (assoc-lookup (assoc-lookup grammar symbol) option)]
               [definitions (ftl-ir-production-definitions production)]
               [steps (assoc-lookup visit-steps (cons symbol option))]
               [get-action (λ (attribute)
                             (cons attribute
                                   (assoc-lookup definitions attribute)))])

    ; perform each step (i.e., sequenced direct evaluation or lockstep
    ; iteration) of the visit in sequence
    (for ([step steps])
      (match step
        [(ftl-visit-eval attributes)
         (ftl-visit-evaluate! grammar
                              (map get-action attributes)
                              self)]
        [(ftl-visit-iter child-name attributes)
         (ftl-visit-iterate! grammar
                             (map get-action attributes)
                             child-name
                             self)]))))

; perform a visit step of sequenced evaluations
(define (ftl-visit-evaluate! grammar actions self)
  (for ([action actions])
    (match-let* ([(cons (cons object label)
                        (ftl-ir-definition iter eval)) action]
                 [(ftl-ir-evaluation fun deps) eval]
                 [load (curry ftl-tree-load self (void) null null)]
                 [value (fun (vector-map load deps))])
      (ftl-tree-bind! self object label value))))

; perform a visit step of sequenced evaluations
; parameterize with: default, bind
(define (ftl-visit-iterate! grammar actions child-name self)
  ; let's just double-check that these actions are actually meant to iterate
  ; over this child sequence
  (for ([action actions])
    (assert (eq? (ftl-ir-definition-iterate (cdr action))
                 child-name)))
  ; for each folded attribute, compute the initial value
  ; collect initial values into an association list
  ; for each child in sequence, evaluate attribute, bind result if
  ; applicable and accumulate result if applicable
  ; for each final accumulated value, bind it if applicable
  (let* ([children (assoc-lookup (ftl-tree-children self) child-name)]
         [name-rule-map (cdrmap ftl-ir-definition-evaluate actions)])

    (define initial
      (for/fold ([current null])
                ([name-rule name-rule-map])
        (match-let ([(cons name rule) name-rule]
                    [load (curry ftl-tree-load self (void) null null)])
          (match rule
            [(ftl-ir-evaluation _ _)
             ; no initial accumulator for straight evaluations
             current]
            [(ftl-ir-reduction (ftl-ir-evaluation fun deps) _)
             ; compute the initial evaluation for each reduction
             (cons (cons name (fun (vector-map load deps)))
                   current)]))))

    (define final
      (for/fold ([previous initial])
                ([child children])
        (for/fold ([current null])
                  ([name-rule name-rule-map])
          (match-let* ([(cons name rule) name-rule]
                       [(cons object label) name]
                       [load (curry ftl-tree-load self child previous current)])
            (match rule
              [(ftl-ir-evaluation fun deps)
               ; straight evaluation, assigning to an attribute on
               ; either some singleton child or the iterated child
               ; sequence
               (let ([value (fun (vector-map load deps))])
                 (if (eq? object child-name)
                     (ftl-tree-bind! child 'self label value)
                     (ftl-tree-bind! self object label value))
                 ; pass through the current accumulator unchanged
                 current)]
              [(ftl-ir-reduction _ (ftl-ir-evaluation fun deps))
               ; reduction, so perform the step evaluation, assign to
               ; iterated child sequence if necessary, and pass
               ; through new accumulator value
               (let ([value (fun (vector-map load deps))])
                 (when (eq? object child-name)
                   (ftl-tree-bind! child 'self label value))
                 ; pass through the current accumulator unchanged
                 (cons (cons name (fun (vector-map load deps)))
                       current))])))))

    ; lastly, bind the final value for each accumulated value to its attribute
    ; destination if on a singleton child
    (for ([result final])
      (match-let ([(cons (cons object label) value) result])
        (unless (eq? object child-name)
          (ftl-tree-bind! self object label value))))))

; -------------------
; Schedule validation
; -------------------

; count the traversals in a schedule in an embarrassingly inefficient manner
(define/match (ftl-schedule-length schedule)
  [((ftl-sched-par left right))
   (+ (ftl-schedule-length left)
      (ftl-schedule-length right))]
  [((ftl-sched-seq left right))
   (+ (ftl-schedule-length left)
      (ftl-schedule-length right))]
  [((ftl-sched-trav order visit-steps))
   1])

; functionally update the grammar to invoke a validation evaluation rule
; returning an int
(define (make-validation-grammar grammar rule)
  (for/list ([symbol-alternatives grammar])
    (match-let ([(cons symbol alternatives) symbol-alternatives])
      (define new-alternatives
        (for/list ([option-production alternatives])
          (match-let* ([(cons option production) option-production]
                       [(ftl-ir-production inputs
                                           labels
                                           definitions
                                           singletons
                                           sequences) production])
            (define new-definitions
              (for/list ([action definitions])
                (match-let ([(cons attribute
                                   (ftl-ir-definition iter fold-or-eval))
                             action])
                  (define new-fold-or-eval
                    (match fold-or-eval
                      [(ftl-ir-reduction (ftl-ir-evaluation _ init-deps)
                                         (ftl-ir-evaluation _ step-deps))
                       (ftl-ir-reduction (ftl-ir-evaluation rule init-deps)
                                         (ftl-ir-evaluation rule step-deps))]
                      [(ftl-ir-evaluation _ deps)
                       (ftl-ir-evaluation rule deps)]))
                  (cons attribute
                        (ftl-ir-definition iter new-fold-or-eval)))))
            (define new-labels
              (cdrmap (const 'int) labels))
            (define new-production
              (ftl-ir-production inputs
                                 new-labels
                                 new-definitions
                                 singletons
                                 sequences))
            (cons option new-production))))
      (cons symbol new-alternatives))))

; construct a new tree with all input attributes set to the same value
(define/match (make-validation-tree value tree)
  [(_ (ftl-tree symbol option attributes children))
   (ftl-tree symbol
             option
             (cdrmap (const value) attributes)
             (cdrmap (λ (child)
                       (if (list? child)
                           (map (curry make-validation-tree value) child)
                           (make-validation-tree value child)))
                       children))])

; check that each attribute's dependencies are satisfied before its evaluation
; TODO: lift over symbolic trees
(define (ftl-schedule-validate grammar schedule tree)
  ; the current index, which is mutated on each successive traversal
  (define index -1)
  (define val-tree (make-validation-tree index tree))
  (set! index 0)

  ; generate the validation grammar
  (define val-grammar
    (make-validation-grammar grammar
                             (λ (dependencies)
                               (for ([dependency dependencies])
                                 (assert (<= dependency index)))
                               index)))

  ; recursively validate the schedule w.r.t. to the validation grammar and tree
  (define (recurse schedule)
    (for/all ([schedule schedule])
      (match schedule
        [(ftl-sched-par left right)
         (let ([left-size (ftl-schedule-length left)]
               [right-size (ftl-schedule-length right)])
           (set! index (+ index left-size))
           (recurse right)
           (set! index (- index left-size))
           (recurse left)
           (set! index (+ index left-size right-size)))]
        [(ftl-sched-seq left right)
         (recurse left)
         (set! index (+ index (ftl-schedule-length left)))
         (recurse right)
         (set! index (+ index (ftl-schedule-length right)))]
        ; TODO: nested schedules
        [(ftl-sched-trav order visit-steps)
         (let ([traverse (for/all ([order order])
                           (match order
                             ['pre ftl-tree-preorder]
                             ['post ftl-tree-postorder]))]
               [visit (curry ftl-visit! val-grammar visit-steps)])
           ; TODO: recursive traversal
           (traverse visit val-tree))])))

  ; evaluate the validation schedule and check the output
  (recurse schedule)

  (ftl-tree-check-output val-grammar val-tree))

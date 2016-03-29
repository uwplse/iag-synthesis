#lang s-exp rosette

; Functional Tree Language (FTL) intepreter
; Schedules

(require "../utility.rkt"
         "translate.rkt"
         "derivation.rkt")

(provide (struct-out ftl-visit-eval)
         (struct-out ftl-visit-iter)
         (struct-out ftl-sched-trav)
         (struct-out ftl-sched-par)
         (struct-out ftl-sched-seq)
         ftl-schedule-interpret
         ftl-schedule-evaluate
         ftl-schedule-validate
         ftl-schedule-verify
         ftl-schedule-synthesize)

; Incremental grammars may simply require the following inference rule:
;     Γ |- τ.α
; -----------------
;    Γ |- ι[τ.α]
; Using this, we may easily and directly construct the regular (but semantically
; significant for incrementality) attributes τ.α_d, τ.α_Δ

; If the list of attributes to compute for a given production in the visit field
; of a traversal contains a subsequences of attributes whose definitions require
; an iteration over the same child sequence, their iterations must be coalesced
; so that any interleaved dependencies may be satisfied.

; Actually, we need to include an explicit iteration grouping, so that schedules
; can express the idea that an iteration over some child sequence that defines,
; say, self.alpha should be fully evaluated before another iteration over that
; same child sequence that defines, say, self.beta, because the iteration step
; of self.beta could full well be dependent on the _final_ value of self.alpha
; and nothing else (besides possibly the dependencies of self.alpha's preceding
; evaluation).

; A schedule can be an ftl-sched-traversal, an ftl-sched-parallel, or an
; ftl-sched-sequence, and a visit step can be either an ftl-visit-evaluate or an
; ftl-visit-iterate.

; a visit step of attribute evaluations to be sequenced
(struct ftl-visit-eval
  (; list of object-label pairs, lacking indexed dependencies and not defining
   ; inherited attributes for sequence children, to compute
   attributes
   ) #:transparent)

; a visit step of attribute evaluations to be iterated in lockstep
(struct ftl-visit-iter
  (; name of child sequence
   sequence
   ; list of object-label pairs to compute in lock-step iteration of child
   ; sequence
   attributes
   ) #:transparent)

; traversal descriptor
(struct ftl-sched-trav
  (; 'pre or 'post (TODO: 'parpre and 'parpost)
   order
   ; list associating (symbol . option) pairs to lists of visit steps (either
   ; ftl-visit-eval or ftl-visit-iter)
   steps
   ; nested schedule or (void)
;   nested ; TODO
   ) #:transparent)

; parallel schedule composition
(struct ftl-sched-par
  (; list of schedules to compose in parallel
   schedules
   ) #:transparent)

; sequential schedule composition
(struct ftl-sched-seq
  (; list of schedules to compose in sequence
   schedules
   ) #:transparent)

(define example-sched
  (ftl-sched-seq
   `(,(ftl-sched-trav 'pre
                      `(((Root . Origin) . (,(ftl-visit-iter 'p '((p . bx)
                                                                  (p . by)))))
                        ((Point . Relative) . (,(ftl-visit-eval '((self . x)
                                                                  (self . y)))
                                               ,(ftl-visit-iter 'p '((p . bx)
                                                                  (p . by)))))
                        ((Point . Fixed) . (,(ftl-visit-eval '((self . x)
                                                               (self . dx)
                                                               (self . y)
                                                               (self . dy)))
                                            ,(ftl-visit-iter 'p '((p . bx)
                                                                  (p . by)))))
                        ((Point . Endpoint). (,(ftl-visit-eval '((self . x)
                                                                 (self . y)))))))
     ,(ftl-sched-trav 'pre
                      `(((Root . Origin) . (,(ftl-visit-iter 'p '((self . w)
                                                                  (self . h)))))
                        ((Point . Relative) . (,(ftl-visit-iter 'p '((self . w)
                                                                     (self . h)))))
                        ((Point . Fixed) . (,(ftl-visit-iter 'p '((self . w)
                                                                  (self . h)))))
                        ((Point . Endpoint). ()))))))

; converts a flat, sequential schedule in the style of ag-scheduler into this
; new, more expressive data structure
(define (ftl-schedule-normalize ag-schedule get-symbol)
  (ftl-sched-seq
   (for/list ([ag-traversal ag-schedule])
     (match-let* ([(cons ag-order ag-steps)
                   ag-traversal]
                  [order
                   (match ag-order
                     ['TD 'pre]
                     ['BU 'post])]
                  [steps
                   (for/list ([ag-step (assoc-group first ag-steps)])
                     (match-let* ([(cons option ag-attributes) ag-step]
                                  [symbol (get-symbol option)])
                       (cons (cons symbol option)
                             (for/list ([ag-attribute ag-attributes])
                               (match-let* ([(list _ ag-object label)
                                             ag-attribute]
                                            [object
                                             (if (null? ag-object)
                                                 'self
                                                 ag-object)])
                                 (cons object
                                       label))))))])
       (ftl-sched-trav order steps)))))

; ------------------
; Schedule execution
; ------------------

; interpret : pi_{g:L(G_FTL)} L([[g]]) * L(G_sched) -> L([[g]])
(define (ftl-schedule-interpret runtime ftl root tree schedule)
  (void))

; evaluate the attributes on the derivation of the grammar according to the
; schedule
(define (ftl-schedule-evaluate grammar derivation schedule)
  (match schedule
    [(ftl-sched-par subschedules)
     ; TODO: parallelism
     (ftl-schedule-evaluate grammar
                            derivation
                            (ftl-sched-seq subschedules))]
    [(ftl-sched-seq subschedules)
     (for ([subschedule subschedules])
       (ftl-schedule-evaluate grammar derivation subschedule))]
    ; TODO: nested schedules
    [(ftl-sched-trav order visit-steps)
     (let ([traverse (match order
                       ['pre ftl-tree-preorder]
                       ['post ftl-tree-postorder])]
           [visit (curry ftl-visit! grammar visit-steps)])
       (traverse visit derivation))]))

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

; ---------------------
; Tree Traversal Visits
; ---------------------

; evaluate some attributes at the each node in the subtree rooted at the given
; node 
(define (ftl-visit! grammar visit-steps self)
  (match-let* ([(ftl-tree symbol option _ _) self]
               [vocab (ftl-ir-grammar-vocabulary grammar)]
               [production (assoc-lookup (assoc-lookup vocab symbol) option)]
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
      ; TODO: remove? schedule _SHOULD_ be okay by here
      (assert (void? iter))
      (ftl-tree-bind! self object label value))))

; perform a visit step of sequenced evaluations
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
         [name-rule-map (cdrmap ftl-ir-definition-evaluate actions)]
         [initial (for/fold ([current null])
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
                               current)])))]
         [final (for/fold ([previous initial])
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
                                 current))]))))])
    ; lastly, bind the final value for each accumulated value to its attribute
    ; destination if on a singleton child
    (for ([result final])
      (match-let ([(cons (cons object label) value) result])
        (unless (eq? object child-name)
          (ftl-tree-bind! self object label value))))))

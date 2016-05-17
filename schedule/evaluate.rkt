#lang rosette

; Functional Tree Language (FTL) intepreter
; Schedules

(require rosette/lib/angelic
         "../utility.rkt"
         "../grammar/tree.rkt"
         "../grammar/traversal.rkt"
         "../grammar/parse.rkt"
         "../grammar/generate.rkt"
         "syntax.rkt"
         "visit.rkt")

(provide ftl-schedule-interpret!
         ftl-schedule-evaluate!)

; Incremental grammars may simply require the following inference rule in a
; distinct context for dirty bit predicates.
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
(define (ftl-schedule-interpret! runtime ftl-port root tree schedule)
  (current-bitwidth #f)
  (ftl-schedule-evaluate! (ftl-ir-generate (parse-ftl ftl-port) runtime)
                          tree
                          schedule))

; evaluate the attributes on the tree of the grammar according to the schedule
(define/match (ftl-schedule-evaluate! grammar tree schedule)
  [(_ _ (ftl-sched-par left right))
   (ftl-schedule-evaluate! grammar tree (ftl-sched-seq left right))]
  [(_ _ (ftl-sched-seq left right))
   (ftl-schedule-evaluate! grammar tree left)
   (ftl-schedule-evaluate! grammar tree right)]
  [(_ _ (ftl-sched-trav order visit-steps))
   (let ([traverse (for/all ([order order])
                     (match order
                       ['pre ftl-tree-preorder]
                       ['post ftl-tree-postorder]
                       ['rec ftl-tree-inorder]))]
         [visit (curry ftl-visit! grammar visit-steps ftl-tree-bind!)])
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

; TODO for incrementality
; 1. dirtiness predicates in incremental attribute grammar
; 2. explore partitioning of input attributes for synthesis of specialized
;    update schedules, perhaps such that their dependency graphs share no
;    vertices (i.e., exhibit orthogonality); more generally, we need to look at
;    what sort of subdomains will present interesting opportunities for
;    specialization, both incrementally and generally

; evaluate the attributes requiring updates on a fully annotated tree of the
; grammar according to the schedule, with implicit but naive incrementality
(define (ftl-schedule-incremental-evaluate grammar tree schedule)
  ; TODO: propagate bounds of affected region, perhaps via a path and depth
  (match schedule
    [(ftl-sched-par left right)
     (ftl-schedule-incremental-evaluate grammar tree (ftl-sched-seq left right))]
    [(ftl-sched-seq left right)
     (ftl-schedule-incremental-evaluate grammar tree left)
     (ftl-schedule-incremental-evaluate grammar tree right)]
    [(ftl-sched-trav order visit-steps)
     (let ([traverse (match order
                       ['pre ftl-tree-partial-preorder]
                       ['post ftl-tree-partial-postorder])]
           [visit (curry ftl-visit! grammar visit-steps ftl-tree-bind!)])
       (traverse visit tree))]))

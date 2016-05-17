#lang rosette

; Functional Tree Language (FTL) intepreter
; Schedule DSL Abstract Syntax Tree

(require rosette/lib/angelic
         "../utility.rkt"
         "../grammar/intermediate.rkt")

(provide (struct-out ftl-step-eval)
         (struct-out ftl-step-iter)
         (struct-out ftl-step-recur)
         (struct-out ftl-sched-trav)
         (struct-out ftl-sched-par)
         (struct-out ftl-sched-seq)
         ftl-step?
         ftl-sched?
         ftl-sched-normalize
         make-step*
         ftl-sched*)

(define (ftl-sched? s)
  (or (ftl-sched-par? s)
      (ftl-sched-seq? s)
      (ftl-sched-trav? s)))

(define (ftl-step? s)
  (or (void? s)
      (ftl-step-eval? s)
      (ftl-step-iter? s)
      (ftl-step-recur? s)))

; a visit step of an attribute evaluation
(struct ftl-step-eval
  (; an attribute, identified as an object-label pair
   attributes
   ) #:mutable
     #:transparent)

; a visit step of attribute evaluations to be iterated in lockstep
(struct ftl-step-iter
  (; a list of attribute evaluation substeps (ftl-step-eval)
   evaluations
   ) #:mutable
     #:transparent)

; a visit step of a recursive visit
(struct ftl-step-recur
  (; an identifier of a child (singleton or sequence) to recursively visit
   child
   ) #:transparent
     #:mutable)

; traversal descriptor
(struct ftl-sched-trav
  (; 'pre, 'post, or 'rec
   order
   ; list associating (symbol . option) pairs to lists of visit steps
   steps
   ) #:transparent
     #:mutable)

; parallel schedule composition
(struct ftl-sched-par
  (; first schedules to compose in parallel
   left
   ; second schedules to compose in parallel
   right
   ) #:transparent
     #:mutable)

; sequential schedule composition
(struct ftl-sched-seq
  (; first schedules to compose in sequence
   left
   ; second schedules to compose in sequence
   right
   ) #:transparent
     #:mutable)

; converts a flat, sequential schedule in the style of ag-scheduler into this
; new, more expressive data structure
(define (ftl-sched-normalize ag-schedule get-symbol)
  (define traversals
    (for/list ([ag-traversal ag-schedule])
      (match-let ([(cons ag-order ag-steps) ag-traversal])
        (define order
          (match ag-order
            ['TD 'pre]
            ['BU 'post]))
        (define steps
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
                              label)))))))
        (ftl-sched-trav order steps))))
  (match traversals
    [(list traversal)
     traversal]
    [else
     (foldl ftl-sched-seq (first traversals) (rest traversals))]))

; ---------------
; Schedule oracle
; ---------------

(define (make-step* production #:recur [recur #f])
  (let*-values ([(definitions) (ftl-ir-production-definitions production)]
                [(iterates) (compose ftl-ir-definition-iterate cdr)]
                [(unlooped looped) (partition (compose void? iterates) definitions)]
                [(wrap) (compose ftl-step-eval car)]
                [(uniterated) (map wrap unlooped)]
                [(iterated) (map (compose (λ (l)
                                            (make-list (- (length l) 1) l))
                                          (curry cons (void))
                                          (curry map wrap))
                                 (group-by iterates looped eq?))]
                [(steps) (cons (void) (append* uniterated iterated))]
                [(children) (append (ftl-ir-production-singletons production)
                                    (ftl-ir-production-sequences production))]
                [(steps) (if recur
                             (append steps
                                     (map ftl-step-recur children))
                             steps)])
    (thunk ; step*
     (apply choose*
            (for/list ([step steps])
              (if (list? step)
                  (ftl-step-iter (map (λ (_)
                                        (apply choose* step))
                                      step))
                  step))))))

; generate a symbolic m-step n-traversal schedule
; parameterize by number of traversals and number of steps per traversal, then
; color each attribute with its traversal and step indices, but asserting (in
; traversal generation) that each evaluation in a step iterates over the same
; child (or void).
(define (ftl-sched* grammar n)
  ; We use the fact that a proper binary tree, which a schedule AST is, must
  ; always have n-1 inner nodes if there are n leaves, which are traversals in a
  ; schedule AST, regardless of shape.
  ; For n-1 inner nodes, there are 2^(n-1) possible [proper] binary trees, so for
  ; now, we'll stick to only exploring the perfect binary tree for n-1 inner
  ; nodes.
  (define step*-list ; list associating each symbol-option pair to a step*
    (append* (for/list ([symbol-alternatives grammar])
               (match-let ([(cons symbol alternatives) symbol-alternatives])
                 (for/list ([alternative alternatives])
                   (match-let ([(cons option production) alternative])
                     (cons (cons symbol option)
                           (make-step* production))))))))
  (define/match (recurse h)
    [(0)
     (ftl-sched-trav 'pre;(choose* 'pre 'post)
                     (cdrmap (λ (step*) (step*)) step*-list))]
    [(h)
     (ftl-sched-seq;(choose* ftl-sched-seq ftl-sched-par)
      (recurse (- h 1))
      (recurse (- h 1)))
     ])
  (recurse n))

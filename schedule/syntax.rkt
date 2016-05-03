#lang rosette

; Functional Tree Language (FTL) intepreter
; Schedule DSL Abstract Syntax Tree

(require "../core/utility.rkt"
         "../core/grammar.rkt")

(provide (struct-out ftl-visit-eval)
         (struct-out ftl-visit-iter)
         (struct-out ftl-sched-trav)
         (struct-out ftl-sched-par)
         (struct-out ftl-sched-seq)
         ftl-sched?
         ftl-sched-normalize
;         ftl-sched*
         )

(define (ftl-sched? v)
  (or (ftl-sched-par? v)
      (ftl-sched-seq? v)
      (ftl-sched-trav? v)))

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
  (; first schedules to compose in parallel
   left
   ; second schedules to compose in parallel
   right
   ) #:transparent)

; sequential schedule composition
(struct ftl-sched-seq
  (; first schedules to compose in sequence
   left
   ; second schedules to compose in sequence
   right
   ) #:transparent)

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

; generate a symbolic m-step n-traversal schedule
; parameterize by number of traversals and number of steps per traversal, then
; color each attribute with its traversal and step indices, but asserting (in
; traversal generation) that each evaluation in a step iterates over the same
; child (or void).
(define (ftl-sched* grammar n m)
  ; We use the fact that a proper binary tree, which a schedule AST is, must
  ; always have n-1 inner nodes if there are n leaves, which are traversals in a
  ; schedule AST, regardless of shape.
  ; For n-1 inner nodes, there are 2^(n-1) possible [proper] binary trees, so for
  ; now, we'll stick to only exploring the perfect binary tree for n-1 inner
  ; nodes.
  (define height (- n 1))
  (define (indices* _)
    (let ([t (integer*)]
          [s (integer*)])
      (cons t s)))
  (define assignments
    (for/list ([symbol-alternatives grammar])
      (match-let ([(cons symbol alternatives) symbol-alternatives])
        (for/list ([alternative alternatives])
          (match-let ([(cons option production) alternative])
            (cons (cons symbol option)
                  (for/list ([defn (ftl-ir-production-definitions production)]
                             [t (integer*)]
                             [s (integer*)])
                    (assert (and (>= t 0) (< t n)))
                    (assert (and (>= s 0) (< s m))))

                  (cdrmap indices*
                          (ftl-ir-production-labels production))))))))
  (define attributes
    (append*
     (for/list ([symbol-alternatives grammar])
       (match-let ([(cons symbol alternatives) symbol-alternatives])
         (for/list ([alternative alternatives])
           (match-let ([(cons option production) alternative])
             (cons (cons symbol option)
                   (cdrmap indices*
                           (ftl-ir-production-labels production)))))))))
  (define i -1)
  (define/match (recurse h)
    [(0)
     (set! i (+ i 1))
     ; 1. throw away attributes if t /= i
     ; 2. group and order attributes by s
     (ftl-sched-trav (choose* 'pre 'post)
                     (map
                     (cdrmap (compose (curry map car)
                                      (curry filter (λ (x) (= (cadr x) i))))
                             attributes)))]
    [(h)
     ((choose* ftl-sched-seq ftl-sched-par)
      (recurse (- h 1)) (recurse (- h 1)))
     ])
  (recurse height))

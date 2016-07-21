#lang rosette

(require rosette/lib/angelic
         "../utility.rkt"
         "../grammar/runtime.rkt"
         "../grammar/tree.rkt"
         "../grammar/traversal.rkt"
         "../grammar/intermediate.rkt"
         "syntax.rkt"
         "visit.rkt")

(provide ftl-schedule-validate)

; -------------------
; Schedule validation
; -------------------

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
(define (make-validation-tree value tree)
  (for/all ([tree tree])
    (match-let ([(ftl-tree symbol option attributes children) tree])
      (ftl-tree symbol
                option
                (cdrmap (const value) attributes)
                (cdrmap (λ (child)
                          (for/all ([child child])
                            (if (list? child)
                                (map (curry make-validation-tree value) child)
                                (make-validation-tree value child))))
                        children)))))

; check that each attribute's dependencies are satisfied before its evaluation
; TODO: lift over symbolic trees
(define (ftl-schedule-validate grammar schedule tree #:runtime [runtime ftl-base-runtime])
  ; the current index, which is mutated on each successive traversal
  (define exclude-min -1) ; exclusive exclusion
  (define exclude-max -1) ; inclusive exclusion
  (define index 0)

  ; generate the validation grammar
  (define val-grammar
    (make-validation-grammar grammar
                             (λ (dependencies)
                               (set! index (+ index 1))
                               (for ([dependency dependencies])
                                 (assert (not (and (> dependency exclude-min)
                                                   (<= dependency exclude-max))))
                                 (assert (< dependency index)))
                               index)))
  (define val-tree
    (make-validation-tree index tree))
  (ftl-tree-symbolize! val-grammar val-tree #:runtime runtime)

  ; recursively validate the schedule w.r.t. to the validation grammar and tree
  (define (recurse schedule)
    (for/all ([schedule schedule])
      (match schedule
        [(ftl-sched-par left right)
         (let ([old-min exclude-min]
               [old-max exclude-max]
               [new-min index])
           (recurse left)
           (set! exclude-min (if (> old-min 0)
                                 (min new-min old-min)
                                 new-min))
           (set! exclude-max index)
           (recurse right)
           (set! exclude-min old-min)
           (set! exclude-max old-max))]
        [(ftl-sched-seq left right)
         (recurse left)
         (recurse right)]
        [(ftl-sched-trav order steps)
         (let ([traverse (for/all ([order order])
                           (match order
                             ['pre ftl-tree-preorder]
                             ['post ftl-tree-postorder]
                             ['rec ftl-tree-inorder]))]
               [visit (curry ftl-visit! val-grammar steps ftl-tree-bind*)])
           (traverse visit val-tree))])))

  ; evaluate the validation schedule and check the output
  (recurse schedule)

  ; TODO: rename to 'ftl-tree-annotated?'
  (ftl-tree-check-output val-grammar val-tree))

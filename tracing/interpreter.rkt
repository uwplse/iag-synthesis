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
    [(sched-sequential left right)
     (interpret grammar left tree)
     (interpret grammar right tree)]
    [(sched-parallel left right)
     (thread (interpret grammar left tree)
             (interpret grammar right tree))]
    [(sched-traversal order visitors)
     (traverse grammar (get-traversal grammar order) visitors tree)]))

(define (assign node label)
  (write (tree-fields node) label)
  (step))

(define (traverse grammar traversal visitors self)
  (let ([recur (curry traverse grammar traversal visitors)]
        [class-ast (get-class grammar (tree-class self))]
        [form (cdr (assoc (tree-class self) (ag-traversal-forms traversal)))]
        [visitor (cdr (assoc (tree-class self) visitors))])
    (for/fold ([blocks visitor]
               [virtual (empty)])
              ([part form])
      (match part
        [(ag-trav-recur child)
         (recur (cdr (assoc child (tree-children self))))
         (values blocks virtual)]
        [(ag-trav-visit)
         (for/multichoice ([slot (first blocks)])

           (match slot
             [(sched-slot-skip) (void)]
             [(sched-slot-eval object label)
              (let ([expr (ag-rule-right (lookup-rule class-ast object label))])
                (eval expr self #:final virtual)
                (assign (tree-object self object) label))]))
         (values (rest blocks) virtual)]
        [(ag-trav-loop child-seq loop-form)
         (let-values ([(loop-blocks rest-blocks)
                       (split-at blocks (count ag-trav-visit? loop-form))])
           (values rest-blocks
                   (loop class-ast child-seq loop-form loop-blocks recur self)))]))))

; Interpret a loop
(define (loop class-ast child-seq form blocks recur self) ; TODO: final virtual node from previous loops?
  (let ([children (tree-object self child-seq)]
        [initial (empty)])

    ; Initialize the virtual node for all visits.
    (for ([block blocks])
      (for/multichoice ([slot block])
        (match slot
          [(sched-slot-skip) (void)]
          [(sched-slot-eval object label)
           (let ([expr (rule-initial (lookup-rule class-ast object label))])
             (when expr
               (eval expr self)
               (write initial (cons object label))
               (step)))])))

    ; Iterate through the child sequence
    (define final
      (for/fold ([previous initial])
                ([current children])
        (let ([virtual (empty)])
          (for/fold ([blocks blocks])
                    ([part form])
            (match part
              [(ag-trav-recur (? void?))
               (recur current)
               blocks]
              [(ag-trav-visit)
               (for/multichoice ([slot (first blocks)])
                 (match slot
                   [(sched-slot-skip) (void)]
                   [(sched-slot-eval object label)
                    (let ([expr (rule-iterate (lookup-rule class-ast object label))])
                      (eval expr self child-seq previous current virtual)
                      (write virtual (cons object label))
                      (step)
                      (when (equal? object child-seq)
                        (assign current label)))]))
               (rest blocks)]))
          virtual)))

    ; Assign final attribute values.
    (for ([block blocks])
      (for/multichoice ([slot block])
        (match slot
          [(sched-slot-skip) (void)]
          [(sched-slot-eval object label)
           (unless (equal? object child-seq)
             (read final (cons object label))
             (assign (tree-object self object) label))])))

    final))

; Abstract interpretation of an expression
(define (eval expression self [child-seq #f] [previous #f] [current #f] [virtual #f] #:final [final #f])
  (define/match (recur expression)
    [((ag-expr-unary _ operand))
     (recur operand)]
    [((ag-expr-binary left _ right))
     (recur left)
     (recur right)]
    [((ag-expr-condition condition consequent alternate))
     (recur condition)
     (recur consequent)
     (recur alternate)]
    [((ag-expr-call _ arguments))
     (for-each recur arguments)]
    [((? ag-expr-reference?))
     (let ([lookup (λ (name store) (read store name))]) ; Well this feels a bit silly.
       (tree-load self expression lookup child-seq previous current virtual final))]
    [((or (? number?) (? boolean?) (? string?)))
     (void)])
  (recur expression))

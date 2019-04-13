#lang rosette

; Build a Rust syntax tree from a completed schedule.

(require "../../schedule/syntax.rkt")

(provide build-program)

; Generic program header

(define header
  (list `(use crate context LayoutContext)
        `(use crate flow (Flow GetBaseFlow))
        ;`(use style servo restyle_damage ServoStyleDamage)
        `(use crate incremental RelayoutMode)
        `(use crate traversal (preorder postorder))))

(define/match (build-statement statement)
  [((list statement-list ...))
   `(do . ,(map build-statement statement-list))]
;  [((sched-iterate ,children ,body))
;   `(for child (call (select (select self ,children) iter_mut) ())
;      ,(build-statement body))]
  [((sched-recur `(self . ,child)))
   `(skip)]
  [((sched-slot-skip))
   `(skip)]
  [((sched-slot-call `(self ,method) params))
   `(call (select node ,method) ,params)])

(define (build-visitors visitor-list)
  (match visitor-list
    [(list (cons 'BlockFlow statement-list))
     (build-statement statement-list)]))

; Generation of whole program/module

(define/match (build-evaluator schedule)
;  [((sched-parallel left right))
;   #f]
  [((sched-sequential left right))
   (append (build-evaluator left)
           (build-evaluator right))]
  [((sched-traversal order visitor-list))
   (list `(call ,order (tree (ref (lambda (node) ,(build-visitors visitor-list))))))])

(define (build-program schedule)
  (append header
          (list
           `(blank)
           `(fn reflow ((: tree (ref (mut (dyn Flow))))
                        (: layout_context (ref LayoutContext))
                        (: relayout_mode RelayoutMode))
                (unit)
                (do . ,(build-evaluator schedule ))))))

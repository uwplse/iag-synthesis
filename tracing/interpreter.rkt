#lang rosette

; Tracing interpreter for language of tree traversal schedules

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../tree.rkt"
         "../trace.rkt")

(provide interpret
         traverse)

(define (interpret G schedule tree)
  (match schedule
    [(sched-sequential left right)
     (interpret G left tree)
     (interpret G right tree)]
    [(sched-parallel left right)
     (join (interpret G left tree)
           (interpret G right tree))]
    [(sched-traversal order visitors)
     (traverse G visitors tree)]))

(define (traverse G visitors self)
  (for ([stmt (lookup visitors (tree-class self))])
    (match stmt
      [(sched-recur child)
       (let ([subtree (lookup (tree-children self) child)])
         (if (list? subtree)
             (for ([node subtree])
               (traverse G visitors node))
             (traverse G visitors subtree)))]
      [(sched-iterate child stmt-list) ; TODO: Do we still need explicit iteration in the schedule?
       (for ([subtree (lookup (tree-children self) child)])
         (raise-user-error 'traverse "scheduled iteration is not implemented"))]
      [_
       (for/permuted ([stmt stmt])
         (match stmt
           [(sched-slot-skip)
            (void)]
           [(sched-slot-call method _)
            (match-let ([(ag-method _ inputs outputs)
                         (get-method G (tree-class self) method)])
              (for ([attr inputs])
                (tree-bind self table-ref! attr))
              (for ([attr outputs])
                (tree-bind self table-def! attr)))]))])))

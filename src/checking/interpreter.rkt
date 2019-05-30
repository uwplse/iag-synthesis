#lang rosette

; Checking interpreter for language of tree traversal schedules

(require rosette/lib/angelic
         "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../utility.rkt"
         "../tree.rkt")

(provide emp new upd lkup interpret)

(define (emp)
  null)

(define (new store name)
  (cons (cons name (box #f)) store))

(define (upd store name)
  (let ([ref (lookup store name)])
    (assert (not (unbox ref)))
    (set-box! ref #t)))

(define (lkup store name)
  (assert (unbox (lookup store name))))

(define (interpret G schedule tree)
  (match schedule
    [(sched-sequential left right)
     (interpret G left tree)
     (interpret G right tree)]
    [(sched-parallel left right)
     (let ([tree (tree-copy tree)])
       (interpret G right tree)
       (interpret G left tree))
     (interpret G left tree)
     (interpret G right tree)]
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
       (for/all ([stmt stmt])
         (match stmt
           [(sched-slot-skip)
            (void)]
           [(sched-slot-call method _)
            (match-let ([(ag-method _ inputs outputs)
                         (get-method G (tree-class self) method)])
              (for ([attr inputs])
                (tree-bind self lkup attr))
              (for ([attr outputs])
                (tree-bind self upd attr)))]))])))

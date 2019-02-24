#lang rosette

; Checking interpreter for language of tree traversal schedules

(require rosette/lib/angelic
         "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../utility.rkt"
         "../tree.rkt")

(provide (all-defined-out))

(define (interpret grammar schedule tree)
  (match schedule
    [(sched-comp 'seq left-schedule right-schedule)
     (interpret grammar left-schedule tree)
     (interpret grammar right-schedule tree)]
    [(sched-comp 'par left-schedule right-schedule)
     (let ([tree (tree-copy tree)])
       (interpret grammar right-schedule tree)
       (interpret grammar left-schedule tree))
     (interpret grammar left-schedule tree)
     (interpret grammar right-schedule tree)]
    [(sched-trav 'pre visits)
     (preorder (make-visitor grammar visits) tree)]
    [(sched-trav 'post visits)
     (postorder (make-visitor grammar visits) tree)]))

(define (preorder visitor tree)
  (visitor tree)
  (for* ([child (tree-children tree)]
         [node (listify (cdr child))])
    (preorder visitor node)))

(define (postorder visitor tree)
  (for* ([child (tree-children tree)]
         [node (listify (cdr child))])
    (postorder visitor node))
  (visitor tree))

(define ((make-visitor grammar visits) self)
  (let* ([classname (tree-class self)]
         [class-ast (get-class grammar classname)]
         [slots (cdr (assq classname visits))])

    (for ([slot slots])
      (interpret-slot class-ast self slot))))

; Assign the value to the local attribute name label (e.g., 'y) on the node named
; by object (e.g., 'left) relative to a particular node self, where the currently
; indexed node is provided (if this assignment happens within a loop).
(define (assign self object label value indexed)
  (let* ([target (ag-expr-reference object #f label)]
         [child (and indexed object)]
         [location (tree-load self child #f indexed target)])
    (assert (void? (unbox location))) ; no overwriting already computed values
    (set-box! location value)))

; Interpret a schedule slot, which is either 'nop or (object . label) for some
; node name object (e.g., 'self) and local attribute name label (e.g., 'x).
(define (interpret-slot class-ast self slot*)
  (unless (equal? slot* 'nop)
    (let ([object* (car slot*)]
          [target* (cdr slot*)])
      (for*/all ([object object*]
                 [target target*])
        (if (list? target)
            (interpret-loop class-ast self object target)
            (interpret-stmt class-ast self object target))))))

; Interpret a statement loop, which is a list of attributes corresponding to
; iterated (fold) evaluation rules.
(define (interpret-loop class-ast self loop-object subslots)
  (let ([children (tree-object self loop-object)]
        [virtual (make-virtual-node self loop-object)])

    ; Initialize the virtual node of initial accumulators
    (for ([subslot* subslots])
      (unless (equal? subslot* 'nop)
        (let ([object* (car subslot*)]
              [label* (cdr subslot*)])
          (for*/all ([object object*]
                     [label label*])
            (interpret-stmt class-ast self object label #f virtual)))))

    ; Perform the actual iterated evaluation.
    (for/fold ([previous virtual])
              ([indexed children])
      (for ([subslot* subslots])
        (unless (equal? subslot* 'nop)
          (let ([object* (car subslot*)]
                [label* (cdr subslot*)])
            (for*/all ([object object*]
                       [label label*])
              (interpret-stmt class-ast self object label previous indexed)))))
      indexed)))

; Interpret a statement, which is an attribute corresponding to an evaluation
; rule. If the evaluation rule is iterated, then the previous and indexed
; child nodes are supplied as optional arguments.
(define (interpret-stmt class-ast self object label [previous #f] [indexed #f])
  (let ([dereference (curry tree-load self (and indexed object) previous indexed)])
    (match (ag-rule-right (lookup-rule (ag-class-rules class-ast) object label))
      [(ag-loop _ (ag-fold init-expr iter-expr))
       (let ([value (interpret-expr dereference (if previous iter-expr init-expr))])
         (assign self object label value indexed))]
      [(or (ag-loop _ expr) expr)
       (unless (and previous (not indexed))
         (let ([value (interpret-expr dereference expr)])
           (assign self object label value indexed)))])))

; Is the attribute stored at the given location (box) initialized?
(define (ready? location)
  (not (void? (unbox location))))

; Concrete interpretation of an expression using the supplied function to
; resolve attributes to locations.
(define (interpret-expr dereference expression)
  (let ([recur (curry interpret-expr dereference)])
    (cond
      [(ag-expr-unary? expression)
       (let ([value (recur expression)]
             [operator (ag-expr-unary-operator expression)])
         (cond
           [(eq? operator '-) (- expression)]
           [(eq? operator '+) (+ expression)]))]
      [(ag-expr-binary? expression)
       (let ([left-value (recur (ag-expr-binary-left expression))]
             [right-value (recur (ag-expr-binary-right expression))]
             [operator (ag-expr-binary-operator expression)])
         (cond
           [(eq? operator '+) (+ left-value right-value)]
           [(eq? operator '-) (- left-value right-value)]
           [(eq? operator '*) (* left-value right-value)]
           [(eq? operator '/) (/ left-value right-value)]
           [(eq? operator '<) (< left-value right-value)]
           [(eq? operator '<=) (<= left-value right-value)]
           [(eq? operator '==) (= left-value right-value)]
           [(eq? operator '>=) (>= left-value right-value)]
           [(eq? operator '>) (> left-value right-value)]
           [(eq? operator '&&) (&& left-value right-value)]
           [(eq? operator '||) (|| left-value right-value)]))]
      [(ag-expr-condition? expression)
       (recur (if (recur (ag-expr-condition-if expression))
                    (ag-expr-condition-then expression)
                    (ag-expr-condition-else expression)))]
      [(ag-expr-call? expression)
       (raise-user-error 'interpret
                         "undefined function '~a'"
                         (ag-expr-call-function expression))]
      [(ag-expr-reference? expression)
       (let ([value (unbox (dereference expression))])
         (assert (not (void? value))) ; no reading undefined values!
         value)]
      [(or (number? expression)
           (boolean? expression)
           (string? expression))
       expression])))

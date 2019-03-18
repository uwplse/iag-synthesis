#lang rosette

; Checking interpreter for language of tree traversal schedules

(require rosette/lib/angelic
         "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../utility.rkt"
         "../tree.rkt")

(provide lookup
         interpret)

(define (lookup record attr)
  (cdr (assoc attr record)))

(define (interpret grammar schedule tree)
  (match schedule
    [(sched-sequential left right)
     (interpret grammar left tree)
     (interpret grammar right tree)]
    [(sched-parallel left right)
     (let ([tree (tree-copy tree)])
       (interpret grammar right tree)
       (interpret grammar left tree))
     (interpret grammar left tree)
     (interpret grammar right tree)]
    [(sched-traversal order visitors)
     (traverse grammar (get-traversal grammar order) visitors tree)]))

;; (define (assign node label value)
;;   (let ([target (cdr (assq label (tree-fields node)))])
;;     (assert (not (unbox target))) ; no overwriting already computed values
;;     (set-box! target value)))

(define (assign node label value)
  (assert (not (assq label (tree-fields node))))
  (set-tree-fields! node (cons (cons label value) (tree-fields node))))

(define (traverse grammar traversal visitors self)
  (let ([recur (curry traverse grammar traversal visitors)]
        [class-ast (get-class grammar (tree-class self))]
        [form (cdr (assq (tree-class self) (ag-traversal-forms traversal)))]
        [visitor (cdr (assq (tree-class self) visitors))])
    (for/fold ([blocks visitor])
              ([part form])
      (match part
        [(ag-trav-recur child)
         (recur (cdr (assq child (tree-children self))))
         blocks]
        [(ag-trav-visit)
         (for ([slot (first blocks)])
           (for/all ([slot slot])
             (begin
               (match slot
                 [(sched-slot-skip) (void)]
                 [(sched-slot-eval object label)
                  (for*/all ([object object]
                             [label label])
                    (let ([expr (ag-rule-right (lookup-rule class-ast object label))])
                      (assign (tree-object self object) label (eval expr self))))]))))
         (rest blocks)]
        [(ag-trav-loop child-seq loop-form)
         (let-values ([(loop-blocks rest-blocks)
                       (split-at blocks (count ag-trav-visit? loop-form))])
           (loop class-ast child-seq loop-form loop-blocks recur self)
           rest-blocks)]))))

; Interpret a loop
(define (loop class-ast child-seq form blocks recur self)
  (let ([children (tree-object self child-seq)]
        [slot-list (append* blocks)])

    ; Initialize the virtual node for all visits.
    (define initial
      (for/fold ([initial null])
                ([slot slot-list])
        (for/all ([slot slot])
          (match slot
            [(sched-slot-skip) initial]
            [(sched-slot-eval object label)
             (for*/all ([object object]
                        [label label])
               (let ([expr (rule-initial (lookup-rule class-ast object label))])
                 (if expr
                     (cons (cons (cons object label) (eval expr self)) initial)
                     initial)))]))))

    ; Iterate through the child sequence
    (define final
      (for/fold ([previous initial])
                ([current children])
        (for/fold ([virtual null]
                   [blocks blocks]
                   #:result virtual)
                  ([part form])
          (match part
            [(ag-trav-recur (? void?))
             (recur current)
             (values virtual blocks)]
            [(ag-trav-visit)
             (values
              (for/fold ([virtual virtual])
                        ([slot (first blocks)])
                (for/all ([slot slot])
                  (begin
                    (match slot
                      [(sched-slot-skip) virtual]
                      [(sched-slot-eval object label)
                       (for*/all ([object object]
                                  [label label])
                         (let* ([expr (rule-iterate (lookup-rule class-ast object label))]
                                [value (eval expr self child-seq previous current virtual)])
                           (when (equal? object child-seq)
                             (assign current label value))
                           (cons (cons (cons object label) value) virtual)))]))))
              (rest blocks))]))))

    ; Assign final attribute values.
    (for* ([block blocks]
           [slot block])
      (for/all ([slot slot])
        (match slot
          [(sched-slot-skip) (void)]
          [(sched-slot-eval object label)
           (for*/all ([object object]
                      [label label])
             (unless (equal? object child-seq)
               (let ([value (lookup final (cons object label))])
                 (assign (tree-object self object) label value))))])))))


; Abstract interpretation of an expression
(define (eval expression self [child-seq #f] [previous #f] [current #f] [virtual #f])
  (define (recur expression)
    (cond
      [(ag-expr-unary? expression)
       (recur (ag-expr-unary-operand expression))]
      [(ag-expr-binary? expression)
       (and (recur (ag-expr-binary-left expression))
            (recur (ag-expr-binary-right expression)))]
      [(ag-expr-condition? expression)
       (and (recur (ag-expr-condition-if expression))
            (recur (ag-expr-condition-then expression))
            (recur (ag-expr-condition-else expression)))]
      [(ag-expr-call? expression)
       (andmap recur (ag-expr-call-arguments expression))]
      [(ag-expr-reference? expression)
       (tree-load self expression lookup child-seq previous current virtual)]
      [(or (number? expression) (boolean? expression) (string? expression))
       #t]))
  (assert (recur expression))
  #t)

;; ; Concrete interpretation of an expression.
;; (define (eval expression self [child-seq #f] [previous #f] [current #f] [virtual #f])
;;   (define (recur expression)
;;     (cond
;;       [(ag-expr-unary? expression)
;;        (let ([operator (ag-expr-unary-operator expression)]
;;              [value (recur (ag-expr-unary-operand expression))])
;;          (cond
;;            [(equal? operator '-) (- value)]
;;            [(equal? operator '+) (+ value)]))]
;;       [(ag-expr-binary? expression)
;;        (let ([operator (ag-expr-binary-operator expression)]
;;              [left-value (recur (ag-expr-binary-left expression))]
;;              [right-value (recur (ag-expr-binary-right expression))])
;;          (cond
;;            [(equal? operator '+) (+ left-value right-value)]
;;            [(equal? operator '-) (- left-value right-value)]
;;            [(equal? operator '*) (* left-value right-value)]
;;            [(equal? operator '/) (/ left-value right-value)]
;;            [(equal? operator '<) (< left-value right-value)]
;;            [(equal? operator '<=) (<= left-value right-value)]
;;            [(equal? operator '==) (equal? left-value right-value)]
;;            [(equal? operator '>=) (>= left-value right-value)]
;;            [(equal? operator '>) (> left-value right-value)]
;;            [(equal? operator '&&) (&& left-value right-value)]
;;            [(equal? operator '||) (|| left-value right-value)]))]
;;       [(ag-expr-condition? expression)
;;        (let ([condition (ag-expr-condition-if expression)]
;;              [consequent (ag-expr-condition-then expression)]
;;              [alternate (ag-expr-condition-else expression)])
;;          (recur (if (recur condition) consequent alternate)))]
;;       [(ag-expr-call? expression)
;;        (let ([function (ag-expr-call-function expression)]
;;              [arguments (ag-expr-call-arguments expression)])
;;          (raise-user-error 'interpret "undefined function '~a'" function))]
;;       [(ag-expr-reference? expression)
;;        (let* ([lookup (compose unbox cdr assq)]
;;               [value (tree-load self expression lookup child-seq previous current virtual)])
;;          (assert (not (void? value)))
;;          value)]
;;       [(or (number? expression) (boolean? expression) (string? expression))
;;        expression]))
;;   (recur expression))

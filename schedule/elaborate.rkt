#lang rosette

; Benchmark driver for schedule synthesis and verification.

(require racket/dict
         "../utility.rkt"
         "../grammar/syntax.rkt"
         "../schedule/syntax.rkt")

(provide elaborate-schedule)

; Global variable to avoid threading these through basically everything :-(
(define grammar (void))
(define class-ast (void))

(define (public? label)
  (not (lookup-label (ag-class-labels class-ast) label)))

(define (associate-visitors visitors)
  (associate-by (compose ag-class-interface (curry get-class grammar) car)
                visitors))

(define (make-interface-relation grammar)
  (map (λ (class-ast)
         (cons (ag-class-name class-ast)
               (ag-class-interface class-ast)))
       (ag-grammar-classes grammar)))

(define (make-attribute-relation grammar)
  (map (λ (iface-ast)
         (cons (ag-interface-name iface-ast)
               (map ag-label-name (ag-interface-labels iface-ast))))
       (ag-grammar-interfaces grammar)))

(define (elaborate-schedule grammar0 schedule)
  (set! grammar grammar0)
  (define/match (recur sched)
    [((sched-sequential left-sched right-sched))
     `(seq ,(recur left-sched) ,(recur right-sched))]
    [((sched-parallel left-sched right-sched))
     `(par ,(recur left-sched) ,(recur right-sched))]
    [((sched-traversal order visitors))
     (let ([template (ag-traversal-forms (get-traversal grammar order))])
       `(trav ,order . ,(elaborate-traversal template visitors)))])
  (recur schedule))

(define (elaborate-traversal template visitors)
  (for/list ([(iface-name visitors) (in-dict (associate-visitors visitors))])
    `(: ,iface-name
        (case .
          ,(for/list ([(class-name blocks) (in-dict visitors)])
             (set! class-ast (get-class grammar class-name))
             (let ([schema (dict-ref template class-name)])
               `(=> ,class-name (do . ,(elaborate-body-schema schema blocks)))))))))

(define (bundle-blocks schema blocks)
  (define (aux schema blocks bundles)
    (match schema
      [(list (ag-trav-visit) schema ...)
       (let ([bundle (list (first blocks))])
         (aux schema (rest blocks) (cons bundle bundles)))]
      [(list (ag-trav-recur _) schema ...)
       (let ([bundle null])
         (aux schema blocks (cons bundle bundles)))]
      [(list (ag-trav-loop _ loop-schema) schema ...)
       (let-values ([(bundle blocks) (aux loop-schema blocks null)])
         (aux schema blocks (cons bundle bundles)))]
      [null
       (values (reverse bundles) blocks)]))
  (match/values (aux schema blocks null)
     [(bundles null) bundles]))

(define (elaborate-body-schema schema blocks)
  (append-map elaborate-body-form schema (bundle-blocks schema blocks)))

(define/match (elaborate-body-form form bundle)
  [((ag-trav-visit) (list block))
   (map elab-slot block)]
  [((ag-trav-recur child) null)
   (list `(recur (child ,child)))]
  [((ag-trav-loop child schema) (list bundles ...))
   (elaborate-loop-schema schema bundles child)])
  
(define (elaborate-loop-schema schema bundles child)
  (let ([body (append-map (curry elaborate-loop-form child) schema bundles)]
        [distr (λ (elab) (map (curry elab child) (flatten bundles)))])
    (append (distr elab-slot-init)
            (list `(iter ,child (do . ,(append body (distr elab-slot-incr)))))
            (distr elab-slot-fin))))
  
(define/match (elaborate-loop-form child form bundle)
  [(_ (ag-trav-visit) (list block))
   (map (curry elab-slot-iter child) block)]
  [(_ (ag-trav-recur _) null)
   (list `(recur (cursor ,child)))])
  
(define/match (elab-slot slot)
  [((sched-slot-skip))
   `(skip)]
  [((sched-slot-eval object label))
   (let ([attr (elab-attr object #f label)]
         [expr (ag-rule-right (lookup-rule class-ast object label))])
     `(:= ,attr ,(elab-expr expr)))])

(define/match (elab-slot-init child slot)
  [(_ (sched-slot-skip))
   `(skip)]
  [(_ (sched-slot-eval object label))
   (let ([prev (elab-attr object 'previous label #:loop child)]
         [expr (rule-initial (lookup-rule class-ast object label))])
     (if expr
         `(let ,prev ,(elab-expr expr))
         `(skip)))])
  
(define/match (elab-slot-iter child slot)
  [(_ (sched-slot-skip))
   `(skip)]
  [(_ (sched-slot-eval object label))
   (let ([curr (elab-attr object 'current label #:loop child)]
         [expr (rule-iterate (lookup-rule class-ast object label))])
     `(:= ,curr ,(elab-expr expr #:loop child)))])
  
(define/match (elab-slot-incr child slot)
  [(_ (sched-slot-skip))
   `(skip)]
  [(_ (sched-slot-eval object label))
   (let ([curr (elab-attr object 'current label #:loop child)]
         [prev (elab-attr object 'previous label #:loop child)])
     `(:= ,prev ,curr))])
  
(define/match (elab-slot-fin child slot)
  [(_ (sched-slot-skip))
   `(skip)]
  [(_ (sched-slot-eval (== child) label))
   `(skip)]
  [(_ (sched-slot-eval object label))
   (let ([attr (elab-attr object #f label)]
         [prev (elab-attr object 'previous label #:loop child)])
     `(:= ,attr ,prev))])

(define (elab-expr expression #:loop [child #f])
  (define/match (recur expr)
    [((ag-expr-call function arguments))
     `(call ,function ,(map recur arguments))]
    [((ag-expr-unary operator operand))
     `(,operator ,(recur operand))]
    [((ag-expr-binary left operator right))
     `(,operator ,(recur left) ,(recur right))]
    [((ag-expr-condition condition consequent alternate))
     `(if ,(recur condition) ,(recur consequent) ,(recur alternate))]
    [((ag-expr-reference object index label))
     (elab-attr object index label #:loop child)]
    [((? number?)) expr]
    [((? boolean?)) expr]
    [((? string?)) expr])
  (recur expression))

(define (elab-attr object index label #:loop [child #f])
  (cond
    [(equal? index 'first)
     `(attr (first ,object) ,label)]
    [(equal? index 'last)
     `(attr (last ,object) ,label)]
    [(equal? index 'previous)
     `(attr (virtual ,object) ,label)]
    [(equal? object 'self)
     (if (public? label)
         `(attr (public self) ,label)
         `(attr (private self) ,label))]
    [(equal? object child)
     `(attr (cursor ,object) ,label)]
    [(not index)
     `(attr (child ,object) ,label)]))
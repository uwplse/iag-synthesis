#lang rosette

; Build a Rust syntax tree from a completed schedule.

(require racket/dict
         "../../grammar/syntax.rkt"
         "../../schedule/syntax.rkt"
         "../../utility.rkt")

(provide (all-defined-out))

; Generic program header

(define header
  (list `(extern crossbeam)
        `(blank)
        `(use std cell UnsafeCell)
        `(use std time Instant)
        `(blank)
        `(use crossbeam thread *)
        `(blank)
        `(type int i32)
        `(type float f32)
        `(blank)))

; Generation of data structures

(define (build-label-field label-ast)
  (let ([name (ag-label-name label-ast)]
        [type (ag-label-type label-ast)])
    `(: ,name ,type)))

(define (build-child-field child-ast)
  (let ([name (ag-child-name child-ast)]
        [seq? (ag-child-sequence child-ast)]
        [type (ag-child-interface child-ast)])
    `(: ,name (gen ,(if seq? 'Vec 'Box) (,type)))))

(define (build-virtual-field iface-name)
  `(: class ,(symbol-append iface-name 'Kind)))

(define (build-class-fields class-ast)
  (append (map build-label-field (ag-class-labels class-ast))
          (map build-child-field (ag-class-children class-ast))))

(define (build-interface-fields iface-ast)
  (append (map build-label-field (ag-interface-labels iface-ast))
          (list (build-virtual-field (ag-interface-name iface-ast)))))

(define (build-interface-structure iface-ast)
  `(struct
     (constructor ,(ag-interface-name iface-ast)
                  (record . ,(build-interface-fields iface-ast)))))

(define (build-class-variant class-name)
  `(constructor ,(symbol-append 'Is class-name)
                (tuple ,class-name)))

(define (build-class-enumeration iface-name class-names)
  `(enum ,(symbol-append iface-name 'Kind) .
         ,(map build-class-variant class-names)))

(define (build-class-structure class-ast)
  `(struct
     (constructor ,(ag-class-name class-ast)
                  (record . ,(build-class-fields class-ast)))))

(define (build-types grammar)
  (append*
   (for/list ([(iface-name class-ast-list)
               (in-dict (associate-classes grammar))])
     (let ([iface-ast (get-interface grammar iface-name)]
           [class-names (map ag-class-name class-ast-list)])
       (list*
        `(blank)
        (build-interface-structure iface-ast)
        (build-class-enumeration iface-name class-names)
        (map build-class-structure class-ast-list))))))

; Generation of traversal functions

(define (build-variable node label)
  (match node
    [`(public self)
     `(select self ,label)]
    [`(private self)
     `(select class ,label)]
    [`(child ,object)
     `(select (select class ,object) ,label)]
    [`(first ,children)
     `(select (index (select class ,children) 0) ,label)]
    [`(last ,children)
     (let ([length `(call (select (select class ,children) len) ())])
       `(select (index (select class ,children) (- ,length 1)) ,label))]
    [`(virtual ,object)
     (symbol-append label '_acc)]
    [`(cursor ,children)
     `(select ,(symbol-append children '_cur) ,label)]))

(define (build-expression expression loop)
  (define/match (recur expr)
    [(`(call ,function ,arguments))
     `(call ,function ,(map recur arguments))]
    [(`(if ,condition ,consequent ,alternate))
     `(if ,(recur condition) ,(recur consequent) ,(recur alternate))]
    [(`(attr ,node ,label))
     (build-variable node label)]
    [(`(,operator ,operand))
     `(,operator ,(recur operand))]
    [(`(,operator ,left ,right))
     `(,operator ,(recur left) ,(recur right))]
    [((? number?)) expr]
    [((? boolean?)) expr]
    [((? string?)) expr])
  (recur expression))

(define (build-statement trav-name statement)
  (define loop #f)
  (define/match (recur stmt)
    [(`(do ,statement-list ...))
     `(do . ,(map recur (remove* (list `(skip)) statement-list)))]
    [(`(let (attr ,node ,label) ,expr))
     `(let-mut ,(build-variable node label) ,(build-expression expr loop))]
    [(`(:= (attr ,node ,label) ,expr))
     `(:= ,(build-variable node label) ,(build-expression expr loop))]
    [(`(iter ,children (do ,stmt-list ...)))
     (set! loop children)
     (begin0
       `(for ,(symbol-append children '_cur) (call (select (select class ,children) iter_mut) ())
          (do . ,(map recur stmt-list)))
       (set! loop #f))]
    [(`(recur (cursor ,children)))
     `(call (select ,(symbol-append children '_cur) ,trav-name) ())]
    [(`(recur (child ,child)))
     `(call (select (select class ,child) ,trav-name) ())])
  (recur statement))

(define (build-class-visitor trav-name visitor-case)
  (match visitor-case
    [`(=> ,class-name ,visitor-stmt)
     `(=> (constructor ,(symbol-append 'Is class-name) (tuple class))
          ,(build-statement trav-name visitor-stmt))]))

(define (build-interface-visitor trav-name visitor)
  (match visitor
    [`(: ,iface-name (case ,visitor-cases ...))
     `(impl
       ,iface-name
       (fn ,trav-name ((: self (ref-mut Self))) (unit)
           (do (match (select self class) .
                 ,(map (curry build-class-visitor trav-name)
                       visitor-cases)))))]))

(define (build-visitors schedule)
  (define/match (recur sched)
    [(`(seq ,left-sched ,right-sched))
     (append (recur left-sched) (recur right-sched))]
    [(`(par ,left-sched ,right-sched))
     (append (recur left-sched) (recur right-sched))]
    [(`(trav ,order ,visitor-list ...))
     (map (curry build-interface-visitor order) visitor-list)])
  (recur schedule))

; Generation of whole program/module

(define (build-evaluator root-name schedule func-name)
  (define alias-tree
    '(unsafe (call (select (call (select (call (select cell get) ()) as_mut) ()) unwrap) ())))
  (define/match (recur sched tree)
    [(`(seq ,left-sched ,right-sched) _)
     (append (recur left-sched tree) (recur right-sched tree))]
    [(`(par ,left-sched ,right-sched) _)
     (let ([left-tree (symbol-append tree '_l)]
           [right-tree (symbol-append tree '_r)])
       (list
        `(let ,left-tree ,alias-tree)
        `(let ,right-tree ,alias-tree)
        `(call (:: thread scope)
               ((lambda (s)
                  (do (call (select s spawn)
                            ((lambda (_) (do . ,(recur left-sched left-tree)))))
                      (call (select s spawn)
                            ((lambda (_) (do . ,(recur right-sched right-tree)))))))))))]
    [(`(trav ,order ,visitor-list ...) _)
     (list `(call (select ,tree ,order) ()))])
  `(fn ,func-name ((: tree ,root-name)) (unit)
       (do (let cell (call (:: UnsafeCell new) (tree)))
           (let tree ,alias-tree) .
           ,(recur schedule 'tree))))

(define (build-program grammar schedule)
  (append header
          (build-types grammar)
          (build-visitors schedule)
          (list `(blank) (build-evaluator (ag-grammar-root grammar) schedule 'evaluate))))

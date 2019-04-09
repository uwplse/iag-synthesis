#lang rosette

; Build a Rust syntax tree from a completed schedule.

(require racket/dict
         "../../grammar/syntax.rkt"
         "../../schedule/syntax.rkt"
         "../../utility.rkt")

(provide build-program)

; Generic program header

;(define header
;  (list `(extern crossbeam)
;        `(blank)
;        `(use std cell UnsafeCell)
;        `(use std time Instant)
;        `(blank)
;        `(use crossbeam thread *)
;        `(blank)
;        `(type int i32)
;        `(type float f32)))

; Generation of traversal methods

(define (build-variable node label)
  (match node
    [`(public self)
     `(select (call (select self mut_base) ()) label)]
    [`(private self)
     `(select self label)]
    [`(child ,object)
     `(select (call (select (select self ,object) mut_base) ()) ,label)]
    [`(first ,children)
     `(select (call (select (index (select self ,children) 0) mut_base) ()) ,label)]
    [`(last ,children)
     (let ([length `(call (select (select self ,children) len) ())])
       `(select (call (select (index (select self ,children) (- ,length 1)) mut_base) ()) ,label))]
    [`(virtual ,object)
     (symbol-append label '_acc)]
    [`(cursor ,children)
     `(select (call (select ,(symbol-append children '_cur) mut_base) ()) ,label)]))

(define (build-expression expression)
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
  (define/match (recur stmt)
    [(`(do ,statement-list ...))
     `(do . ,(map recur (remove* (list `(skip)) statement-list)))]
    [(`(let (attr ,node ,label) ,expr))
     `(let-mut ,(build-variable node label) ,(build-expression expr))]
    [(`(:= (attr ,node ,label) ,expr))
     `(:= ,(build-variable node label) ,(build-expression expr))]
    [(`(iter ,children ,body))
     `(for ,(symbol-append children '_cur) (call (select (select self ,children) iter_mut) ())
        ,(recur body))]
    [(`(recur (cursor ,children)))
     ;`(call (select ,(symbol-append children '_cur) ,trav-name) ())
     `(skip)]
    [(`(recur (child ,child)))
     `(call (select (select class ,child) ,trav-name) ())])
  (recur statement))

;(define (build-class-visitor trav-name visitor-case)
;  (match visitor-case
;    [`(=> ,class-name ,visitor-stmt)
;     `(=> (constructor ,(symbol-append 'Is class-name) (tuple (ref (mut class))))
;          ,(build-statement trav-name visitor-stmt))]))

(define (build-visitors-for-interface trav-name visitor)
  (match visitor
    [`(: ,iface-name (case ,visitor-cases ...))
     (for/list ([visitor-case visitor-cases])
       (match visitor-case
         [`(=> ,class-name ,visitor-stmt)
          `(impl
            (for ,iface-name ,class-name)
            (fn ,trav-name ((: self (ref (mut Self)))) (unit)
                ,(build-statement trav-name visitor-stmt)))]))]))

(define (build-visitors schedule)
  (define/match (recur sched)
    [(`(seq ,left-sched ,right-sched))
     (append (recur left-sched) (recur right-sched))]
    [(`(par ,left-sched ,right-sched))
     (append (recur left-sched) (recur right-sched))]
    [(`(trav ,order ,visitor-list ...))
     (append-map (curry build-visitors-for-interface order) visitor-list)])
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
  (append; header
          (list '(blank))
          (add-between (build-visitors schedule) `(blank))
          ;(list `(blank) (build-evaluator (ag-grammar-root grammar) schedule 'evaluate))
          ))

#lang rosette

(require "../grammar/syntax.rkt")

(provide elaborate-program)

(define (symbol-subscript base sub)
  (string->symbol (string-append (symbol->string base) (number->string sub))))

; program ::= (datatype sort (field ...) (of kind (child ...) (field ...)) ...)
;           | (function func (~ sort (=> kind (statement ...))) ...)
;
; statement ::= (call func link)
;             | (for+ link (statement ...))
;             | (for- link (statement ...))
;             | (:= location expression[location])
;
; location ::= (unit link label)
;            | (first link label)
;            | (pred link label)
;            | (curr link label)
;            | (succ link label)
;            | (last link label)
;
; field ::= (: label type)
;
; child ::= (: link (unit sort))
;         | (: link (star sort))
;         | (: link (plus sort))
;
; sort | kind ::= symbol
; link ::= symbol | self | priv
; label ::= symbol
; type ::= symbol
; func ::= symbol

(define/match (elaborate-child child)
  [((ag:child/one name (ag:interface type _ _))) `(: ,name (unit ,type))]
  [((ag:child/seq name (ag:interface type _ _))) `(: ,name (star ,type))])

(define/match (elaborate-field attribute)
  [((ag:label name type)) `(: ,name ,type)])

(define (elaborate-data G)
  (for/list ([interface (ag:grammar-interfaces G)])
    (define public (ag:interface-labels interface))
    (define variants
      (for/list ([class (ag:interface-classes interface)])
        (define private
          (append* (ag:class-labels class)
                   (map ag:trait-labels (ag:class-traits class))))
        (define arity (ag:class-children* class))
        `(of ,(ag:class-name class)
             ,(map elaborate-child arity)
             ,(map elaborate-field private))))
    `(datatype ,(ag:interface-name interface)
               ,(map elaborate-field public)
               .
               ,variants)))

(define (elaborate-iteration class order rev? child commands)
  (define virt (if rev? 'succ 'pred))
  (define base (if rev? 'last 'first))
  (define loop (if rev? 'for- 'for+))
  (define virtual-statements
    (append-map (match-lambda
                  [(ag:eval attr)
                   (let* ([rule (ag:class-ref*/rule class attr)]
                          [term (ag:rule-fold-init rule)])
                     (if term
                         (list `(:= (accum ,attr) ,term))
                         null))]
                  [(ag:recur (== child))
                   null])
                commands))
  (define basis-statements
    (append-map (match-lambda
                  [(ag:eval attr)
                   (let* ([rule (ag:class-ref*/rule class attr)]
                          [expr (ag:rule-fold-next rule)])
                     (if expr
                         (list `(:= (base ,attr) ,expr))
                         null))]
                  [(ag:recur (== child))
                   (list `(call ,order ((,base ,child))))])
                commands))
  (define iterative-statements
    (map (match-lambda
           [(ag:eval (cons node label))
            `(:= ((curr ,node) . ,label) ,(ag:class-ref*/rule class (cons node label)))]
           [(ag:recur (== child))
            `(call ,order ((curr ,child)))])
         commands))
  (append virtual-statements
          basis-statements
          (list `(,loop ,child ,iterative-statements))))

(define (elaborate-visitor class order visitor)
  (append-map (match-lambda
                [(ag:iter/left child body)
                 (list `(when ,child ,(elaborate-iteration class order #f child body)))]
                [(ag:iter/right child body)
                 (elaborate-iteration class order #t child body)]
                [(ag:eval (cons node label))
                 (let* ([rule (ag:class-ref*/rule class (cons node label))]
                        [expr (ag:rule-formula rule)])
                   (list `(:= ((unit ,node) . ,label) ,expr)))]
                [(ag:recur child)
                 (list `(call ,order ((unit ,child))))]
                [(ag:skip)
                 null])
              visitor))

(define (elaborate-code G S)
  (match S
    [(ag:parallel S1 S2)
     (append (elaborate-code G S1)
             (elaborate-code G S2))]
    [(ag:sequential S1 S2)
     (append (elaborate-code G S1)
             (elaborate-code G S2))]
    [(ag:traversal order visitors)
     (define cases
       (for/list ([(kind visitor) (in-dict visitors)])
         (let* ([class (ag:grammar-ref/class G kind)]
                [sort (ag:interface-name (ag:class-interface class))])
           `(~ ,sort (=> ,kind ,(elaborate-visitor class order visitor))))))
     (define branches
       (map (λ (group) `(~ ,(second (first group)) . ,(map third group)))
            (group-by second cases eq?)))
     (list `(function ,order ,branches))]))

(define/match (elaborate-main S)
  [((ag:parallel S1 S2))
   `(join ,(elaborate-main S1)
          ,(elaborate-main S2))]
  [((ag:sequential S1 S2))
   `(then ,(elaborate-main S1)
          ,(elaborate-main S2))]
  [((ag:traversal order visitors))
   order])

(define (elaborate-program G S)
  (define S0 (preprocess-schedule S))
  (append (elaborate-data G)
          (elaborate-code G S0)
          (list `(main ,(elaborate-main S0)))))

(define/match (preprocess-command visitor)
  [((ag:iter/left ,child ,body))
   (list `(iter-left ,child ,(preprocess-command body)))]
  [((ag:iter/right ,child ,body))
   (list `(iter-right ,child ,(preprocess-command body)))]
  [((ag:eval (cons node label)))
   (list `(eval ,node ,label))]
  [((ag:recur ,child))
   (list `(recur ,child))]
  [((ag:skip))
   (list `(skip))]
  [((list commands ...))
   (append-map preprocess-command commands)])

(define (preprocess-schedule S)
  (define used (make-hasheq))
  (define/match (aux S)
    [((ag:parallel ,S1 ,S2))
     `(par ,(aux S1) ,(aux S2))]
    [((ag:sequential ,S1 ,S2))
     `(seq ,(aux S1) ,(aux S2))]
    [((ag:traversal ,order ,visitors))
     (let ([i (hash-ref! used order 0)])
       (hash-set! used order (+ i 1))
       `(trav ,order ; ,(symbol-subscript order i)
              ,(for/list ([(kind visitor) (in-dict visitors)])
                 `(,kind . ,(append-map preprocess-command visitor)))))])
  (aux S))

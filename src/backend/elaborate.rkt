#lang rosette

(require "../grammar/syntax.rkt")

(provide elaborate-program)

(define (symbol-subscript base sub)
  (string->symbol (string-append (symbol->string base) (number->string sub))))

;; (define (symbol-append . xs)
;;   (string->symbol (string-append* (map symbol->string xs))))

;; (define symbol-upcase
;;   (compose string->symbol string-upcase symbol->string))

;; (define symbol-downcase
;;   (compose string->symbol string-downcase symbol->string))

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
  [(`(,link . (unit ,sort))) `(: ,link (unit ,sort))]
  [(`(,link . (plus ,sort))) `(: ,link (plus ,sort))]
  [(`(,link . (star ,sort))) `(: ,link (star ,sort))])

(define/match (elaborate-field attribute)
  [(`(,label . (in ,type))) `(: ,label ,type)]
  [(`(,label . (out ,type))) `(: ,label ,type)])

(define (elaborate-data G)
  (for/list ([(sort classes) (in-dict (associate-classes G))])
    (define public (grammar-interface G sort))
    (define variants
      (for/list ([(kind class) (in-dict classes)])
        (define private (remove* public (ag-class-attributes class)))
        (define arity (ag-class-children class))
        `(of ,kind ,(map elaborate-child arity) ,(map elaborate-field private))))
    `(datatype ,sort ,(map elaborate-field public) . ,variants)))

(define (elaborate-iteration class order rev? child commands)
  (define virt (if rev? 'succ 'pred))
  (define base (if rev? 'last 'first))
  (define loop (if rev? 'for- 'for+))
  (define virtual-statements
    (append-map (match-lambda
                  [`(eval ,node ,label)
                   (let ([expr (class-rule-init class node label)])
                     (if expr
                         (list `(:= ((,virt ,node) . ,label) ,expr))
                         null))]
                  [`(recur ,(== child))
                   null])
                commands))
  (define basis-statements
    (append-map (match-lambda
                  [`(eval ,node ,label)
                   (let ([expr (class-rule-base class node label)])
                     (if expr
                         (list `(:= ((,base ,node) . ,label) ,expr))
                         null))]
                  [`(recur ,(== child))
                   (list `(call ,order ((,base ,child))))])
                commands))
  (define iterative-statements
    (map (match-lambda
           [`(eval ,node ,label)
            `(:= ((curr ,node) . ,label) ,(class-rule-step class node label))]
           [`(recur ,(== child))
            `(call ,order ((curr ,child)))])
         commands))
  (append virtual-statements
          basis-statements
          (list `(,loop ,child ,iterative-statements))))

(define (elaborate-visitor class order visitor)
  (append-map (match-lambda
                [`(iter-left ,child ,body)
                 (list `(when ,child ,(elaborate-iteration class order #f child body)))]
                [`(iter-right ,child ,body)
                 (elaborate-iteration class order #t child body)]
                [`(eval ,node ,label)
                 (let ([expr (class-rule-unit class node label)])
                   (list `(:= ((unit ,node) . ,label) ,expr)))]
                [`(recur ,child)
                 (list `(call ,order ((unit ,child))))]
                [`(skip)
                 null])
              visitor))

(define (elaborate-code G S)
  (match S
    [`(par ,S1 ,S2)
     (append (elaborate-code G S1)
             (elaborate-code G S2))]
    [`(seq ,S1 ,S2)
     (append (elaborate-code G S1)
             (elaborate-code G S2))]
    [`(trav ,order ,visitors)
     (define cases
       (for/list ([(kind visitor) (in-dict visitors)])
         (let* ([class (grammar-class G kind)]
                [sort (ag-class-interface class)])
           `(~ ,sort (=> ,kind ,(elaborate-visitor class order visitor))))))
     (define branches
       (map (λ (group) `(~ ,(second (first group)) . ,(map third group)))
            (group-by second cases eq?)))
     (list `(function ,order ,branches))]))

(define/match (elaborate-main S)
  [(`(par ,S1 ,S2))
   `(join ,(elaborate-main S1)
          ,(elaborate-main S2))]
  [(`(seq ,S1 ,S2))
   `(then ,(elaborate-main S1)
          ,(elaborate-main S2))]
  [(`(trav ,order ,visitors))
   order])

(define (elaborate-program G S)
  (define S0 (preprocess-schedule S))
  (append (elaborate-data G)
          (elaborate-code G S0)
          (list `(main ,(elaborate-main S0)))))

(define/match (preprocess-command visitor)
  [(`(iter-left ,child ,body))
   (list `(iter-left ,child ,(preprocess-command body)))]
  [(`(iter-right ,child ,body))
   (list `(iter-right ,child ,(preprocess-command body)))]
  [(`(eval ,node ,label))
   (list `(eval ,node ,label))]
  [(`(recur ,child))
   (list `(recur ,child))]
  [(`(skip))
   (list `(skip))]
  [((list commands ...))
   (append-map preprocess-command commands)])

(define (preprocess-schedule S)
  (define used (make-hasheq))
  (define/match (aux S)
    [(`(par ,S1 ,S2))
     `(par ,(aux S1) ,(aux S2))]
    [(`(seq ,S1 ,S2))
     `(seq ,(aux S1) ,(aux S2))]
    [(`(trav ,order ,visitors))
     (let ([i (hash-ref! used order 0)])
       (hash-set! used order (+ i 1))
       `(trav ,order ; ,(symbol-subscript order i)
              ,(for/list ([(kind visitor) (in-dict visitors)])
                 `(,kind . ,(append-map preprocess-command visitor)))))])
  (aux S))

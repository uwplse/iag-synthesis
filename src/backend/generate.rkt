#lang rosette

(require "../utility.rkt"
         "../grammar/syntax.rkt")

(provide generate-program)

; -----------------------
; Standard program header
; -----------------------

(define header
  null)

; ---------------------------------
; Generation of tree data structure
; ---------------------------------

(define/match (generate-child-field child)
  [((ag:child/one name (ag:interface sort _ _)))
   `(: ,name (gen Box (,sort)))]
  [((ag:child/seq name (ag:interface sort _ _)))
   `(: ,name (gen Vec (,sort)))])

(define/match (generate-label-field label)
  [((ag:label name type))
   `(: ,name ,type)])

(define (generate-class-field interface)
  (define sort (symbol-append (ag:interface-name interface) 'Class))
  `(: class ,sort))

(define (generate-class-variant class)
  (define name (ag:class-name class))
  (define fields (map generate-child-field (ag:class-children* class)))
  `(constructor ,name (record . ,fields)))

(define (generate-interface-enumeration interface)
  (define sort (symbol-append (ag:interface-name interface) 'Class))
  (define classes (ag:interface-classes interface))

  `(enum ,sort . ,(map generate-class-variant classes)))

(define (generate-interface-structure interface)
  (define sort (ag:interface-name interface))
  (define fields
    (cons (generate-class-field interface)
          (map generate-label-field (ag:interface-labels interface))))

  `(struct (constructor ,sort (record . ,fields))))

(define (generate-structure G)
  (define interfaces (ag:grammar-interfaces G))
  (define classes (ag:grammar-classes G))

  (append (map generate-interface-structure interfaces)
          (map generate-interface-enumeration interfaces)))

; ---------------------------------
; Generation of tree traversal code
; ---------------------------------

(define (generate-term class term)
  (define/match (recur term)
    [((ag:const v)) v]
    [((ag:field (cons 'self field)))
     `(select self ,field)]
    [((ag:field (cons child field)))
     (displayln (ag:class-ref*/child class child))
     (define node
       (if (ag:child/seq? (ag:class-ref*/child class child))
           (symbol-append child '_i)
           child))
     `(select ,node ,field)]
    [((ag:accum (cons object field)))
     (symbol-join (list object field) "_")]
    [((ag:index/first (cons child field) default))
     (define first-child `(call (select ,child first) ()))
     `(call (select ,first-child map_or_else)
            ((lambda () ,(recur default))
             (lambda (node) (select node ,field))))]
    [((ag:index/last (cons child field) default))
     (define last-child `(call (select ,child last) ()))
     `(call (select ,last-child map_or_else)
            ((lambda () ,(recur default))
             (lambda (node) (select node ,field))))]
    [((ag:ite condition consequent alternate))
     `(if ,(recur condition)
          ,(recur consequent)
          ,(recur alternate))]
    [((ag:expr operator operands))
     `(,operator . ,(map recur operands))]
    [((ag:call function (cons head-argument tail-arguments)))
     `(call (select ,(recur head-argument) ,function)
            ,(map recur tail-arguments))])
  (recur term))

(define (generate-bindings class commands)
  (for/list ([rule (filter-map (curry ag:eval->rule class) commands)]
             #:when (ag:rule-folds? rule))
    (match-define (cons object field) (ag:rule-attribute rule))
    (define variable (symbol-join (list object field) "_"))
    (define term (ag:rule-fold-init rule))
    `(let-mut ,variable ,(generate-term class term))))

(define (generate-command function class command #:iterated? [iterated? #f])
  (define recur (curry generate-command function class))
  (match command
    [(ag:iter/left child commands)
     (define iterator (symbol-append child '_i))
     (define body (append-map (recur #:iterated? #t) commands))
     (append (generate-bindings class commands)
             (list `(for ,iterator (ref (mut ,child))
                         (do . ,body))))]
    [(ag:iter/right child commands)
     (define iterator (symbol-append child '_i))
     (define body (append-map (recur #:iterated? #t) commands))
     (append (generate-bindings class commands)
             (list `(for ,iterator (call (select (ref (mut ,child)) rev) ())
                         (do . ,body))))]
    [(ag:eval attr)
     (define rule (ag:class-ref*/rule class attr))
     (define term
       (match (ag:rule-formula rule)
         [(ag:fold init next) (if iterated? next init)]
         [term term]))
     (define target (generate-term class (ag:field attr)))
     (list `(:= ,target ,(generate-term class term)))]
    [(ag:recur child)
     (define receiver (if iterated? (symbol-append child '_i) child))
     (list `(call (select ,receiver ,function) ()))]
    [(ag:skip)
     (list `(skip))]))

(define (generate-visitor name visitor)
  (define class (ag:visitor-class visitor))
  (define sort (ag:interface-name (ag:class-interface class)))
  (define kind (ag:class-name class))
  (define children (ag:class-children* class))

  (define commands (ag:visitor-commands visitor))
  (define body (append-map (curry generate-command name class) commands))

  `(=> (constructor (:: ,sort ,kind) (record . ,(map ag:child-name children)))
       (do . ,body)))

(define (generate-traversal G traversal)
  (define name (ag:traversal-name traversal))

  (for/list ([interface (ag:grammar-interfaces G)])
    (define sort (ag:interface-name interface))
    (define cases
      (map (curry generate-visitor name)
           (ag:traversal-ref/interface traversal interface)))

    `(impl ,sort
           (fn ,name ((: self (ref (mut Self)))) (unit)
               (do (match (select self class)
                     .
                     ,cases))))))

(define (generate-program G S)
  (append header
          (add-between (generate-structure G) `(blank))
          (list `(blank))
          (add-between (generate-traversal G S) `(blank))))

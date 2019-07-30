#lang rosette

(require "../utility.rkt"
         "../grammar/syntax.rkt")

(provide generate-program)

; -----------------------
; Standard program header
; -----------------------

(define header
  (list `(use style (StyledNode Style Display Edge Pixels))
        `(use paint (DisplayList DisplayCommand))
        `(use std default Default)
        `(use itertools Itertools)
        `(blank)))

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
  `(variant ,name (record . ,fields)))

(define (generate-interface-enumeration interface)
  (define sort (symbol-append (ag:interface-name interface) 'Class))
  (define classes (ag:interface-classes interface))

  `(enum ,sort . ,(map generate-class-variant classes)))

(define (generate-interface-structure interface)
  (define sort (ag:interface-name interface))
  (define fields
    (cons (generate-class-field interface)
          (map generate-label-field (ag:interface-labels interface))))

  `(struct ,sort (record . ,fields)))

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
     #:when (ag:class-ref*/child class child)
     (define child-i (symbol-append child '_i))
     `(select ,child-i ,field)]
    [((ag:field (cons child field)))
     `(select ,child ,field)]
    [((ag:accum (cons object field)))
     `(select ,object ,field)]
    [((ag:index (cons child field) default))
     (define endpoint (if (ag:index/first? term) 'first 'last))
     (define first-child `(call (select ,child ,endpoint) ()))
     `(call (select ,first-child map_or_else)
            ((lambda () ,(recur default))
             (lambda (node) (select node ,field))))]
    [((ag:ite condition consequent alternate))
     `(if ,(recur condition)
          ,(recur consequent)
          ,(recur alternate))]
    [((ag:expr operator operands))
     `(,operator . ,(map recur operands))]
    [((ag:call function (cons receiver arguments)))
     `(call (select ,(recur receiver) ,function)
            ,(map recur arguments))])
  (recur term))

(define (generate-command function class command #:iterated? [iterated? #f])
  (define recur (curry generate-command function class))
  (match command
    [(ag:iter child commands)
     (define reversed? (ag:iter-rev? command))
     (define iterator `(call (select ,child iter_mut) ()))
     (define cursor (symbol-append child '_i))
     (define initial (append-map (recur #:iterated? #f) commands))
     (define action (append-map (recur #:iterated? #t) commands))
     (append initial
             (list `(for ,cursor ,(if reversed? `(call (select ,iterator rev) ()) iterator)
                      (do . ,action))))]
    [(ag:eval attr)
     (define rule (ag:class-ref*/rule class attr))
     (define term
       (match (ag:rule-formula rule)
         [(ag:fold init next) (if iterated? next init)]
         [term term]))
     (define target (generate-term class (ag:field attr)))
     (list `(:= ,target ,(generate-term class term)))]
    [(ag:recur child)
     (define child-i (symbol-append child '_i))
     (if (implies (ag:child/seq? (ag:class-ref*/child class child)) iterated?)
         (let ([receiver (if iterated? child-i child)])
           (list `(call (select ,receiver ,function) ())))
         null)]
    [(ag:skip)
     (list `(skip))]))

(define (generate-visitor name visitor)
  (define class (ag:visitor-class visitor))
  (define interface (ag:class-interface class))
  (define commands (ag:visitor-commands visitor))

  (define sort (symbol-append (ag:interface-name interface) 'Class))
  (define kind (ag:class-name class))
  (define variant `(:: ,sort ,kind))
  (define fields (map ag:child-name (ag:class-children* class)))
  (define pattern `(constructor ,variant (record . ,fields)))
  (define body (append-map (curry generate-command name class) commands))

  `(=> ,pattern (do . ,body)))

(define (generate-traversal G traversal)
  (define name (ag:traversal-name traversal))

  (for/list ([interface (ag:grammar-interfaces G)])
    (define sort (ag:interface-name interface))
    (define cases
      (map (curry generate-visitor name)
           (ag:traversal-ref/interface traversal interface)))

    `(impl (life a ,sort)
           (fn ,name ((: self (ref (mut Self)))) (unit)
               (do (match (ref (mut (select self class)))
                     .
                     ,cases))))))

(define (generate-program G S)
  (append header
          (add-between (generate-structure G) `(blank))
          (list `(blank))
          (add-between (generate-traversal G S) `(blank))))

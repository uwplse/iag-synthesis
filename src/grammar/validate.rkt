#lang rosette

(require "../utility.rkt"
         "syntax.rkt")

(provide validate-grammar)

(define/match (contextualize item)
  [((ag:interface name _ _)) (format "in interface '~a'" name)]
  [((ag:class name _ _ _ _)) (format "in class '~a'" name)]
  [((ag:trait name _)) (format "in trait '~a'" name)]
  [((ag:traversal name _)) (format "in traversal '~a'" name)]
  [((ag:child/one name _)) (format "on scalar child '~a'" name)]
  [((ag:child/seq name _)) (format "on vector child '~a'" name)]
  [((ag:label name _)) (format "for label '~a'" name)]
  [((ag:rule attr _ #f)) (format "in scalar rule for '~a'" (ag:attribute->string attr))]
  [((ag:rule attr _ (? symbol?))) (format "in vector rule for '~a'" (ag:attribute->string attr))])

(define context
  (make-parameter null (λ (item) (cons (contextualize item) (context)))))

(define (reject problem name)
  (define ident (if (cons? name) (ag:attribute->string name) name))
  (define message (string-join (cons (format problem ident) (context)) " "))
  (displayln message)
  (raise-user-error 'validate message))

(define (validate-target G class attribute)
  (match attribute
    [(cons 'self field)
     (match (ag:class-ref*/label class field #:partial? #t)
       [(ag:label/in _ _)
        (reject "Cannot define input field '~a' on self" field)]
       [(ag:label/out _ _)
        (void)]
       [#f
        (reject "No such field '~a' on self" field)])]
    [(cons node field)
     (define child (ag:class-ref*/child class node #:partial? #t))
     (parameterize ([context child])
       (match (ag:interface-ref/label (ag:child-interface child) field)
         [(ag:label/in _ _)
          (reject "Cannot define input field '~a'" field)]
         [(ag:label/out _ _)
          (void)]
         [#f
          (reject "No such field '~a'" field)]))]))

(define (validate-term G class term [iterates #f])
  (define/match (recur term)
    [((ag:const _)) (void)]
    [((ag:field (cons 'self field)))
     (unless (ag:class-ref*/label class field #:partial? #t)
       (reject "Undefined field '~a' on self" field))]
    [((ag:field (cons node field)))
     (define child (ag:class-ref*/child class node #:partial? #t))
     (unless child
       (reject "No such child '~a'" node))
     (parameterize ([context child])
       (unless (ag:interface-ref/label (ag:child-interface child) field)
         (reject "Undefined field '~a'" field))
       (unless (implies (ag:child/seq? child) (eq? node iterates))
         (reject "Mismatched iteration for field '~a'" field)))]
    [((ag:accum attr))
     (define friend (ag:class-ref*/rule class attr #:partial? #t))
     (unless friend
       (reject "No such attribute '~a'" attr))
     (unless (ag:rule-folds? friend)
       (reject "No such accumulator for attribute '~a'" attr))
     (unless (eq? (ag:rule-iteration friend) iterates)
       (reject "Mismatched iteration for accumulator of attribute '~a'" attr))]
    [((ag:index (cons node field) default))
     (define child (ag:class-ref*/child class node #:partial? #t))
     (unless child
       (reject "No such child '~a'" node))
     (parameterize ([context child])
       (unless (ag:interface-ref/label (ag:child-interface child) field)
         (reject "Undefined field '~a'" field))
       (unless (ag:child/seq? child)
         (reject "Cannot index first/last field '~a'" field)))]
    [((ag:expr _ operands)) (for-each recur operands)]
    [((ag:call _ arguments)) (for-each recur arguments)]
    [((ag:ite if then else)) (for-each recur (list if then else))])
  (recur term))

; Validate well-formedness of the rule statement.
(define (validate-rule G class rule)
  (validate-target G class (ag:rule-attribute rule))
  (match (ag:rule-formula rule)
    [(ag:fold init next)
     (validate-term G class init)
     (validate-term G class next (ag:rule-iteration rule))]
    [term
     (validate-term G class term (ag:rule-iteration rule))]))

; Validate well-formedness of the class.
(define (validate-class G class)

  (when/let ([duplicate (check-duplicates (map ag:trait-name (ag:class-traits class)))])
    (reject "Duplicate trait inclusions for '~a'" duplicate))

  (when/let ([duplicate (check-duplicates (map ag:child-name (ag:class-children* class)))])
    (reject "Duplicate child declarations for '~a'" duplicate))

  (when/let ([duplicate (check-duplicates (map ag:label-name (ag:class-labels* class)))])
    (reject "Duplicate field declarations for '~a'"))

  (when/let ([duplicate (check-duplicates (map ag:rule-attribute (ag:class-rules* class)))])
    (reject "Duplicate rule definitions for '~a'" duplicate))

  (for ([rule (ag:class-rules* class)])
    (parameterize ([context rule])
      (validate-rule G class rule))))

; Validate well-formedness of the attribute grammar.
(define (validate-grammar G)

  (when/let ([duplicate (check-duplicates (map ag:interface-name (ag:grammar-interfaces G)))])
    (reject "Duplicate interfaces for '~a'" duplicate))

  (when/let ([duplicate (check-duplicates (map ag:class-name (ag:grammar-classes G)))])
    (reject "Duplicate classes for '~a'" duplicate))

  (when/let ([duplicate (check-duplicates (map ag:trait-name (ag:grammar-traits G)))])
    (reject "Duplicate traits for '~a'" duplicate))

  (when/let ([duplicate (check-duplicates (map ag:traversal-name (ag:grammar-traversals G)))])
    (reject "Duplicate traversals for '~a'" duplicate))

  (for ([class (ag:grammar-classes G)])
    (parameterize ([context class])
      (validate-class G class))))

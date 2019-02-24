#lang rosette

; Typechecker for language of attribute grammars

(require "syntax.rkt")

(provide typecheck-object
         typecheck-label
         typecheck-expression
         undefined-function
         ag-typecheck)

; This language defines only the types bool, int, float, and string. The usual
; assortment of binary and unary operators are also present but inextensible.
; It is assumed that any two values of the same type are equal?-comparable.
; The backend may require additional information to handle additional types.
; equality function).

(define undefined-function
  (curry raise-user-error 'typecheck "undefined function '~a'"))

; Raise a user error to indicate that an object identifier is undefined.
(define (undefined-object class-ast object)
  (raise-user-error 'typecheck
                    "undefined child '~a' in class ~a"
                    object
                    (ag-class-name class-ast)))

; For a given class and object, return the pair of the object's interface name
; (or class name, if the object is self) and a boolean indicating whether the
; object is a sequence.
(define (typecheck-object class-ast object)
  (let* ([child-ast-list (ag-class-children class-ast)]
         [child-ast (lookup-child child-ast-list object)])
    (cond
      [(eq? object 'self) (cons (ag-class-name class-ast) #f)]
      [child-ast (cons (ag-child-interface child-ast)
                       (ag-child-sequence child-ast))]
      [else (undefined-object class-ast object)])))

; For a given class, object, and label, return the attribute's type.
(define (typecheck-label class-ast iface-ast-list object label)
  (let* ([child-ast-list (ag-class-children class-ast)]
         [child-ast (lookup-child child-ast-list object)])
    (cond
      [(eq? object 'self)
       (let* ([class-label-ast-list (ag-class-labels class-ast)]
              [class-label-ast (lookup-label class-label-ast-list label)]
              [iface-name (ag-class-interface class-ast)]
              [iface-ast (lookup-interface iface-ast-list iface-name)]
              [iface-label-ast-list (ag-interface-labels iface-ast)]
              [iface-label-ast (lookup-label iface-label-ast-list label)]
              [label-ast (or class-label-ast iface-label-ast)])
         (if label-ast
             (ag-label-type label-ast)
             (raise-user-error 'typecheck
                               "undeclared attribute '~a' for class ~a"
                               label
                               (ag-class-name class-ast))))]
      [child-ast
       (let* ([iface-name (ag-child-interface child-ast)]
              [iface-ast (lookup-interface iface-ast-list iface-name)]
              [label-ast-list (ag-interface-labels iface-ast)]
              [label-ast (lookup-label label-ast-list label)])
         (if label-ast
             (ag-label-type label-ast)
             (raise-user-error 'typecheck
                               "undeclared attribute '~a' for interface ~a of child ~a"
                               label
                               iface-name
                               object)))]
      [else
       (undefined-object class-ast object)])))

(define (boolean-type? typename)
  (eq? typename 'bool))

(define (numeric-type? typename)
  (or (eq? typename 'int)
      (eq? typename 'float)))

(define (string-type? typename)
  (eq? typename 'string))

(define (typecheck-literal value)
  (cond
    [(boolean? value) 'bool]
    [(integer? value) 'int]
    [(number? value) 'float]
    [(string? value) 'string]
    [else (raise-user-error 'typecheck
                            "unknown literal value '~a'"
                            value)]))

(define (typecheck-unary operator-symbol operand-type)
  (define operand-okay?
    (match operator-symbol
      [(or '- '+) numeric-type?]
      ['! boolean-type?]
      [_ (raise-user-error 'typecheck
                           "illegal unary operator '~a'"
                           operator-symbol)]))
  (if (operand-okay? operand-type)
      operand-type
      (raise-user-error 'typecheck
                        "illegal application of unary '~a' to an expression of type '~a'"
                        operator-symbol
                        operand-type)))

(define (typecheck-binary operator-symbol left-type right-type)
  (define result-type
    ; assume that left-type = right-type
    (match operator-symbol
      [(or '== '!=) 'bool]
      [(or '< '<= '>= '>) (and (numeric-type? left-type) 'bool)]
      [(or '+ '- '* '/) (and (numeric-type? left-type) left-type)]
      [(or '&& '||) (and (boolean-type? left-type) left-type)]
      [_ (raise-user-error 'typecheck
                           "illegal binary operator '~a'")]))
  (cond
    [(not (eq? left-type right-type))
     (raise-user-error 'typecheck
                       "illegal application of binary '~a' to different types '~a' and '~a'"
                       operator-symbol
                       left-type
                       right-type)]
    [(not result-type)
     (raise-user-error 'typecheck
                       "illegal application of binary '~a' to operands of type '~a'"
                       operator-symbol
                       left-type)]
    [else result-type]))

(define (typecheck-conditional cond-type then-type else-type)
  (cond
    [(not (boolean-type? cond-type))
     (raise-user-error 'typecheck
                       "non-boolean condition expression of type '~a'"
                       cond-type)]
    [(not (eq? then-type else-type))
     (raise-user-error 'typecheck
                       "cannot unify conditional branch types '~a' and '~a'"
                       then-type
                       else-type)]
    [else then-type]))

(define (typecheck-application typecheck-function function-symbol argument-types)
  (let* ([function-type (typecheck-function function-symbol)]
         [domain-types (car function-type)]
         [codomain-type (cdr function-type)]
         [arity (length domain-types)])
    (cond
      [(not (eq? arity (length argument-types)))
       (raise-user-error 'typecheck
                         "function '~a' has ~a parameters but got ~a arguments"
                         function-symbol
                         arity
                         (length argument-types))]
      [(not (andmap eq? domain-types argument-types))
       (raise-user-error 'typecheck
                         "ill-typed application of function '~a'"
                         function-symbol)]
      [else codomain-type])))

; It is up to the caller to know to typecheck the seed and the step in contexts
; with and without the accumulator, respectively. Similarly for the fold states.
(define (typecheck-fold init-type iter-type)
  (if (eq? init-type iter-type)
      init-type
      (raise-user-error 'typecheck
                        "cannot unify fold seed type '~a' and fold step type '~a'"
                        init-type iter-type)))

(define (typecheck-expression class-ast
                              iface-ast-list
                              expr-ast
                              target-object
                              target-label
                              loop-object
                              [typecheck-function undefined-function])
  (define/match (recurse expr-ast)
    ; unary operation
    [((ag-expr-unary operator operand))
     (let ([operand-type (recurse operand)])
       (typecheck-unary operator operand-type))]

    ; binary operation
    [((ag-expr-binary left-operand operator right-operand))
     (let ([left-type (recurse left-operand)]
           [right-type (recurse right-operand)])
       (typecheck-binary operator left-type right-type))]

    ; conditional expression
    [((ag-expr-condition condition then-branch else-branch))
     (let ([cond-type (recurse condition)]
           [then-type (recurse then-branch)]
           [else-type (recurse else-branch)])
       (typecheck-conditional cond-type then-type else-type))]

    ; function application
    [((ag-expr-call function arguments))
     (let ([argument-types (map recurse arguments)])
       (typecheck-application typecheck-function function argument-types))]

    ; attribute reference
    [((ag-expr-reference object index label))
     (let* ([label-type (typecheck-label class-ast iface-ast-list object label)]
            [sequence (cdr (typecheck-object class-ast object))]
            [recursive (and (eq? object target-object) (eq? label target-label))]
            [regular (and (not index) (not recursive) (not sequence))]
            [initial (and (eq? index 'first) (not recursive) sequence)]
            [final (and (eq? index 'last) (not recursive) sequence)]
            [looping (and (or (eq? index 'current) (not index))
                          (eq? object loop-object) (not recursive))]
            ; This assumes that the attribute is defined by a fold (possibly not
            ; this one). NOTE: Maybe we should actually check that.
            [folding (or (eq? index 'previous)
                         (and (eq? index 'current) (not recursive)))])

       (if (or regular initial final looping folding)
           label-type
           (raise-user-error 'typecheck
                             "bad attribute reference: ~a"
                             (ag-expr-reference->string expr-ast))))]

    ; value literal
    [(_)
     (typecheck-literal expr-ast)])

  (recurse expr-ast))

(define (typecheck-rule class-ast
                        iface-ast-list
                        rule-ast
                        [typecheck-function undefined-function])
  (match-define (cons object label) (ag-rule-left rule-ast))

  (define inferred-type
    (match (ag-rule-right rule-ast)
      [(ag-loop loop-object fold-expr)
       (unless (cdr (typecheck-object class-ast loop-object)) ; sequence child?
         (raise-user-error 'typecheck
                           "evaluation rule defining attribute ~a.~a iterating over non-sequence child ~a"
                           object label loop-object))
       (define (typecheck iterated expr-ast)
         (typecheck-expression class-ast iface-ast-list expr-ast object label
                               (and iterated loop-object) typecheck-function))
       (match fold-expr
         [(ag-fold init-expr iter-expr)
          (let ([init-type (typecheck #f init-expr)]
                [iter-type (typecheck #t iter-expr)])
            (typecheck-fold init-type iter-type))]
         [expr-ast
          (unless (eq? loop-object object)
            (raise-user-error 'typecheck
                              "evaluation rule redefining non-sequence attribute ~a.~a iterating over sequence child ~a"
                              object label loop-object))
          (typecheck #t expr-ast)])]
      [expr-ast
       (typecheck-expression class-ast iface-ast-list expr-ast object label #f
                             typecheck-function)]))

  (let ([annotated-type (typecheck-label class-ast iface-ast-list object label)])
    (cond
      [(not annotated-type)
       (raise-user-error 'typecheck
                         "evaluation rule for nonexistent attribute '~a.~a'"
                         object)]
      [(eq? annotated-type inferred-type)
       annotated-type]
      [else
       (raise-user-error 'typecheck
                         "ill-typed evaluation rule for '~a.~a': expected '~a' but got '~a'"
                         object
                         label
                         annotated-type
                         inferred-type)])))

(define (typecheck-class class-ast iface-ast-list typecheck-function)
  ; Generate a generic error for a duplicated label or property.
  (define (duplicate-error part name)
    (raise-user-error 'typecheck
                      "duplicate ~a '~a' in class ~a"
                      part
                      (if (symbol? name) (symbol->string name) name)
                      (ag-class-name class-ast)))

  ; Check that attribute labels are locally unique.
  (let* ([iface-name (ag-class-interface class-ast)]
         [iface-ast (findf (compose (curry eq? iface-name)
                                    ag-interface-name)
                           iface-ast-list)]
         [iface-labels (ag-interface-labels iface-ast)]
         [class-labels (ag-class-labels class-ast)]
         [labels (append class-labels iface-labels)]
         [duplicate (check-duplicates labels eq? #:key ag-label-name)])
    (when duplicate
      (duplicate-error "attribute declaration" (ag-label-name duplicate))))

  ; Check that no class defines an attribute more than once.
  (let ([duplicate (check-duplicates (ag-class-rules class-ast) equal?
                                     #:key ag-rule-left)])
    (when duplicate
      (match-let ([(cons object label) (ag-rule-left duplicate)])
        (duplicate-error "evaluation rule for attribute '~a.~a'"
                         object
                         label))))

  ; Check that no class defines a child more than once.
  (let* ([children (ag-class-children class-ast)]
         [duplicate (check-duplicates children eq? #:key ag-child-name)])
    (when duplicate
      (duplicate-error "child declaration" (ag-child-name duplicate))))

  ; Typecheck each evaluation rule
  (for ([rule (ag-class-rules class-ast)])
    (typecheck-rule class-ast iface-ast-list rule typecheck-function)))

; Typecheck a list of normalized class ASTs with respect to a list of interface
; ASTs and function typechecker, which implements the function type context for
; attribute expressions.
(define (ag-typecheck grammar [typecheck-function undefined-function])
  (for ([class-ast (ag-grammar-classes grammar)])
    (typecheck-class class-ast (ag-grammar-interfaces grammar) typecheck-function)))

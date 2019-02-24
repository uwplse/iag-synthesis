#lang rosette

; Abstract Syntax for Language of Attribute Grammars

(provide (all-defined-out))

; Note that identifiers are symbols

(struct ag-grammar
  (; identifier of root interface
   root
   ; list of interface ASTs
   interfaces
   ; list of normalized class ASTs
   classes
   ) #:transparent)

; interface definition
(struct ag-interface
  (; identifier corresponding to nonterminal in the attribute grammar
   name
   ; list of attribute declarations
   labels
   ) #:transparent)

; trait (mixin class) definition
(struct ag-trait
  (; identifier
   name
   ; class body
   body
   ) #:transparent)

; class definition
(struct ag-class
  (; identifier corresponding to a particular production for the nonterminal
   name
   ; list of trait identifiers
   traits
   ; the nonterminal symbol
   interface
   ; a class body
   body
   ) #:transparent)

; body of a class or trait
(struct ag-body
  (; list of child declarations
   children
   ; list of attribute declarations
   labels
   ; list of attribute evaluation rules, possibly within loops
   rules
   ) #:transparent)

; child declaration
(struct ag-child
  (; child (more generally, object) identifier (cannot be 'self)
   name
   ; whether this is a child sequence
   sequence
   ; the interface type of this child node
   interface
   ) #:transparent)

; attribute declaration
(struct ag-label
  (; whether this attribute's value is given (#t) or computed (#f)
   input
   ; attribute identifier, also known as a label
   name
   ; the identifier for the attribute value's type
   type
   ) #:transparent)

; loop body
(struct ag-loop
  (; identifier of a sequence child
   object
   ; before normalizatiuon, a list of looped evaluation rules, and after
   ; normalization, either a fold or an expression
   body
   ) #:transparent)

; attribute evaluation rule (i.e. definition)
(struct ag-rule
  (; target attribute(s), as a pair of an object symbol and a label symbol
   left
   ; expression, fold, or loop (if normalized) to compute value(s)
   right
   ) #:transparent)

(struct ag-fold
  (; initial accumulator expression
   initial
   ; iteration accumulator expression
   iterate
   ) #:transparent)

; attribute reference
(struct ag-expr-reference
  (; object identifier
   object
   ; 'first, 'previous, 'last, or #f
   index
   ; attribute identifier
   label
   ) #:transparent)

; some simple expression AST nodes
(struct ag-expr-call (function arguments) #:transparent)
(struct ag-expr-unary (operator operand) #:transparent)
(struct ag-expr-binary (left operator right) #:transparent)
(struct ag-expr-condition (if then else) #:transparent)

; some handy two-hop accessors for class AST nodes
(define ag-class-children
  (compose ag-body-children ag-class-body))
(define ag-class-labels
  (compose ag-body-labels ag-class-body))
(define ag-class-rules
  (compose ag-body-rules ag-class-body))

(define/match (ag-expr-references expression [accumulator null])
  [((ag-expr-call _ arguments) _)
   (foldl accumulator ag-expr-references arguments)]
  [((ag-expr-unary _ operand) _)
   (ag-expr-references operand)]
  [((ag-expr-binary left _ right) _)
   (ag-expr-references left (ag-expr-references right accumulator))]
  [((ag-expr-condition if then else) _)
   (foldl accumulator ag-expr-references (list if then else))]
  [((? ag-expr-reference?) _) (cons expression accumulator)]
  [((or (? number?) (? boolean?) (? string?)) _) accumulator])

(define (ag-expr-reference->string reference)
  (string-append (symbol->string (ag-expr-reference-object reference))
                 (match (ag-expr-reference-index reference)
                   [#f ""]
                   ['first "$0"]
                   ['previous "$-"]
                   ['last "$$"])
                 "."
                 (symbol->string (ag-expr-reference-label reference))))

(define (ag-body-merge . body-ast-list)
  (ag-body (apply append (map ag-body-children body-ast-list))
           (apply append (map ag-body-labels body-ast-list))
           (apply append (map ag-body-rules body-ast-list))))

; Partition the list of top-level ASTs into three lists of ASTs: interfaces,
; traits, and classes.
(define (partition-ast-list ast-list)
  (let*-values ([(trait-ast-list other-ast-list)
                 (partition ag-trait? ast-list)]
                [(class-ast-list interface-ast-list)
                 (partition ag-class? other-ast-list)])
    (values interface-ast-list trait-ast-list class-ast-list)))

; Inline every inherited trait's body AST into the class's body AST and return
; the updated class AST.
(define (inline-traits trait-ast-list class-ast)
  (match-define (ag-class class-name trait-names interface-name class-body)
    class-ast)

  (define trait-bodies
    (for/list ([trait-name trait-names])
      (let ([trait-ast (lookup-trait trait-ast-list trait-name)])
        (if trait-ast
            (ag-trait-body trait-ast)
            (raise-user-error 'typecheck "undefined trait '~a'" trait-name)))))

  (ag-class class-name
            trait-names
            interface-name
            (apply ag-body-merge (cons class-body trait-bodies))))

; Normalize each loop, by replacement with a new loop node containing a
; particular evaluation rule for every rule in the original loop's body.
(define (normalize-loops class-ast)
  (match-let* ([(ag-class name traits interface body) class-ast]
               [(ag-body children labels rules) body])

    (define processed-rules
      (for/fold ([normalized-rules null])
                ([rule-or-loop rules])
        (match rule-or-loop
          [(ag-loop child rules)
           (for/fold ([normalized-rules normalized-rules])
                     ([rule rules])
             (match-let ([(ag-rule lhs rhs) rule])
               (cons (ag-rule lhs (ag-loop child rhs)) normalized-rules)))]
          [rule
           (cons rule normalized-rules)])))

    (ag-class name traits interface (ag-body children labels processed-rules))))

; Transform a list of top-level abstract syntax trees (AST) into a normal form,
; consisting of two lists. The first list holds all the class ASTs given in the
; original AST list (in the same order), where each class has been merged with
; its inherited traits and had its loops unbundled. The second list holds all
; the interface ASTs from the original AST list (again, in the same order). This
; transformation will raise a user error if any of the interface, trait, or class
; namespaces are ambiguous.
(define (ag-normalize root ast-list)
  (let-values ([(interface-ast-list trait-ast-list class-ast-list)
                (partition-ast-list ast-list)])
    ; check for ambiguity in the interface namespace
    (let ([conflict (check-duplicates interface-ast-list
                                      eq?
                                      #:key ag-interface-name)])
      (when conflict
        (raise-user-error 'normalize "interface name conflict: ~a" conflict)))
    ; check for ambiguity in the trait namespace
    (let ([conflict (check-duplicates trait-ast-list
                                      eq?
                                      #:key ag-trait-name)])
      (when conflict
        (raise-user-error 'normalize "trait name conflict: ~a" conflict)))
    ; check for ambiguity in the class namespace
    (let ([conflict (check-duplicates class-ast-list
                                      eq?
                                      #:key ag-class-name)])
      (when conflict
        (raise-user-error 'normalize "class name conflict: ~a" conflict)))
    ; perform the actual transformation(s)
    (let* ([inline-class (curry inline-traits trait-ast-list)]
           [normalize-class (compose normalize-loops inline-class)])
      (ag-grammar root interface-ast-list (map normalize-class class-ast-list)))))

; Look up an interface in a list of interface ASTs, returning #f if not found.
(define (lookup-interface interface-ast-list interface-name)
  (findf (λ (interface-ast) (eq? (ag-interface-name interface-ast) interface-name))
         interface-ast-list))

; Look up a trait in a list of trait ASTs, returning #f if not found.
(define (lookup-trait trait-ast-list trait-name)
  (findf (λ (trait-ast) (eq? (ag-trait-name trait-ast) trait-name))
         trait-ast-list))

; Look up a class in a list of class ASTs, returning #f if not found.
(define (lookup-class class-ast-list class-name)
  (findf (λ (class-ast) (eq? (ag-class-name class-ast) class-name))
         class-ast-list))

; Look up an object in a list of child [declaration] ASTs, returning #f if not
; found. Note that the self object will not be found, as it is not declared as
; a child node.
(define (lookup-child child-ast-list object)
  (findf (λ (child-ast) (eq? (ag-child-name child-ast) object))
         child-ast-list))

; Look up a label in a list of [attribute] declaration ASTs, returning #f if not
; found.
(define (lookup-label decl-ast-list label)
  (findf (λ (decl-ast) (eq? (ag-label-name decl-ast) label))
         decl-ast-list))

; Look up an evaluation rule in a list of evaluation rule ASTs, returning #f if
; not found.
(define (lookup-rule rule-ast-list object label)
  (let ([target (cons object label)])
    (findf (λ (rule-ast) (equal? (ag-rule-left rule-ast) target))
           rule-ast-list)))

(define (get-class grammar classname)
  (lookup-class (ag-grammar-classes grammar) classname))

(define (get-interface grammar classname)
  (lookup-interface (ag-grammar-interfaces grammar) classname))

(define (get-labels grammar class-ast)
  (let ([iface-ast (get-interface grammar (ag-class-interface class-ast))])
    (append (ag-interface-labels iface-ast)(ag-class-labels class-ast))))

; Extract the initial expression from the right-hand side of a looped evaluation
; rule, returning #f if not applicable.
(define/match (initial-expression rhs)
  [((ag-loop _ (ag-fold init-expr _))) init-expr]
  [(_) #f])

; Extract the iterative expression from the right-hand side of a looped
; evaluation rule.
(define/match (iterate-expression rhs)
  [((ag-loop _ (ag-fold _ iter-expr))) iter-expr]
  [((ag-loop _ expr)) expr]
  [(_) #f])

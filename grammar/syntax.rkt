#lang rosette

; Abstract syntax for language of attribute grammars

(provide (all-defined-out))

; NOTE: Identifiers are symbols.

(struct ag-grammar
  (; identifier of root interface
   root
   ; list of interface ASTs
   interfaces
   ; list of normalized class ASTs
   classes
   ; list of traversal templates
   traversals
   ) #:transparent)

(struct ag-traversal
  (; traversal type
   name
   ; list associating each classname to a list of traversal components (ag-trav-*)
   forms
   ) #:transparent)

(struct ag-trav-recur
  (; on what child node to recur
   child
   ) #:transparent)

(struct ag-trav-visit () #:transparent)

(struct ag-trav-loop
  (; through what child sequence to loop
   child
   ; iterated traversal components: recur on indexed node (void) or visit self
   body
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
   ; 'first, 'current, 'previous, 'last, or #f
   index
   ; attribute identifier
   label
   ) #:transparent)

; some simple expression AST nodes
(struct ag-expr-call (function arguments) #:transparent)
(struct ag-expr-unary (operator operand) #:transparent)
(struct ag-expr-binary (left operator right) #:transparent)
;(struct ag-expr-branch (condition consequent alternate) #:transparent) ; TODO
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
                   ['current "$i"]
                   ['previous "$-"]
                   ['last "$$"])
                 "."
                 (symbol->string (ag-expr-reference-label reference))))

(define (ag-body-merge . body-ast-list)
  (ag-body (apply append (map ag-body-children body-ast-list))
           (apply append (map ag-body-labels body-ast-list))
           (apply append (map ag-body-rules body-ast-list))))

; Partition the list of top-level ASTs into four lists of ASTs: traversals,
; interfaces, traits, and classes.
(define (partition-ast-list ast-list)
  (let*-values ([(class-ast-list other-ast-list) (partition ag-class? ast-list)]
                [(iface-ast-list other-ast-list) (partition ag-interface? other-ast-list)]
                [(trait-ast-list trav-ast-list) (partition ag-trait? other-ast-list)])
    (values trav-ast-list iface-ast-list trait-ast-list class-ast-list)))

; Inline every inherited trait's body AST into the class's body AST and return
; the updated class AST.
(define (inline-traits trait-ast-list class-ast)
  (match-define (ag-class class-name trait-names iface-name class-body)
    class-ast)

  (define trait-bodies
    (for/list ([trait-name trait-names])
      (let ([trait-ast (lookup-trait trait-ast-list trait-name)])
        (if trait-ast
            (ag-trait-body trait-ast)
            (raise-user-error 'typecheck "undefined trait '~a'" trait-name)))))

  (ag-class class-name
            trait-names
            iface-name
            (apply ag-body-merge (cons class-body trait-bodies))))

; Normalize each loop, by replacement with a new loop node containing a
; particular evaluation rule for every rule in the original loop's body.
(define (normalize-loops class-ast)
  (match-let* ([(ag-class name traits iface body) class-ast]
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

    (ag-class name traits iface (ag-body children labels processed-rules))))

; Inline all trait ASTs into the inheriting classes' ASTs, split loop AST nodes
; (so that each iterated evaluation rule has its own node as its right-hand side),
; and return lists of all components (traversals, classes, and interfaces) as a
; packaged attribute grammar.
(define (ag-normalize root ast-list)
  (let-values ([(trav-ast-list iface-ast-list trait-ast-list class-ast-list)
                (partition-ast-list ast-list)])
    ; check for ambiguity in the interface namespace
    (let ([conflict (check-duplicates iface-ast-list
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
      (ag-grammar root iface-ast-list (map normalize-class class-ast-list) trav-ast-list))))

; Look up a traversal in a list of traversal ASTs, returning #f if not found.
(define (lookup-traversal trav-ast-list trav-name)
  (findf (λ (trav-ast) (eq? (ag-traversal-name trav-ast) trav-name)) trav-ast-list))

; Look up an interface in a list of interface ASTs, returning #f if not found.
(define (lookup-interface iface-ast-list iface-name)
  (findf (λ (iface-ast) (eq? (ag-interface-name iface-ast) iface-name)) iface-ast-list))

; Look up a trait in a list of trait ASTs, returning #f if not found.
(define (lookup-trait trait-ast-list trait-name)
  (findf (λ (trait-ast) (eq? (ag-trait-name trait-ast) trait-name)) trait-ast-list))

; Look up a class in a list of class ASTs, returning #f if not found.
(define (lookup-class class-ast-list class-name)
  (findf (λ (class-ast) (eq? (ag-class-name class-ast) class-name)) class-ast-list))

; Look up an object in a list of child [declaration] ASTs, returning #f if not
; found. Note that the self object will not be found, as it is not declared as
; a child node.
(define (lookup-child child-ast-list object)
  (findf (λ (child-ast) (eq? (ag-child-name child-ast) object)) child-ast-list))

; Look up a label in a list of [attribute] declaration ASTs, returning #f if not
; found.
(define (lookup-label decl-ast-list label)
  (findf (λ (decl-ast) (eq? (ag-label-name decl-ast) label)) decl-ast-list))

; Look up the type of a label in a list of [attribute] declaration ASTs.
(define (lookup-type grammar class-ast object label)
  (define label-ast-list
    (if (equal? object 'self)
        (get-labels grammar class-ast)
        (let* ([child-ast (lookup-child (ag-class-children class-ast) object)]
               [iface-ast (get-interface grammar (ag-child-interface child-ast))])
          (ag-interface-labels iface-ast) label)))

  (ag-label-type (lookup-label label-ast-list)))

; Look up an evaluation rule in a class.
(define (lookup-rule class-ast object label)
  (let ([rule-ast-list (ag-class-rules class-ast)]
        [target (cons object label)])
    (findf (λ (rule-ast) (equal? (ag-rule-left rule-ast) target))
           rule-ast-list)))

(define (get-class grammar classname)
  (lookup-class (ag-grammar-classes grammar) classname))

(define (get-interface grammar iface-name)
  (lookup-interface (ag-grammar-interfaces grammar) iface-name))

(define (get-traversal grammar trav-name)
  (lookup-traversal (ag-grammar-traversals grammar) trav-name))

(define (get-labels grammar class-ast)
  (let ([iface-ast (get-interface grammar (ag-class-interface class-ast))])
    (append (ag-class-labels class-ast) (ag-interface-labels iface-ast))))

; Extract the initial expression from an evaluation rule, returning #f if not
; applicable.
(define (rule-initial rule)
  (match (ag-rule-right rule)
    [(ag-loop _ (ag-fold init-expr _)) init-expr]
    [_ #f]))

; Extract the iterative expression from an evaluation rule.
(define (rule-iterate rule)
  (match (ag-rule-right rule)
    [(ag-loop _ (ag-fold _ iter-expr)) iter-expr]
    [(ag-loop _ expr) expr]
    [_ #f]))

; Return an association list from interface names to class ASTs.
(define (associate-classes grammar)
  (map (λ (group) (cons (ag-class-interface (first group)) group))
       (group-by ag-class-interface (ag-grammar-classes grammar) eq?)))

#lang s-exp rosette

(provide ftl-ast-conflicts?
         ftl-ast-partition
         ftl-ast-inline
         ftl-ast-expr-depends
         ftl-ast-body-merge
         (struct-out ftl-ast-interface)
         (struct-out ftl-ast-trait)
         (struct-out ftl-ast-class)
         (struct-out ftl-ast-body)
         (struct-out ftl-ast-child)
         (struct-out ftl-ast-declare)
         (struct-out ftl-ast-define)
         (struct-out ftl-ast-refer)
         (struct-out ftl-ast-loop)
         (struct-out ftl-ast-expr-fold)
         (struct-out ftl-ast-expr-call)
         (struct-out ftl-ast-expr-unary)
         (struct-out ftl-ast-expr-binary)
         (struct-out ftl-ast-expr-cond))

; Note that identifiers are symbols

; interface definition
(struct ftl-ast-interface
  (; identifier corresponding to nonterminal in the attribute grammar
   name
   ; list of attribute declarations
   fields
   ) #:transparent)

; trait (mixin class) definition
(struct ftl-ast-trait
  (; identifier
   name
   ; class body
   body
   ) #:transparent)

; class definition
(struct ftl-ast-class
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
(struct ftl-ast-body
  (; list of child declarations
   children
   ; list of attribute declarations
   attributes
   ; list of attribute definitions, possibly inside of loops
   actions
   ) #:transparent)

; child declaration
(struct ftl-ast-child
  (; child (more generally, object) identifier (cannot be 'self)
   name
   ; whether this is a child sequence
   sequence
   ; the nonterminal that this child's subtree(s) generates
   interface
   ) #:transparent)

; attribute declaration
(struct ftl-ast-declare
  (; whether this attribute's value is given (#t) or computed (#f)
   input
   ; attribute identifier, also known as a label
   name
   ; the identifier for the attribute value's type, interpreted by a specific
   ; runtime/backend
   type
   ) #:transparent)

; attribute definition
(struct ftl-ast-define
  (; attribute reference (index will always be 'none)
   lhs
   ; expression, made up of function calls, conditionals, unary/binary
   ; operations, and zero-to-one top-level fold
   rhs
   ) #:transparent)

; attribute reference
(struct ftl-ast-refer
  (; object identifier, i.e., a child identifier or 'self 
   object
   ; 'previous, 'current, 'last, or 'none
   index
   ; attribute identifier
   label
   ) #:transparent)

; loop body
(struct ftl-ast-loop
  (; identifier of a sequence child
   iterate
   ; list of attribute definitions
   actions
   ) #:transparent)

(struct ftl-ast-expr-fold (init step) #:transparent)
(struct ftl-ast-expr-call (fun args) #:transparent)
(struct ftl-ast-expr-unary (operator operand) #:transparent)
(struct ftl-ast-expr-binary (left operator right) #:transparent)
(struct ftl-ast-expr-cond (if then else) #:transparent)

(define (ftl-ast-body-merge . xs)
  (ftl-ast-body (apply append (map ftl-ast-body-children xs))
            (apply append (map ftl-ast-body-attributes xs))
            (apply append (map ftl-ast-body-actions xs))))

; collect all dependencies (attribute references) of the given expression
(define (ftl-ast-expr-depends expr)
  (define (recurse expr base)
    (match expr
      [(ftl-ast-expr-unary op subexpr) (recurse subexpr base)]
      [(ftl-ast-expr-binary left op right) (recurse right (recurse left base))]
      [(ftl-ast-expr-call fun args) (foldl recurse base args)]
      [(ftl-ast-expr-cond cond then else) (foldl recurse base (list cond then else))]
      [(ftl-ast-expr-fold init step) (recurse step (recurse init base))]
      [(? ftl-ast-refer?) (if (not (member expr base))
                              (cons expr base)
                              base)]
      [else base]))
  (recurse expr null))

; check for conflicts in the top-level namespace of interfaces, traits, and classes
(define (ftl-ast-conflicts? ast-list)
  (let ([pr (λ (ast)
              (match ast
                [(ftl-ast-interface name _) name]
                [(ftl-ast-trait name _) name]
                [(ftl-ast-class name _ _ _) name]))])
    (pr (check-duplicates ast-list eq? #:key pr))))

; partition the list of top-level ASTs into [a list of] three lists of ASTs:
; interfaces, traits, and classes
(define (ftl-ast-partition ast-list)
  (let*-values ([(trait-list other-list) (partition ftl-ast-trait? ast-list)]
                [(class-list iface-list) (partition ftl-ast-class? other-list)])
    (list iface-list trait-list class-list)))

; inline all inherited traits and implemented interfaces into class bodies and
; produce a hash(eq) from symbols (interface names) to a list such that the head
; is the interface AST rooted with a body node and the rest are pairs of options
; (class names) and inlined ASTs (also rooted with a body node)
(define (ftl-ast-inline ast-list)
  (define (undefined kind name)
    (thunk (raise-arguments-error 'inline (string-append "undefined " kind)
                                  "occurrence" (symbol->string name))))
  (let* ([parts (ftl-ast-partition ast-list)]
         [iface-list (list-ref parts 0)]
         [trait-list (list-ref parts 1)]
         [class-list (list-ref parts 2)]
         [pair-trait (λ (trait)
                       (cons (ftl-ast-trait-name trait)
                             (ftl-ast-trait-body trait)))]
         [pair-iface (λ (iface)
                       (cons (ftl-ast-interface-name iface)
                             (ftl-ast-body null
                                           (ftl-ast-interface-fields iface)
                                           null)))]
         [trait-hash (make-immutable-hasheq
                      (map pair-trait trait-list))]
         [iface-hash (make-immutable-hasheq
                      (map pair-iface iface-list))]
         [inline (λ (class)
                   (let* ([iface-name (ftl-ast-class-interface class)]
                          [iface-body (hash-ref iface-hash
                                                iface-name
                                                (undefined "interface"
                                                           iface-name))]
                          [class-name (ftl-ast-class-name class)]
                          [class-body (ftl-ast-class-body class)]
                          [trait-names (ftl-ast-class-traits class)]
                          [trait-bodies (map (λ (trait-name)
                                               (hash-ref trait-hash
                                                         trait-name
                                                         (undefined "trait"
                                                                    trait-name)))
                                             trait-names)]
                          [bodies (list* class-body iface-body trait-bodies)])
                     (cons iface-name
                           (cons class-name
                                 (apply ftl-ast-body-merge bodies)))))]
         [inlined (map inline class-list)]
         [grouped (group-by car inlined eq?)] ; requires Racket v6.3+
         ; to transform each grouping into an association, pull a copy of the
         ; symbol (interface name) out front, cons with a list of the interface
         ; "body" followed by the inlined class bodies of that interface in no
         ; particular order.
         [assoced (map (λ (group)
                           (list* (caar group)
                                  (hash-ref iface-hash (caar group))
                                  (map cdr group)))
                       grouped)])
    assoced))
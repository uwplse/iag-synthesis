#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Grammar DSL Abstract Syntax Tree

(require "utility.rkt")

(provide (struct-out ftl-ast-interface)
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
         (struct-out ftl-ast-expr-cond)
         ftl-ast-refer->pair
         ftl-ast-expr-depends
         ftl-ast-body-merge
         ftl-ast-conflicts?
         ftl-ast-partition
         ftl-ast-inline
         ftl-ast-validate
         ftl-ast-map-class
         ftl-ast-for-each-class)

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

(define (ftl-ast-refer->pair ref)
  (cons (ftl-ast-refer-object ref)
        (ftl-ast-refer-label ref)))

(define (ftl-ast-body-merge . xs)
  (ftl-ast-body (apply append (map ftl-ast-body-children xs))
            (apply append (map ftl-ast-body-attributes xs))
            (apply append (map ftl-ast-body-actions xs))))

; Collect all dependencies (attribute references) of the given expression
(define (ftl-ast-expr-depends expr)
  (define (recurse expr base)
    (match expr
      [(ftl-ast-expr-unary op subexpr)
       (recurse subexpr base)]
      [(ftl-ast-expr-binary left op right)
       (recurse right (recurse left base))]
      [(ftl-ast-expr-call fun args)
       (foldl recurse base args)]
      [(ftl-ast-expr-cond cond then else)
       (foldl recurse base (list cond then else))]
      [(ftl-ast-expr-fold init step)
       (recurse step (recurse init base))]
      [(? ftl-ast-refer?)
       (if (not (member expr base))
           (cons expr base)
           base)]
      [else
       base]))
  (recurse expr null))

; Check for conflicts in the top-level namespace of interfaces, traits, and
; classes.
(define (ftl-ast-conflicts? ast-list)
  (define (pr ast)
    (match ast
      [(ftl-ast-interface name _) name]
      [(ftl-ast-trait name _) name]
      [(ftl-ast-class name _ _ _) name]))
  (let ([dup (check-duplicates ast-list eq? #:key pr)])
    (if dup (pr dup) (void))))

; Partition the list of top-level ASTs into [a list of] three lists of ASTs:
; interfaces, traits, and classes.
(define (ftl-ast-partition ast-list)
  (let*-values ([(trait-list other-list)
                 (partition ftl-ast-trait? ast-list)]
                [(class-list iface-list)
                 (partition ftl-ast-class? other-list)])
    (list iface-list trait-list class-list)))

; Given a (conflict-free) list of top-level AST nodes, produce an list
; associating each symbol of the grammar's implicit alphabet (i.e., interface
; names) with the interface as a body AST paired with another list associating each
; production option (i.e., class name) with its inlined body AST. The output of
; this function is often referred to as an AST vocabulary or an AST map.
(define (ftl-ast-inline ast-list)
  ; some local helper functions
  (define (undefined kind name)
    (thunk (raise-arguments-error 'inline (string-append "undefined " kind)
                                  "occurrence" (symbol->string name))))
  (define (pair-trait trait)
    (cons (ftl-ast-trait-name trait)
          (ftl-ast-trait-body trait)))
  (define (pair-iface iface)
    (cons (ftl-ast-interface-name iface)
          (ftl-ast-body null
                        (ftl-ast-interface-fields iface)
                        null)))
  ; partition the list of AST top-level forms and create top-level static
  ; environments (i.e., identifier mappings)
  (match-let* ([(list iface-list trait-list class-list)
                (ftl-ast-partition ast-list)]
               [trait-map (map pair-trait trait-list)]
               [iface-map (map pair-iface iface-list)])
    (define (inline class)
      (let* ([iface-name (ftl-ast-class-interface class)]
             [iface-body (assoc-lookup iface-map
                                       iface-name
                                       (undefined "interface"
                                                  iface-name))]
             [class-name (ftl-ast-class-name class)]
             [class-body (ftl-ast-class-body class)]
             [trait-names (ftl-ast-class-traits class)]
             [trait-bodies (map (λ (trait-name)
                                  (assoc-lookup trait-map
                                                trait-name
                                                (undefined "trait"
                                                           trait-name)))
                                trait-names)]
             [bodies (list* class-body iface-body trait-bodies)])
        (cons iface-name
              (cons class-name
                    (apply ftl-ast-body-merge bodies)))))

    ; in three stages, transform the list of groupings into a list associating
    ; each interface name to a list of inlined class bodies (that implement said
    ; interface)
    (let* ([inlined (map inline class-list)]
           [grouped (group-by car inlined eq?)] ; requires Racket v6.3+
           [assoced (map (λ (group)
                           (cons (caar group)
                                 (map cdr group)))
                         grouped)])

      ; generate a new version of assoced that includes unimplemented interfaces
      (for/list ([iface-mapping iface-map])
        (match-let ([(cons symbol iface) iface-mapping])
          (list* symbol
                 iface
                 (assoc-lookup assoced symbol null)))))))

(define (ftl-ast-for-each-class proc ast-map)
  (for/list ([ast-mapping ast-map])
    (let ([classes (cddr ast-mapping)])
      (cdrmap proc classes))))

(define (ftl-ast-map-class proc ast-map)
  (for/list ([ast-mapping ast-map])
    (match-let ([(list-rest symbol iface classes) ast-mapping])
      (list* symbol
             iface
             (cdrmap proc classes)))))

; Check for certain semantic errors in an inlined AST map:
;  * duplicate declarations among a class, its trait(s), and its interface
;  * duplicate definitions among a class and its trait(s)
;  * definitions of undeclared traits among a class and its trait(s)
;  * the existence of traits inherited by a class
; Additionally, "deloopify" the actions of all body ASTs, by wrapping each
; attribute definition in its own AST loop node if applicable.
(define (ftl-ast-validate ast-map)
  ; define so many helper functions
  (define (deloopify act)
    (if (ftl-ast-loop? act)
        (ftl-ast-loop-actions act)
        act))
  (define (check-duplicate-fields access identify equal has-duplicate)
    (ftl-ast-for-each-class
     (λ (class)
       ; check-duplicates requires Racket v6.3+
       (let ([duplicate (check-duplicates (access class)
                                          equal
                                          #:key identify)])
         (when duplicate
           (has-duplicate class duplicate))))
     ast-map))
  (define (has-duplicate part class name)
    (raise-arguments-error
     'ftl-ast-validate
     (string-append "duplicate " part)
     part (if (symbol? name) (symbol->string name) name)
     "component of class" class))
  (define (has-duplicate-define class def)
    (match-let ([(ftl-ast-refer object _ label)
                 (ftl-ast-define-lhs def)])
    (has-duplicate "definition of attribute"
                   (ftl-ast-class-name class)
                   (string-append (symbol->string object)
                                  "."
                                  (symbol->string label)))))
  (define (has-duplicate-declare class decl)
    (has-duplicate "declaration of attribute"
                   (ftl-ast-class-name class)
                   (ftl-ast-declare-name decl)))
  (define (has-duplicate-child class child)
    (has-duplicate "declaration of child"
                   (ftl-ast-class-name class)
                   (ftl-ast-child-name child)))

  ; check that no attribute is defined more than once
  (check-duplicate-fields (compose flatten
                                   (curry map deloopify)
                                   ftl-ast-body-actions)
                          (compose ftl-ast-refer->pair
                                   ftl-ast-define-lhs)
                          equal?
                          has-duplicate-define)
  ; check that the same label was not used for multiple attributes
  (check-duplicate-fields ftl-ast-body-attributes
                          ftl-ast-declare-name
                          eq?
                          has-duplicate-declare)
  ; check that no child name was declared more than once
  (check-duplicate-fields ftl-ast-body-children
                          ftl-ast-child-name
                          eq?
                          has-duplicate-child)
  ast-map)

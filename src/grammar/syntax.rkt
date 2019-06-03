#lang rosette

; Abstract syntax for language of attribute grammars

(require "../utility.rkt")

(provide (all-defined-out))

; attribute grammar
(struct ag-grammar
  (interfaces ; (--> name (--> label (| `(in ,type) `(out ,type)))
   classes ; (--> name ag-class)
   traits ; (--> name ag-body)
   traversals ; (--> name traversal)
   ) #:transparent)

; class/trait body
(struct ag-body
  (children ; (--> name (| `(unit ,interface) `(star ,interface) `(plus ,interface)))
   attributes ; (--> name (| `(in ,type) `(out ,type)))
   statements ; (--> reference (| expression `(fold ,expression ,expression)))
   methods ; (--> name `(,(* reference) . ,(* reference)))
   ) #:transparent)

; class declaration
(struct ag-class
  (interface ; interface
   traits ; (* traits)
   body ; ag-body
    ) #:transparent)

; ------------------------------
; Utilities for identifier paths
; ------------------------------

;(define (path->string path)
;  (string-join (map symbol->string path) "."))
;
;(define (path->symbol path)
;  (compose string->symbol path->string))
;
;; Does the second path extend/refine the first path?
;(define (extends? shorter longer)
;  (list-prefix? shorter longer eq?))
;
;; Return r s.t. p = q ++ r or #f if no such r exists
;(define/match (shift p q)
;  [((cons l1 p) (cons l2 q)) #:when (equal? l1 l2) (shift p q)]
;  [(r null) r]
;  [(_ _) #f])
;
;; Return r s.t. p = q ++ r or q = p ++ r or #f if no such r exists
;(define/match (trail p q)
;  [((cons l1 p) (cons l2 q)) #:when (equal? l1 l2) (shift p q)]
;  [(r null) r]
;  [(null r) r]
;  [(_ _) #f])

; ------------------------------------
; Utilities for attribute declarations
; ------------------------------------

(define-match-expander ag:sort
  (syntax-rules ()
    [(ag:sort mode type)
     `(,(and mode (or 'in 'out)) ,(symbol type))]))

(define-match-expander ag:input
  (syntax-rules ()
    [(ag:input type) (ag:sort 'in type)]))

(define-match-expander ag:output
  (syntax-rules ()
    [(ag:output type) (ag:sort 'out type)]))

(define/match (ag:input? v)
  [((ag:sort 'in _)) #t]
  [(_) #f])

(define/match (ag:output? v)
  [((ag:sort 'out _)) #t]
  [(_) #f])

; ----------------------------------
; Utilities for attribute references
; ----------------------------------

(define-match-expander ag:reference
  (syntax-rules ()
    [(ag:reference object label) (cons object (symbol label))]))

(define-match-expander ag:object
  (syntax-rules ()
    [(ag:object node index)
     `(,(and index (or 'unit 'first 'pred 'curr 'succ 'last)) ,(symbol node))]))

(define-match-expander ag:object1
  (syntax-rules ()
    [(ag:object1 node) (ag:object node 'unit)]))

(define-match-expander ag:object$0
  (syntax-rules ()
    [(ag:object$0 node) (ag:object node 'first)]))

(define-match-expander ag:object$-
  (syntax-rules ()
    [(ag:object$- node) (ag:object node 'pred)]))

(define-match-expander ag:object$i
  (syntax-rules ()
    [(ag:object$i node) (ag:object node 'curr)]))

(define-match-expander ag:object$+
  (syntax-rules ()
    [(ag:object$+ node) (ag:object node 'succ)]))

(define-match-expander ag:object$$
  (syntax-rules ()
    [(ag:object$$ node) (ag:object node 'last)]))

(define-match-expander ag:object$_
  (syntax-rules ()
    [(ag:object$_ node) (ag:object node _)]))

(define/match (ag:object-iterated? v)
  [((ag:object$i _)) #t]
  [((ag:object$- _)) #t]
  [((ag:object$+ _)) #t]
  [(_) #f])

; Whether the first object has a weaker iteration order than the second.
(define/match (ag:object-iterated<=? lower upper)
  [((ag:object$i node1) (ag:object$i node2)) (eq? node1 node2)]
  [((ag:object$i node1) (ag:object$+ node2)) (eq? node1 node2)]
  [((ag:object$0 node1) (ag:object$+ node2)) (eq? node1 node2)]
  [((ag:object$+ node1) (ag:object$+ node2)) (eq? node1 node2)]
  [((ag:object$i node1) (ag:object$- node2)) (eq? node1 node2)]
  [((ag:object$$ node1) (ag:object$- node2)) (eq? node1 node2)]
  [((ag:object$- node1) (ag:object$- node2)) (eq? node1 node2)]
  [(#f #f) #t]
  [(_ _) #f])

; Whether the two objects refer to a disjoint set of nodes.
(define/match (ag:object-disjoint? object1 object2)
  [((ag:object$0 _) (ag:object$+ _)) #t]
  [((ag:object$+ _) (ag:object$0 _)) #t]
  [((ag:object$$ _) (ag:object$- _)) #t]
  [((ag:object$- _) (ag:object$$ _)) #t]
  [((ag:object$_ node1) (ag:object$_ node2)) (not (eq? node1 node2))])

; Whether the two references refer to a disjoint set of attributes.
(define/match (ag:reference-disjoint? reference1 reference2)
  [((ag:reference object1 label1) (ag:reference object2 label2))
   (or (not (eq? label1 label2))
       (ag:object-disjoint? object1 object2))])

; Inverse of `ag:reference-disjoint?`
(define ag:reference-overlap? (negate ag:reference-disjoint?))

(define/match (reference->string reference)
  [((ag:reference object label))
   (format "~a.~a" (object->string object) label)])

(define/match (object->string object)
  [((ag:object1 node)) (format "~a" node)]
  [((ag:object$0 node)) (format "~a$0" node)]
  [((ag:object$- node)) (format "~a$-" node)]
  [((ag:object$i node)) (format "~a$i" node)]
  [((ag:object$+ node)) (format "~a$+" node)]
  [((ag:object$$ node)) (format "~a$$" node)])

; --------------------------------
; Utilities for child declarations
; --------------------------------

(define-match-expander ag:child
  (syntax-rules ()
    [(ag:child mode kind)
     `(,(and mode (or 'unit 'star 'plus)) ,(symbol kind))]))

(define-match-expander ag:child1
  (syntax-rules ()
    [(ag:child1 kind) (ag:child 'unit kind)]))

(define-match-expander ag:child*
  (syntax-rules ()
    [(ag:child* kind) (ag:child 'star kind)]))

(define-match-expander ag:child+
  (syntax-rules ()
    [(ag:child+ kind) (ag:child 'plus kind)]))

(define-match-expander ag:child_
  (syntax-rules ()
    [(ag:child_ kind) (ag:child _ kind)]))

(define/match (ag:child-mode v)
  [((ag:child mode _)) mode])

(define/match (ag:child-kind v)
  [((ag:child _ kind)) kind])

(define (ag:child1? v)
  (eq? (ag:child-mode v) 'unit))

(define (ag:child*? v)
  (eq? (ag:child-mode v) 'star))

(define (ag:child+? v)
  (eq? (ag:child-mode v) 'plus))

; ---------------------------------
; Utilities for method declarations
; ---------------------------------

(define-match-expander method
  (syntax-rules ()
    [(method name inflow outflow)
     (cons (symbol name) (cons inflow outflow))]))

(define/match (method-name v)
  [((method name _ _)) name])

(define/match (method-inflow v)
  [((method _ inflow _)) inflow])

(define/match (method-outflow v)
  [((method _ _ outflow)) outflow])

; -----------------------------
; Utilities for statement rules
; -----------------------------

(define-match-expander rule
  (syntax-rules ()
    [(rule object label define) `((,object . ,label) . ,define)]))

(define/match (rule-object v)
  [((rule object _ _)) object])

(define/match (rule-label v)
  [((rule _ label _)) label])

(define/match (rule-define v)
  [((rule _ _ define)) define])

(define/match (rule-iterates v)
  [((rule (and object (ag:object$i _)) _ _)) object]
  [((rule (and object (ag:object$0 _)) _ _)) object]
  [((rule (and object (ag:object$+ _)) _ _)) object]
  [((rule (and object (ag:object$$ _)) _ _)) object]
  [((rule (and object (ag:object$- _)) _ _)) object]
  [((rule (ag:object1 _) _ `(foldl ,_ ,expr)))
   (define/match (search expr)
     [((? number?)) #f]
     [((ag:reference (ag:object$i node) _)) `(succ ,node)]
     [((list (symbol op) exprs ...)) (ormap search exprs)])
   (search expr)]
  [((rule (ag:object1 _) _ `(foldr ,_ ,expr)))
   (define/match (search expr)
     [((? number?)) #f]
     [((ag:reference (ag:object$i node) _)) `(pred ,node)]
     [((list (symbol op) exprs ...)) (ormap search exprs)])
   (search expr)]
  [((rule _ _ _)) #f])

; List all attribute references mentioned in the expression.
(define (expression-context expr)
  (define/match (recur expr acc)
    [((or #t #f (? number?)) _)
     acc]
    [((and ref (ag:reference _ _)) _)
     (cons ref acc)]
    [((list (symbol op) exprs ...) _)
     (foldl recur acc exprs)])
  (recur expr null))

; --------------------------------
; Utilities for class/trait bodies
; --------------------------------

(define/match (ag-body-union body1 body2)
  [((ag-body children1 attributes1 statements1 methods1)
    (ag-body children2 attributes2 statements2 methods2))
   (ag-body (append children1 children2)
            (append attributes1 attributes2)
            (append statements1 statements2)
            (append methods1 methods2))])

(define/match (ag-body-inherit body interface)
  [((ag-body children attributes statements methods) _)
   (ag-body children (append interface attributes) statements methods)])

(define ag-class-children (compose ag-body-children ag-class-body))
(define ag-class-attributes (compose ag-body-attributes ag-class-body))
(define ag-class-statements (compose ag-body-statements ag-class-body))
(define ag-class-methods (compose ag-body-methods ag-class-body))

; --------------------------------
; Utilities for class declarations
; --------------------------------

; Get expression for initial virtual accumulator.
(define (class-rule-init class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) `(foldl ,init ,_)) init]
           [(rule (ag:object1 (== node)) (== label) `(foldr ,init ,_)) init]
           [_ #f])
         (ag-class-statements class-body)))

; Get expression for basis node in iteration sequence.
(define (class-rule-base class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) `(foldl ,_ ,step)) step]
           [(rule (ag:object1 (== node)) (== label) `(foldr ,_ ,step)) step]
           [(rule (ag:object$0 (== node)) (== label) expr) expr]
           [(rule (ag:object$$ (== node)) (== label) expr) expr]
           [(rule (ag:object$i (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-statements class-body)))

; Get expression for inductive nodes in iteration sequence.
(define (class-rule-step class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) `(foldl ,_ ,step)) step]
           [(rule (ag:object1 (== node)) (== label) `(foldr ,_ ,step)) step]
           [(rule (ag:object$+ (== node)) (== label) expr) expr]
           [(rule (ag:object$- (== node)) (== label) expr) expr]
           [(rule (ag:object$i (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-statements class-body)))

; Get expression for base/inductive nodes in iteration sequence.
(define (class-rule-iter class-body node label base?)
  (if base?
      (class-rule-base class-body node label)
      (class-rule-step class-body node label)))

; Get expression for a unit node outside all iteration.
(define (class-rule-unit class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-statements class-body)))

; Get method declaration given its name.
(define (class-method class-body method-name)
  (assoc method-name (ag-class-methods class-body)))

; Return an association list from interface names to class ASTs.
(define (associate-classes grammar)
  (map (λ (group)
         (cons (ag-class-interface (cdar group)) group))
       (group-by (compose ag-class-interface cdr)
                 (ag-grammar-classes grammar)
                 eq?)))

; ------------------------------
; Utilities for complete grammar
; ------------------------------

(define/match (grammar-add-interface grammar interface)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar (cons interface interfaces) classes traits traversals)])

(define/match (grammar-add-class grammar class)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar interfaces (cons class classes) traits traversals)])

(define/match (grammar-add-trait grammar trait)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar interfaces classes (cons trait traits) traversals)])

(define/match (grammar-add-traversal grammar traversal)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar interfaces classes traits (cons traversal traversals))])

; ---------------------
; Utilities for look-up
; ---------------------

(define (grammar-traversal G name)
  (define traversal (lookup (ag-grammar-traversals G) name))
  (if traversal
      `(trav ,name ,traversal)
      (raise-user-error 'grammar-traversal "Undefined traversal '~a'" name)))

(define (grammar-interface G name)
  (define interface (lookup (ag-grammar-interfaces G) name))
  (if interface
      interface
      (raise-user-error 'grammar-interface "Undefined interface '~a'" name)))

(define (grammar-class G name)
  (define class (lookup (ag-grammar-classes G) name))
  (if class
      class
      (raise-user-error 'grammar-class "Undefined class '~a'" name)))

(define (grammar-trait G name)
  (define trait (lookup (ag-grammar-traits G) name))
  (if trait
      trait
      (raise-user-error 'grammar-trait "Undefined trait '~a'" name)))

; -----------------------------------
; Validation of the attribute grammar
; -----------------------------------

; Report an error to the user if the given symbol table, an association list
; with symbol keys, is ambiguous (i.e., has duplicate entries for a symbol).
(define (validate-symbol-table symbol-table kind scope)
  (define duplicate (check-duplicates symbol-table eq? #:key car))
  (when duplicate
    (raise-user-error 'validation
                      "Duplicate ~a declarations for '~a' in ~a"
                      kind (car duplicate) scope)))

; TODO: Document.
(define (validate-reference G class-decl reference #:fold? [fold? #f])
  (match-define (cons class-name (ag-class _ _ class-body)) class-decl)
  (match-define (ag-body class-children class-attributes _ _) class-body)
  (match-define (cons object label) reference)

  (define (reject! hint)
    (raise-user-error 'validation
                      "Invalid attribute reference ~a in class ~a (~a)"
                      (reference->string reference) class-name hint))

  (define (get-child name)
    (let ([child (lookup class-children name)])
      (unless child
        (reject! "no such child"))
      child))

  (define (get-type interface)
    (let ([attribute (lookup interface label)])
      (unless attribute
        (reject! "no such label"))
      attribute))

  (match object
    [(ag:object1 'self)
     (get-type class-attributes)]
    [(or (ag:object$- 'self) (ag:object$+ 'self))
     (unless fold?
       (reject! "accumulator outside fold"))
     (get-type class-attributes)]
    [(ag:object1 child)
     (match (get-child child)
       [(ag:child1 kind)
        (get-type (grammar-interface G kind))]
       [(or (ag:child* _) (ag:child+ _))
        (reject! "unindexed child sequence")])]
    [(or (ag:object$- child) (ag:object$+ child))
     (match (get-child child)
       [(ag:child1 kind)
        (unless fold?
          (reject! "accumulator outside fold"))
        (get-type (grammar-interface G kind))]
       [(or (ag:child* kind) (ag:child+ kind))
        (get-type (grammar-interface G kind))])]
    [(ag:object$i child)
     (match (get-child child)
       [(ag:child1 _)
        (reject! "singular child node")]
       [(or (ag:child* kind) (ag:child+ kind))
        (get-type (grammar-interface G kind))])]
    [(or (ag:object$0 child) (ag:object$$ child))
     (match (get-child child)
       [(ag:child1 _)
        (reject! "singular child node")]
       [(ag:child* _)
        (reject! "potentially empty child sequence")]
       [(ag:child+ kind)
        (get-type (grammar-interface G kind))])]))

; TODO: Document.
(define/match (validate-statement G class-decl statement)
  [(_ _ (cons output (list (or 'foldl 'foldr) seed-expr step-expr)))
   (validate-reference G class-decl output #:fold? #t)
   (for ([input (expression-context seed-expr)])
     (validate-reference G class-decl input))
   (for ([input (expression-context step-expr)])
     (validate-reference G class-decl input #:fold? #t))]
  [(_ _ (cons output expr))
   (validate-reference G class-decl output)
   (for ([input (expression-context expr)])
     (validate-reference G class-decl input))])

; TODO: Document.
(define/match (validate-child G class-name child-decl)
  [(_ _ (cons name (ag:child_ kind)))
   (unless (lookup (ag-grammar-interfaces G) kind)
     (raise-user-error 'validation
                       "Undefined interface '~a' for child ~a in class ~a"
                       kind name class-name))])

; TODO: Document.
(define/match (validate-class G class-decl)
  [(_ (cons name (ag-class _ traits
                           (ag-body children attributes statements methods))))
   (define scope (format "class ~a" name))

   (match (check-duplicates traits eq?)
     [(symbol duplicate-trait)
      (raise-user-error 'validation "Redundant inclusion of trait ~a in ~a"
                        duplicate-trait scope)]
     [#f (void)])

   (validate-symbol-table children "child" scope)
   (for-each (curry validate-child G name) children)

   (validate-symbol-table attributes "attribute" scope)

   (validate-symbol-table methods "method" scope)

   (match (check-duplicates statements ag:reference-overlap? #:key car)
     [(rule (ag:object$_ node) label _)
      (raise-user-error 'validation
                        "Conflicting statement rules for attribute ~a.~a in ~a"
                        node label scope)]
     [#f (void)])
   (for-each (curry validate-statement G class-decl) statements)])

; Validate uniqueness of global names in the attribute grammar.
(define/match (validate-grammar G)
  [((ag-grammar interfaces classes traits traversals))
   (define scope "grammar")
   (validate-symbol-table interfaces "interface" scope)
   (validate-symbol-table classes "class" scope)
   (validate-symbol-table traits "trait" scope)
   (validate-symbol-table traversals "traversal" scope)
   (for-each (curry validate-class G) classes)])

; ------------------------------------
; Elaboration of the attribute grammar
; ------------------------------------

; Inline each trait mixin and attributes inherited from the interface.
(define/match (elaborate-class G class-decl)
  [(_ (cons name (ag-class iface-name trait-names class-body)))
   (define body
     (foldl ag-body-union
            (ag-body-inherit class-body (grammar-interface G iface-name))
            (map (curry grammar-trait G) trait-names)))
   (cons name (ag-class iface-name trait-names body))])

; Inline all traits and inherited attributes.
(define/match (elaborate-grammar G)
  [((ag-grammar interfaces classes traits traversals))
   (ag-grammar interfaces
               (map (curry elaborate-class G) classes)
               traits
               traversals)])

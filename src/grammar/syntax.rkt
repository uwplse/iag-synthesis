#lang rosette

; Abstract syntax for language of attribute grammars

(require "../utility.rkt")

(provide (all-defined-out))

; attribute grammar
(struct ag-grammar
  (interfaces ; (--> iface-name (--> label (| `(in ,type) `(out ,type)))
   classes ; (--> class-name class-body)
   traits
   traversals ; (--> trav-name trav-decl)
   ) #:transparent)

; class/trait body
(struct ag-body
  (children ; (--> child-name (| `(unit ,iface-name) `(star ,iface-name) `(plus ,iface-name)))
   attributes ; (--> label (| `(in ,type) `(out ,type)))
   statements ; (--> var (| expr `(fold ,expr ,expr)))
   methods ; (--> method-name `(,(* var) . ,(* var)))
   ) #:transparent)

; class declaration
(struct ag-class
  (interface ; iface-name
   traits ; (list trait-name)
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

; TODO: Is this actually used anywhere?
(define (uniquely-associated? lst [same? equal?])
  (check-duplicates lst same? #:key car))

; ---------------------
; Utilities for look-up
; ---------------------

(define (grammar-traversal G trav-name)
  `(trav ,trav-name ,(lookup (ag-grammar-traversals G) trav-name eq?)))

(define (grammar-interface G iface-name)
  (lookup (ag-grammar-interfaces G) iface-name eq?))

(define (grammar-class G class-name)
  (lookup (ag-grammar-classes G) class-name eq?))

(define (grammar-trait G trait-name)
  (lookup (ag-grammar-traits G) trait-name eq?))

; ------------------------------------
; Utilities for attribute declarations
; ------------------------------------

(define-match-expander ag:sort
  (syntax-rules ()
    [(ag:sort mode type) `(,(and mode (or 'in 'out)) ,(symbol type))]))

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
    [(ag:object node index) `(,index ,(symbol node))]))

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

(define/match (ag:object-iterated? v)
  [((ag:object$i _)) #t]
  [((ag:object$- _)) #t]
  [((ag:object$+ _)) #t]
  [(_) #f])

; FIXME: Is this to blame for spurious iteration failures?
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

; --------------------------------
; Utilities for child declarations
; --------------------------------

(define-match-expander child
  (syntax-rules ()
    [(child mode name kind) `(,(symbol name) . (,mode ,(symbol kind)))]))

(define-match-expander child1
  (syntax-rules ()
    [(child1 name kind) (child 'unit name kind)]))

(define-match-expander child*
  (syntax-rules ()
    [(child* name kind) (child 'star name kind)]))

(define-match-expander child+
  (syntax-rules ()
    [(child+ name kind) (child 'plus name kind)]))

(define/match (child-name v)
  [((child _ name _)) name])

(define/match (child-mode v)
  [((child mode _ _)) mode])

(define/match (child-kind v)
  [((child _ _ kind)) kind])

(define/match (child? v)
  [((child _ _ _)) #t]
  [(_) #f])

(define (child1? v)
  (eq? (child-mode v) 'unit))

(define (child*? v)
  (eq? (child-mode v) 'star))

(define (child+? v)
  (eq? (child-mode v) 'plus))

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

(define-match-expander fold
  (syntax-rules ()
    [(fold init step) (or `(foldl ,init ,step) `(foldr ,init ,step))]))

(define/match (rule-iterates v)
  [((rule (and object (ag:object$i _)) _ _)) object]
  [((rule (and object (ag:object$0 _)) _ _)) object]
  [((rule (and object (ag:object$+ _)) _ _)) object]
  [((rule (and object (ag:object$$ _)) _ _)) object]
  [((rule (and object (ag:object$- _)) _ _)) object]
  [((rule (ag:object1 _) _ (fold _ expr)))
   (define/match (search expr)
     [((? number?)) #f]
     [((ag:reference (? ag:object-iterated? object) _)) object]
     [(`(call ,_ ,arg-exprs))
      (ormap search arg-exprs)]
     [(`(ite ,cond-expr ,then-expr ,else-expr))
      (or (search cond-expr) (search then-expr) (search else-expr))]
     [(`(! ,expr))
      (search expr)]
     [(`(,_ ,left-expr ,right-expr))
      (or (search left-expr) (search right-expr))])
   (search expr)]
  [((rule _ _ _)) #f])

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

(define (labels-for-node G class-name)
  #f)

(define (children-for-node G class-name)
  #f)

(define (statements-for-node G class-name)
  #f)

(define (class-attributes G class-name)
  (let ([class-body (grammar-class G class-name)])
    (append (ag-class-attributes class-body)
            (grammar-interface G (ag-class-interface class-body)))))

; Get expression for initial virtual accumulator.
(define (class-rule-init class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) (fold init _)) init]
           [_ #f])
         (ag-class-statements class-body)))

; Get expression for basis node in iteration sequence.
(define (class-rule-base class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) (fold _ step)) step]
           [(rule (ag:object$0 (== node)) (== label) expr) expr]
           [(rule (ag:object$$ (== node)) (== label) expr) expr]
           [(rule (ag:object$i (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-statements class-body)))

; Get expression for inductive nodes in iteration sequence.
(define (class-rule-step class-body node label)
  (ormap (match-lambda
           [(rule (ag:object1 (== node)) (== label) (fold _ step)) step]
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

; TODO: Document.
(define (class-duplicate-traits class-body)
  #f)

; TODO: Document.
(define (class-duplicate-children class-body)
  #f)

; TODO: Document.
(define (class-duplicate-attributes class-body)
  #f)

; TODO: Document.
(define (class-duplicate-statements class-body)
  #f)

; TODO: Document.
(define (class-duplicate-methods class-body)
  #f)

(define/match (validate-class G class-decl)
  [(_ (cons name (ag-class iface-name trait-names class-body)))
   #t])

(define/match (elaborate-class G class-decl)
  [(_ (cons name (ag-class iface-name trait-names class-body)))
   (define body
     (foldl ag-body-union
            (ag-body-inherit class-body (grammar-interface G iface-name))
            (map (curry grammar-trait G) trait-names)))
   (cons name (ag-class iface-name trait-names body))])

; ------------------------------
; Utilities for complete grammar
; ------------------------------

(define/match (ag-grammar-add-interface grammar interface)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar (cons interface interfaces) classes traits traversals)])

(define/match (ag-grammar-add-class grammar class)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar interfaces (cons class classes) traits traversals)])

(define/match (ag-grammar-add-trait grammar trait)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar interfaces classes (cons trait traits) traversals)])

(define/match (ag-grammar-add-traversal grammar traversal)
  [((ag-grammar interfaces classes traits traversals) _)
   (ag-grammar interfaces classes traits (cons traversal traversals))])

; Inline all traits and inherited attributes.
(define/match (elaborate-grammar G)
  [((ag-grammar interfaces classes traits traversals))
   (ag-grammar interfaces
               (map (curry elaborate-class G) classes)
               traits
               traversals)])

; ---------------------
; General functionality
; ---------------------

; TODO: Implement some sanity checks
(define (check-grammar grammmar)
  #t)

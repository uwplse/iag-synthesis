#lang rosette

; Abstract syntax for language of attribute grammars

(require "../utility.rkt")

(provide (all-defined-out))

; attribute grammar
(struct ag-grammar
   (interfaces ; (--> iface-name iface-body)
    classes ; (--> class-name class-body)
    traversals ; (--> trav-name trav-decl)
    ) #:transparent)

; traversal declaration
(struct ag-traversal
  (cases ; (--> class-name (* (| `(visit ,node) `(recur ,node) `(iterate ,node ,body))))
   ) #:transparent)

; interface declaration
(struct ag-interface
  (attributes ; (--> label (| `(in ,type) `(out ,type)))
   ) #:transparent)

; class declaration
(struct ag-class
  (interface ; iface-name
   children ; (--> child-name (| `(unit ,iface-name) `(star ,iface-name) `(plus ,iface-name)))
   attributes ; (--> label (| `(in ,type) `(out ,type)))
   methods ; (--> method-name `(,(* var) . ,(* var)))
   rules ; (--> var (| expr `(fold ,expr ,expr)))
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
  (lookup (ag-grammar-traversals G) trav-name eq?))

(define (grammar-interface G iface-name)
  (lookup (ag-grammar-interfaces G) iface-name eq?))

(define (grammar-class G class-name)
  (lookup (ag-grammar-classes G) class-name eq?))

; ------------------------------------
; Utilities for attribute declarations
; ------------------------------------

(define-match-expander attribute
  (syntax-rules ()
    [(attribute mode name type) `(,(symbol name) . (,mode ,(symbol type)))]))

(define-match-expander input
  (syntax-rules ()
    [(input name type) (attribute 'in name type)]))

(define-match-expander output
  (syntax-rules ()
    [(output name type) (attribute 'out name type)]))

(define/match (attribute-name v)
  [((attribute _ name _)) name])

(define/match (attribute-mode v)
  [((attribute mode _ _)) mode])

(define/match (attribute-type v)
  [((attribute _ _ type)) type])

(define/match (attribute? v)
  [((attribute _ _ _)) #t]
  [(_) #f])

(define (input? v)
  (eq? (attribute-mode v) 'in))

(define (output? v)
  (eq? (attribute-mode v) 'out))

; ----------------------------------
; Utilities for attribute references
; ----------------------------------

(define-match-expander reference
  (syntax-rules ()
    [(reference object label) (cons object (symbol label))]))

(define/match (reference-object v)
  [((reference object _)) object])

(define/match (reference-label v)
  [((reference _ label)) label])

(define-match-expander object
  (syntax-rules ()
    [(object node index) `(,index ,(symbol node))]))

(define-match-expander object1
  (syntax-rules ()
    [(object1 node) (object node 'unit)]))

(define-match-expander object$0
  (syntax-rules ()
    [(object$0 node) (object node 'first)]))

(define-match-expander object$-
  (syntax-rules ()
    [(object$- node) (object node 'pred)]))

(define-match-expander object$i
  (syntax-rules ()
    [(object$i node) (object node 'curr)]))

(define-match-expander object$+
  (syntax-rules ()
    [(object$+ node) (object node 'succ)]))

(define-match-expander object$$
  (syntax-rules ()
    [(object$$ node) (object node 'last)]))

(define/match (object-node v)
  [((object node _)) node])

(define/match (object-index v)
  [((object _ index)) index])

(define/match (object-iterated? v)
  [((object$i _)) #t]
  [((object$- _)) #t]
  [((object$+ _)) #t]
  [(_) #f])

(define/match (object-iterated<=? lower upper)
  [((object$i node1) (object$i node2)) (eq? node1 node2)]
  [((object$i node1) (object$+ node2)) (eq? node1 node2)]
  [((object$0 node1) (object$+ node2)) (eq? node1 node2)]
  [((object$+ node1) (object$+ node2)) (eq? node1 node2)]
  [((object$i node1) (object$- node2)) (eq? node1 node2)]
  [((object$$ node1) (object$- node2)) (eq? node1 node2)]
  [((object$- node1) (object$- node2)) (eq? node1 node2)]
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

; -------------------------------
; Utilities for rule declarations
; -------------------------------

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
  [((rule (and object (object$i _)) _ _)) object]
  [((rule (and object (object$0 _)) _ _)) object]
  [((rule (and object (object$+ _)) _ _)) object]
  [((rule (and object (object$$ _)) _ _)) object]
  [((rule (and object (object$- _)) _ _)) object]
  [((rule (object1 _) _ (fold _ expr)))
   (define/match (search expr)
     [((? number?)) #f]
     [((reference (? object-iterated? object) _)) object]
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

; ------------------------------------
; Utilities for interface declarations
; ------------------------------------

(define (interface-attributes G iface-name)
  (ag-interface-attributes (grammar-interface G iface-name)))

(define (interface-labels G iface-name)
  (map attribute-name (interface-attributes G iface-name)))

; --------------------------------
; Utilities for class declarations
; --------------------------------

(define-match-expander class
  (syntax-rules ()
    [(class name iface-name child-decls attr-decls method-decls rules)
     (cons (symbol name)
           (ag-class iface-name child-decls attr-decls method-decls rules))]))

(define/match (class-name class-decl)
  [((class name _ _ _ _ _))
   name])

(define (class-interface G class-name)
  (ag-class-interface (grammar-class G class-name)))

(define (class-children G class-name)
  (ag-class-children (grammar-class G class-name)))

(define (class-attributes G class-name)
  (let* ([class-body (grammar-class G class-name)]
         [iface-body (grammar-interface G (ag-class-interface class-body))])
    (append (ag-class-attributes class-body)
            (ag-interface-attributes iface-body))))

(define (class-methods G class-name)
  (ag-class-methods (grammar-class G class-name)))

(define (class-rules G class-name)
  (ag-class-rules (grammar-class G class-name)))

; Get expression for initial virtual accumulator.
(define (class-rule-init class-body node label)
  (ormap (match-lambda
           [(rule (object1 (== node)) (== label) (fold init _)) init]
           [_ #f])
         (ag-class-rules class-body)))

; Get expression for basis node in iteration sequence.
(define (class-rule-base class-body node label)
  (ormap (match-lambda
           [(rule (object1 (== node)) (== label) (fold _ step)) step]
           [(rule (object$0 (== node)) (== label) expr) expr]
           [(rule (object$$ (== node)) (== label) expr) expr]
           [(rule (object$i (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-rules class-body)))

; Get expression for inductive nodes in iteration sequence.
(define (class-rule-step class-body node label)
  (ormap (match-lambda
           [(rule (object1 (== node)) (== label) (fold _ step)) step]
           [(rule (object$+ (== node)) (== label) expr) expr]
           [(rule (object$- (== node)) (== label) expr) expr]
           [(rule (object$i (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-rules class-body)))

; Get expression for base/inductive nodes in iteration sequence.
(define (class-rule-iter class-body node label base?)
  (if base?
      (class-rule-base class-body node label)
      (class-rule-step class-body node label)))

; Get expression for a unit node outside all iteration.
(define (class-rule-unit class-body node label)
  (ormap (match-lambda
           [(rule (object1 (== node)) (== label) expr) expr]
           [_ #f])
         (ag-class-rules class-body)))

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

; ------------------------------------
; Utilities for interface declarations
; ------------------------------------

(define-match-expander interface
  (syntax-rules ()
    [(interface name attr-decls)
     (cons (symbol name)
           (ag-interface attr-decls))]))

(define/match (interface-name iface-decl)
  [((interface name _))
   name])

; ---------------------
; General functionality
; ---------------------

; TODO: Implement some sanity checks 
(define (check-grammar grammmar)
  #t)

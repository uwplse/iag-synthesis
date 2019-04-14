#lang rosette

; Abstract syntax for language of attribute grammars

(require "../utility.rkt")

(provide (all-defined-out))

; NOTE: Identifiers are symbols.

(struct ag-grammar
  (; association list of interface ASTs
   interfaces
   ; association list of class ASTs
   classes
   ; association list of traversal templates
   traversals
   ) #:transparent)

(struct ag-recur
  (; on what child node to recur
   child
   ) #:transparent)

(struct ag-visit () #:transparent)

(struct ag-iterate
  (child ; through what child sequence to loop
   body ; iterated traversal components: recur on indexed node (void) or visit self
   ) #:transparent)

; interface definition
(struct ag-interface
  (inputs ; list of input attributes
   outputs ; list of output attributes
   ) #:transparent)

; class definition
(struct ag-class
  (interface ; the nonterminal symbol
   singletons ; list associating each singleton child's name to an interface
   sequences ; list associating each sequence child's name to an interface
   inputs ; list of input attributes
   outputs ; list of output attributes
   methods ; association list of method signatures
   ) #:transparent)

; method signature
(struct ag-method
  (parameters ; list of required traversal parameters
   reads ; list of attributes read
   writes ; list of attributes written
   ) #:transparent)

(define (path->string path)
  (string-join (map symbol->string path) "."))

(define (path->symbol path)
  (compose string->symbol path->string))

; Does the second path extend/refine the first path?
(define (extends? shorter longer)
  (list-prefix? shorter longer eq?))

; Return r s.t. p = q ++ r or #f if no such r exists
(define/match (shift p q)
  [((cons l1 p) (cons l2 q)) #:when (equal? l1 l2) (shift p q)]
  [(r null) r]
  [(_ _) #f])

; Return r s.t. p = q ++ r or q = p ++ r or #f if no such r exists
(define/match (trail p q)
  [((cons l1 p) (cons l2 q)) #:when (equal? l1 l2) (shift p q)]
  [(r null) r]
  [(null r) r]
  [(_ _) #f])

(define (uniquely-associated? lst [same? equal?])
  (check-duplicates lst same? #:key car))

(define (get-method G class-name method-name)
  (lookup (ag-class-methods (get-class G class-name)) method-name eq?))

(define (get-class G name)
  (lookup (ag-grammar-classes G) name eq?))

(define (get-interface G name)
  (lookup (ag-grammar-interfaces G)) name eq?)

(define (get-traversal G name)
  (lookup (ag-grammar-traversals G) name eq?))

(define (get-interface-inputs G name)
  (ag-interface-inputs (get-interface G name)))

(define (get-class-inputs G name)
  (let* ([class (get-class G name)]
         [iface (get-interface G (ag-class-interface class))])
    (append (ag-interface-inputs iface)
            (ag-class-inputs class))))

(define (get-interface-outputs G name)
  (ag-interface-outputs (get-interface G name)))

(define (get-class-outputs G name)
  (let* ([class (get-class G name)]
         [iface (get-interface G (ag-class-interface class))])
    (append (ag-interface-outputs iface)
            (ag-class-outputs class))))

(define (get-interface-attributes G name)
  (let ([iface (get-interface G name)])
    (append (ag-interface-inputs iface)
            (ag-interface-outputs iface))))

(define (get-class-attributes G name)
  (let* ([class (get-class G name)]
         [iface (get-interface G (ag-class-interface class))])
    (append (ag-interface-inputs iface)
            (ag-interface-outputs iface)
            (ag-class-inputs class)
            (ag-class-outputs class))))

(define (get-class-children G name)
  (let ([class-ast (get-class G name)])
    (values (ag-class-singletons class-ast)
            (ag-class-sequences class-ast))))

; Return an association list from interface names to class ASTs.
(define (associate-classes grammar)
  (map (λ (group)
         (cons (ag-class-interface (cdar group)) group))
       (group-by (compose ag-class-interface cdr)
                 (ag-grammar-classes grammar)
                 eq?)))

; TODO: Implement some sanity checks 
(define (check-grammar grammmar)
  #f)

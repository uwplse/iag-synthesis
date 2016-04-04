#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Derivation

(require xml
         "translate.rkt"
         "runtime.rkt"
         "../utility.rkt")

(provide (struct-out ftl-tree)
         xexpr->ftl-tree
         ftl-tree*
         ftl-tree-symbolize!
         ftl-tree-verify
         ftl-tree-verify-input
         ftl-tree-verify-output
         ftl-tree-load
         ftl-tree-bind!
         example-deriv)

; derivations should _definitely_ be treated imperatively, using mutation

; (sub)derivation of an FTL attribute grammar
(struct ftl-tree
  (; nonterminal symbol from vocabulary
   symbol
   ; identifier of production alternative for this node
   option
   ; list associating labels with values, such that an attribute is defined
   ; initially iff it is an input attribute
   attributes
   ; list associating child names with (lists of) derivations (all objects
   ; except 'self)
   children
   ) #:mutable #:transparent)

; ------------------------------------------
; Generation and Verification of Derivations
; ------------------------------------------

; derive an FTL tree of a grammar in IR form given an X-expression
(define (xexpr->ftl-tree grammar xexpr)
  ; XML tags are option names, attributes are attributes, and identifiers are
  ; child names (duplicated for sequences)
  (void))

; derive a symbolic FTL tree of an IR grammar bounded by the given height
(define (ftl-tree* runtime grammar sentence width height input)
  (let* ([types (ftl-runtime-types runtime)])
    (define (recurse symbol bound)
      (match-let* ([(cons option production) (apply choose* (assoc-lookup grammar symbol))]
                   [singletons (ftl-ir-production-singletons production)]
                   [sequences (ftl-ir-production-sequences production)]
                   [inputs (ftl-ir-production-inputs production)]
                   [input? (λ (label) (memq label inputs))]
                   [labels (ftl-ir-production-labels production)]
                   [value* (λ (type)
                             ((ftl-type-generate (assoc-lookup types type))))])
        ; tell Rosette to ensure that terminals are chosen at the appropriate
        ; point(s)
        (assert (>= bound 0))
        ; construct the symbolically filled-out node
        (ftl-tree symbol
                  option
                  (cdrmap value*
                          (if input
                              (filter (compose input? car) labels)
                              labels))
                  (append (cdrmap (λ (child-symbol)
                                    (recurse child-symbol (- bound 1)))
                                  singletons)
                          (cdrmap (λ (child-symbol)
                                    (for/list ([_ (in-range width)])
                                      (recurse child-symbol (- bound 1))))
                                  sequences)))))
    (recurse sentence height)))

; bind any missing attributes of the given tree to symbolic values
(define (ftl-tree-symbolize! runtime grammar tree)
  (match-let* ([listify (λ (l)
                          (if (list? l)
                              l
                              (list l)))]
               [(ftl-tree symbol option bindings children) tree]
               [production (assoc-lookup (assoc-lookup grammar symbol) option)]
               [labels (ftl-ir-production-labels production)]
               [value* (λ (type)
                         ((ftl-type-generate
                           (assoc-lookup (ftl-runtime-types runtime) type))))])

    ; bind all unbound attributes to symbolic values
    (for ([label-type labels])
      (match-let ([(cons label type) label-type])
        (unless (associated? bindings label)
          (ftl-tree-bind! tree 'self label (value* type)))))

    ; recursively do the same on each child tree
    (for ([child children])
      (for ([subchild (listify (cdr child))])
        (ftl-tree-symbolize! runtime grammar subchild)))))

; assert that the derivation is valid w.r.t. the given derivation, depending on
; whether the derivation is input (unevaluated) or output (evaluated); note that
; this does not work with symbolic derivations, as symbolic numbers fail fixnum?
; and the association list utility functions need to be symbolically lifted.
(define (ftl-tree-verify runtime grammar derivation input)
  (let* ([symbol (ftl-tree-symbol derivation)]
         [option (ftl-tree-option derivation)]
         [attributes (ftl-tree-attributes derivation)]
         [children (ftl-tree-children derivation)]
         [production (assoc-lookup (assoc-lookup grammar symbol) option)]
         [labels (ftl-ir-production-labels production)]
         [inputs (ftl-ir-production-inputs production)]
         [singletons (ftl-ir-production-singletons production)]
         [sequences (ftl-ir-production-sequences production)])
    ; validate the quantity of attributes
    (assert (eq? (length attributes)
                 (length (if input
                             inputs
                             labels))))
    ; validate presence and types of given attributes
    (for ([attribute attributes])
      (match-let* ([(cons label value) attribute]
                   [type (assoc-lookup labels label)]
                   [type? (ftl-type-predicate
                           (assoc-lookup (ftl-runtime-types runtime) type))])
        (when input
          (assert (memq label inputs)))
        (assert (type? value))))
    ; validate presence and types of given singleton children
    (for ([child singletons])
      (assert (associated? children (car child)))
      (let ([child-tree (assoc-lookup children (car child))])
        (assert (not (list? child-tree)))
        (assert (eq? (ftl-tree-symbol child-tree)
                     (cdr child)))))
    ; validate presence and types of given sequence children
    (for ([child sequences])
      (match-let ([(cons child-name child-symbol) child])
        (assert (associated? children child-name))
        (let ([child-trees (assoc-lookup children child-name)])
          (assert (list? child-trees))
          (for ([child-tree child-trees])
            (assert (eq? (ftl-tree-symbol child-tree)
                         child-symbol))))))
    ; now recursively validate each child subtree
    (for ([child-binding children])
      (let ([child (cdr child-binding)])
        (if (list? child)
            (for ([subchild child])
              (ftl-tree-verify runtime grammar subchild input))
            (ftl-tree-verify runtime grammar child input))))))

(define (ftl-tree-verify-input runtime grammar derivation)
  (ftl-tree-verify runtime grammar derivation #t))

(define (ftl-tree-verify-output runtime grammar derivation)
  (ftl-tree-verify runtime grammar derivation #f))

; -----------------------------
; Loading and Binding Attribute
; -----------------------------

; bind a value to a label on the specified object relative to the given node by
; functionally extending the attribute association list and destructively
; updating the tree
(define (ftl-tree-bind! self object label value)
  (let ([node (if (eq? object 'self)
                  self
                  (assoc-lookup (ftl-tree-children self) object))]
        [binding (cons label value)])
    (set-ftl-tree-attributes! node (cons binding (ftl-tree-attributes node)))))

; load a dependency from the current node, the indexed node, the previous
; accumulator, or the current accumulator
(define (ftl-tree-load self indexed previous current dependency)
  (match dependency
    [(ftl-ir-dependency 'self 'none label)
     (assoc-lookup (ftl-tree-attributes self) label)]
    [(ftl-ir-dependency object 'none label)
     (assoc-lookup (ftl-tree-attributes
                    (assoc-lookup (ftl-tree-children self) object))
                   label)]
    [(ftl-ir-dependency object 'previous label)
     (assoc-lookup previous (cons object label))]
    [(ftl-ir-dependency object 'first label)
     (assoc-lookup (ftl-tree-attributes
                    (first (assoc-lookup (ftl-tree-children self) object)))
                   label)]
    [(ftl-ir-dependency object 'current label)
     ; TODO: (assert (eq? object child-name))?
     (assoc-lookup current
                   label
                   (assoc-lookup (ftl-tree-attributes indexed) label))]
    [(ftl-ir-dependency object 'last label)
     (assoc-lookup (ftl-tree-attributes
                    (last (assoc-lookup (ftl-tree-children self) object)))
                   label)]))

; -------------------------------------
; Example derivation of example grammar
; -------------------------------------

(define example-xml
"
<Origin x='1' y='17'>
  <Endpoint />
  <Relative dx='3' dy='7'>
    <Endpoint />
  </Relative/>
</Origin>
")

; an example derivation of the example FTL string (example-ftl from parse.rkt)
(define example-deriv
  (ftl-tree 'Root
            'Origin
            '((x . 7) (y . 10))
            `((p . (,(ftl-tree 'Point
                               'Endpoint
                               '()
                               '())
                    ,(ftl-tree 'Point
                               'Relative
                               '((dx . 3) (dy . 7))
                               `((p . (,(ftl-tree 'Point
                                                  'Endpoint
                                                  '()
                                                  '()))))))))))

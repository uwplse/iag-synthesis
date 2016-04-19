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
         example-deriv
         example-large-deriv)

; -------------------------
; Derivation Tree Structure
; -------------------------

; (sub)derivation of an FTL attribute grammar
(struct ftl-tree
  (; nonterminal symbol from vocabulary
   symbol
   ; identifier of production alternative for this node
   option
   ; list associating labels with values, such that an attribute is defined
   ; initially iff it is an input attribute, that is updated functionally
   attributes
   ; list associating child names with (lists of) derivations (all objects
   ; except 'self) that is updated both functionally and destructively
   children
   ) #:mutable
     #:transparent)

; -----------------------
; Equality of Derivations
; -----------------------

; Note that we intentionally do not implement the equal+hash generic interface
; because Racket makes assumptions that are unsafe with regard to Rosette
; semantics (e.g., expecting concrete hashes for symbolic values). Besides, the
; below function really implements an equivalence relation, not equality.

; whether two derivations are equivalent, which requires labels (of both
; attributes and children) to be eq? and their values to be equal?
(define (ftl-tree-equal? tree1 tree2)
  (or (eq? tree1 tree2) ; short-circuit on identity
      (match-let* ([(ftl-tree symbol1
                              option1
                              attributes1
                              children1) tree1]
                   [(ftl-tree symbol2
                              option2
                              attributes2
                              children2) tree2]
                   [labels1 (assoc-symbols attributes1)]
                   [labels2 (assoc-symbols attributes2)]
                   ; identifier ~ child label
                   [idents1 (assoc-symbols children1)]
                   [idents2 (assoc-symbols children2)])
        (and (eq? symbol1 symbol2)
             (eq? option1 option2)
             (eq? (length labels1) (length labels2))
             (andmap (λ (label)
                       (equal? (assoc-lookup attributes1 label)
                               (assoc-lookup attributes2 label)))
                     labels1)
             (eq? (length idents1) (length idents2))
             (andmap (λ (ident)
                       (let ([child-list1 (listify (cdr (assoc-lookup children1
                                                                      ident)))]
                             [child-list2 (listify (cdr (assoc-lookup children2
                                                                      ident)))])
                         (and (eq? (length child-list1)
                                   (length child-list2))
                              (andmap (λ (child-pair)
                                        (ftl-tree-equal? (car child-pair)
                                                         (cdr child-pair)))
                                      (zip child-list1 child-list2)))))
                     idents1)))))

 ;; (define (ftl-tree-hash hash-field tree rec-hash)
 ;;  (match-let* ([(ftl-tree symbol
 ;;                          option
 ;;                          attributes
 ;;                          children) tree]
 ;;               [listify (λ (l) (if (list? l) l (list l)))])
 ;;    (+ (* 101 (hash-field symbol))
 ;;       (* 311 (hash-field option))
 ;;       (* 701 (foldl (λ (attribute sum)
 ;;                       (+ sum (hash-field attribute)))
 ;;                     0
 ;;                     attributes))
 ;;       (* 997 (foldl (λ (child-list sum)
 ;;                       (+ sum
 ;;                          (* 7
 ;;                             (foldl (λ (child sum)
 ;;                                      (+ sum (rec-hash child)))
 ;;                                    0
 ;;                                    child-list))))
 ;;                     0
 ;;                     (map (compose listify cdr) children))))))

; ------------------------------------------
; Generation and Verification of Derivations
; ------------------------------------------

; derive an FTL tree of a grammar in IR form given an XML string
(define (xml->ftl-tree runtime grammar sentence xml-string)
  (xexpr->ftl-tree runtime grammar sentence (string->xexpr xml-string)))

; derive an FTL tree of a grammar with the given sentence symbol from an
; X-expression, assuming that all singleton children are given first (in the same
; order as the grammar) and followed by at most one child sequence
(define (xexpr->ftl-tree runtime grammar sentence xexpr)
  (match xexpr
    [(list-rest tag
                (and xattributes
                     (list (list (? symbol?) (? string?))...))
                xcontent)
     (match-let* ([xchildren (filter list? xcontent)]
                  [(ftl-ir-production _ context _ singletons sequences)
                   (assoc-lookup (assoc-lookup grammar sentence) tag)])

       (define-values (single-children xchild-sequence)
         (for/fold ([children null]
                    [xchildren xchildren])
                   ([singleton singletons])
           (match-let* ([(cons xchild xchildren) xchildren]
                        [(cons label symbol) singleton]
                        [child (xexpr->ftl-tree grammar symbol xchild)])
             (values (cons (cons label child) children) xchildren))))

       (define children
         (match sequences
           [(list (cons label symbol))
            (define child-sequence
              (for/list ([xchild xchild-sequence])
                (xexpr->ftl-tree runtime grammar symbol xchild)))
            (cons (cons label child-sequence) single-children)]
           [null
            single-children]))

       (define attributes
         (for/list ([xattribute xattributes])
           (match-let* ([(list label value) xattribute]
                        [type (assoc-lookup (ftl-runtime-types runtime)
                                            (assoc-lookup context label))]
                        [parse (ftl-type-parse type)])
             (cons label (parse value)))))

       (ftl-tree sentence tag attributes children))]

    [(list-rest tag children)
     (xexpr->ftl-tree runtime grammar sentence (list tag null children))]

    [_
     (raise-arguments-error 'xexpr->ftl-tree
                            "could not convert X-expression to tree"
                            'xexpr xexpr)]))

; derive a symbolic FTL tree of an IR grammar bounded by the given height
(define (ftl-tree* runtime grammar sentence width height input)
  (let* ([types (ftl-runtime-types runtime)])
    (define (recurse symbol bound)
      ; tell Rosette to ensure that terminals are chosen at the appropriate
      ; point(s)
      (assert (>= bound 0))
      (let* ([alternative (apply choose* (assoc-lookup grammar symbol))]
             [option (car alternative)]
             [production (cdr alternative)]
             [singletons (ftl-ir-production-singletons production)]
             [sequences (ftl-ir-production-sequences production)]
             [inputs (ftl-ir-production-inputs production)]
             [input? (λ (label)
                       (memq label inputs))]
             [labels (ftl-ir-production-labels production)]
             [value* (λ (type)
                       ((ftl-type-generate (assoc-lookup types type))))])
        ; create symbolic union of concrete nodes with symbolic terms for
        ; attribute values and with symbolic unions for child nodes, to simplify
        ; symbolically lifting other code (with greater efficiency as well)
        (for/all ([option option])
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
                                     sequences))))))
    (recurse sentence height)))

; bind any missing attributes of the given tree to symbolic values
(define (ftl-tree-symbolize! runtime grammar tree)
  (for/all ([tree tree])
    (match-let* ([(ftl-tree symbol option bindings children) tree]
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
          (ftl-tree-symbolize! runtime grammar subchild))))))

; assert that the derivation is valid w.r.t. the given derivation, depending on
; whether the derivation is input (unevaluated) or output (evaluated); note that
; this does not work with symbolic derivations, as symbolic numbers fail fixnum?
; and the association list utility functions need to be symbolically lifted.
(define (ftl-tree-verify runtime grammar derivation input)
  (for/all ([derivation derivation])
    (match-let* ([(ftl-tree symbol option attributes children)
                  derivation]
                 [production (assoc-lookup (assoc-lookup grammar symbol)
                                           option)]
                 [(ftl-ir-production inputs labels _ singletons sequences)
                  production])
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
              (ftl-tree-verify runtime grammar child input)))))))

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
    (set-ftl-tree-attributes! node
                              (cons binding (ftl-tree-attributes node)))))

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

(define example-large-deriv
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
                                                  'Relative
                                                  '((dx . 3) (dy . 7))
                                                  `((p . (,(ftl-tree 'Point
                                                                     'Relative
                                                                     '((dx . 3) (dy . 7))
                                                                     `((p . (,(ftl-tree 'Point
                                                                                        'Endpoint
                                                                                        '()
                                                                                        '())))))

                                                          ,(ftl-tree 'Point
                                                                     'Endpoint
                                                                     '()
                                                                     '())))))

                                       ,(ftl-tree 'Point
                                                  'Endpoint
                                                  '()
                                                  '())))))
                    ,(ftl-tree 'Point
                               'Relative
                               '((dx . 3) (dy . 7))
                               `((p . (,(ftl-tree 'Point
                                                  'Endpoint
                                                  '()
                                                  '()))))))))))

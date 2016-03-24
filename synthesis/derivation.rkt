#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Derivation

(require xml
         "translate.rkt"
         "runtime.rkt"
         "../utility.rkt")

(provide (struct-out ftl-tree)
         ftl-tree-verify
         ftl-tree-verify-input
         ftl-tree-verify-output
         ftl-tree-evaluate
         ftl-tree-iterate
         load-dependency
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

; assert that the derivation is valid w.r.t. the given derivation, depending on
; whether the derivation is input (unevaluated) or output (evaluated); note that
; this does not work with symbolic derivations, as symbolic numbers fail fixnum?
; and the association list utility functions need to be symbolically lifted.
(define (ftl-tree-verify runtime grammar derivation input)
  (let* ([vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-tree-symbol derivation)]
         [option (ftl-tree-option derivation)]
         [attributes (ftl-tree-attributes derivation)]
         [children (ftl-tree-children derivation)]
         [production (assoc-lookup (assoc-lookup vocab symbol) option)]
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

; ------------------------
; Evaluation of attributes
; ------------------------

; load a dependency using the current node, the indexed node, the previous
; accumulator, and the current accumulator (an association list)
; TODO: really need both previous and current? For angelic, it'll be fully
; filled with symbolic attributes, but scheduled will just fold it along
; the iterated evaluations.
(define (load-dependency self indexed previous current dependency)
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
     ; TODO: assert that object actually is indexed?
     (assoc-lookup current
                   label
                   (assoc-lookup (ftl-tree-attributes indexed) label))]
    [(ftl-ir-dependency object 'last label)
     (assoc-lookup (ftl-tree-attributes
                    (last (assoc-lookup (ftl-tree-children self) object)))
                   label)]))

; perform an IR evaluation
(define (ftl-tree-evaluate self indexed previous current evaluation)
  (match-let* ([(ftl-ir-evaluation function dependencies) evaluation]
               [arguments (vector-map (curry load-dependency
                                             self
                                             indexed
                                             previous
                                             current)
                                      dependencies)])
    (function arguments)))

; self : node
; children : [node]
; init : node -> [(k, v)]
; step : node * node * [(k, v)] -> node * node * [(k, v)]
; iterate : self * children * init * step -> node * [node] * [(k, v)]
; iterate over a child sequence, invoking a function at each step
(define (ftl-tree-iterate self children init step)
  (foldl (λ (child result)
           (match-let* ([(list self new-children accum) result]
                        [(list new-self new-child new-accum) (step self child accum)])
             (list new-self (cons new-child new-children) new-accum)))
         (list self null (init self))
         children))

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

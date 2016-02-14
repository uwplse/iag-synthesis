#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Derivation

(require xml
         "../utility.rkt"
         "translate.rkt")

(provide (struct-out ftl-derivation)
         xml-derive
         derive*
         example-deriv)

(struct ftl-derivation
  (symbol ; nonterminal symbol from vocabulary
   option ; identifier of production alternative for this nonterminal
   attributes ; list associating labels with values, such that an attribute is defined initially iff it is an input
   children ; list associating child names with (lists of) derivations (all objects except 'self)
   ) #:transparent)

; TODO: read derivation from XML
(define (xml-derive xml-string)
  ; XML tags are option names and attributes are inputs
  ; children are expected to be in order, and sequences are consumed greedily
  (define (derive grammar tree)
    (void))
  (derive (string->xexpr xml-string)))

; TODO: generate symbolic derivation (derivation oracle)
(define (derive* grammar height)
  (let* ([symbol (choose* (hash-keys grammar))]
         [option (choose* (hash-keys (hash-ref grammar symbol)))])
    (ftl-derivation symbol
                    option
                    (void)
                    (void))))

; -------------------------
; Traversals of derivations
; -------------------------

; FIXME: how to control or specify "short-circuited" traversals?
; Pertaining to the above, is the conservative method of aborting a
; traversal if the subtree is a certain nonterminal sufficient? How
; about including an 'abort'/'short-circuit'/'guard' parameter to the
; traversal functions below, to decide whether to continue the traversal
; as a function of the current node (usually the nonterminal and the
; production)?

; child must be a sequence
; accumulator is list associating $- attributes to values
; initial : accumulator
; visit-parent : accumulator * node -> accumulator
; visit-child : accumulator * node * node -> accumulator
; TODO: in-order (i.e., "recursive") traversal
(define (inorder tree child initial visit-parent visit-child)
  (void))

; visit-parent : node -> void
; visit-child : node * node -> void
; TODO: pre-order traversal
(define (preorder tree child visit-parent visit-child)
  (void))

; visit-parent : node -> void
; visit-child : node * node -> void
; TODO: post-order traversal
(define (postorder tree child visit-parent visit-child)
  (void))

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
  (ftl-derivation 'Root
                  'Origin
                  (let ([symtab (make-symtab '(x y) (void))])
                    (symtab-set! symtab 'x 1)
                    (symtab-set! symtab 'y 17)
                    symtab)
                  (hasheq 'p
                          (list (ftl-derivation 'Point
                                                'Endpoint
                                                (make-symtab '(x y bx by) (void))
                                                (hasheq))
                                (ftl-derivation 'Point
                                                'Relative
                                                (let ([symtab (make-symtab '(x y bx by dx dy)
                                                                           (void))])
                                                  (symtab-set! symtab 'dx 3)
                                                  (symtab-set! symtab 'dy 7)
                                                  symtab)
                                                (hasheq 'p
                                                        (list (ftl-derivation 'Point
                                                                              'Endpoint
                                                                              (make-symtab '(x y bx by) (void))
                                                                              (hasheq)))))))))

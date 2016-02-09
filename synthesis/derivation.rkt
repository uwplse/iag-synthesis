#lang s-exp rosette

; Functional Tree Language (FTL) intepreter
; Derivation

(require xml
         "../utility.rkt"
         "translate.rkt")

(provide (struct-out ftl-derivation)
         xml-derive
         derive*
         example-deriv)

(struct ftl-derivation
  (symbol ; key from vocabulary
   option ; identifier of production instance
   attributes ; symbol table of labels to values, such that all input attributes are set and the rest are (void)
   children ; dictionary of IR derivations (or lists of them) keyed by child names
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
    (ftl-ir-derivation symbol
                       option
                       (void)
                       (void))))

; ------------
; Demo program
; ------------

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
#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Angelic Evaluation (Interpretation)

(require "../utility.rkt"
         "parse.rkt"
         "translate.rkt"
         "runtime.rkt"
         "derivation.rkt")

; interpret : L(G_FTL) * L([[L(G_FTL)]]) -> L([[L(G_FTL)]])
(define (interpret-angelic ftl root tree runtime) ; TODO: parse tree from xml input
  (current-bitwidth 6)
  (evaluate-angelic tree (translate (parse-ftl ftl) root runtime) runtime))

(define (interpret-example)
  (evaluate-angelic ftl-base-runtime example-ir example-deriv))

(define (evaluate-angelic runtime grammar derivation)
  (assert (eq? (ftl-ir-grammar-sentence grammar)
               (ftl-derivation-symbol derivation))) ; assert that we actually were given a root
  (symbolize runtime grammar derivation)
  (define solution (solve (constrain grammar derivation)))
  (displayln "solution: ")
  (displayln solution)
  (evaluate derivation solution))

(define (symbolize runtime grammar derivation)
  (let* ([recurse (curry symbolize runtime grammar)]
         [oracle (λ (type)
                   (cdr (hash-ref (ftl-runtime-types runtime) type)))]
         [vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-derivation-symbol derivation)]
         [option (ftl-derivation-option derivation)]
         [attributes (ftl-derivation-attributes derivation)]
         [children (ftl-derivation-children derivation)]
         [alternative (dict-ref (dict-ref vocab symbol) option)]
         [labels (ftl-ir-alternative-labels alternative)]
         [typeof (λ (label)
                   (dict-ref labels label))])
    (for-each (λ (label)
                (when (void? (symtab-ref attributes label))
                  (symtab-set! attributes label ((oracle (typeof label))))))
              (symtab-keys attributes))
    (for-each (λ (child)
                (if (list? child)
                    (for-each recurse child)
                    (recurse child)))
              (dict-values children))
    derivation))

; FIXME: refactor
(define (constrain grammar derivation) ; constrain angelic values
  (let* ([recurse (curry constrain grammar)]
         [vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-derivation-symbol derivation)]
         [option (ftl-derivation-option derivation)]
         [attributes (ftl-derivation-attributes derivation)]
         [children (ftl-derivation-children derivation)]
         [alternative (dict-ref (dict-ref vocab symbol) option)]
         [sequences (ftl-ir-alternative-sequences alternative)]
         [definitions (ftl-ir-alternative-definitions alternative)]
         [listify (λ (l) (if (list? l) l (list l)))])
    (dict-for-each definitions ; FIXME: actually verify that inputs, children given and of the correct types
                   (λ (attribute action)
                     (let* ([def-obj (car attribute)]
                            [def-lbl (cdr attribute)]
                            [def-seq (dict-has-key? sequences def-obj)]
                            [lbl-ref (λ (deriv label)
                                       (symtab-ref (ftl-derivation-attributes deriv)
                                                   label))]
                            [reduce (ftl-ir-reduction? action)]
                            [lookup (λ (child-attributes accumulator dependency)
                                      (let ([dep-obj (ftl-ir-dependency-object dependency)]
                                            [dep-idx (ftl-ir-dependency-index dependency)]
                                            [dep-lbl (ftl-ir-dependency-label dependency)])
                                        (cond
                                          [(and (eq? dep-obj def-obj)
                                                def-seq
                                                (eq? dep-idx 'index)) (symtab-ref child-attributes dep-lbl)]
                                          [(and (eq? dep-obj def-obj)
                                                reduce
                                                (eq? dep-idx 'previous)) accumulator]
                                          [(and (eq? dep-obj 'self)
                                                (eq? dep-idx 'none)) (symtab-ref attributes dep-lbl)]
                                          [(eq? dep-idx 'none) (lbl-ref (dict-ref children dep-obj) dep-lbl)]
                                          [(eq? dep-idx 'first) (lbl-ref (first (dict-ref children dep-obj)) dep-lbl)]
                                          [(eq? dep-idx 'last) (lbl-ref (last (dict-ref children dep-obj)) dep-lbl)])))]
                            [echo-assert (λ (b)
                                           (display "assertion: ")
                                           (displayln b)
                                           (assert b))])
                       (if reduce
                           (let* ([init (ftl-ir-reduction-init-define action)]
                                  [init-eval (ftl-ir-definition-evaluate init)]
                                  [init-deps (ftl-ir-definition-dependencies init)]
                                  [step (ftl-ir-reduction-step-define action)]
                                  [step-eval (ftl-ir-definition-evaluate step)]
                                  [step-deps (ftl-ir-definition-dependencies step)]
                                  [iter (dict-ref children (ftl-ir-reduction-iter-child action))]
                                  [proc (λ (child accum)
                                          (let* ([child-attributes (ftl-derivation-attributes child)]
                                                 [result (step-eval (vector-map (curry lookup child-attributes accum)
                                                                                step-deps))])
                                            (when def-seq
                                              (echo-assert (equal? (symtab-ref child-attributes def-lbl) result))
                                              result)))]
                                  [seed (init-eval (vector-map (curry lookup (void) (void)) init-deps))]
                                  [result (foldl proc seed iter)])
                             (unless def-seq
                               (echo-assert (equal? (lbl-ref (dict-ref children def-obj) def-lbl)
                                                    result))))
                           (let* ([eval (ftl-ir-definition-evaluate action)]
                                  [deps (ftl-ir-definition-dependencies action)]
                                  [iter (listify (if (eq? def-obj 'self)
                                                     derivation
                                                     (dict-ref children def-obj)))]
                                  [proc (λ (child)
                                          (let* ([child-attributes (ftl-derivation-attributes child)]
                                                 [result (eval (vector-map (curry lookup child-attributes (void))
                                                                           deps))])
                                            (echo-assert (equal? (symtab-ref child-attributes def-lbl) result))))])
                             (for-each proc iter))))))
    (for-each (λ (child)
                (if (list? child)
                    (for-each recurse child)
                    (recurse child)))
              (dict-values children))))

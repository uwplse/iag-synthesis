#lang s-exp rosette

; Functional Tree Traversal Transformation Language (FT3L) intepreter
; Execution

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         "../utility.rkt"
         "parse.rkt"
         "runtime.rkt"
         "translate.rkt")

;(provide verify-schedule
;         synthesize-schedule
;         toposort-schedule)

; ------------
; Demo program
; ------------

; TODO: parse both grammar (FTL) and derivation (XML or w/e) from regular files

; an example derivation of the example FTL string
(define example-deriv
  (ftl-ir-derivation 'Root
                 'Origin
                 (let ([symtab (make-symbol-table '(x y) (void))])
                   (dict-set! symtab 'x 1)
                   (dict-set! symtab 'y 17)
                   symtab)
                 (hasheq 'p
                         (list (ftl-ir-derivation 'Point
                                              'Endpoint
                                              (make-symbol-table '(x y bx by) (void))
                                              (hasheq))
                               (ftl-ir-derivation 'Point
                                              'Relative
                                              (let ([symtab (make-symbol-table '(x y bx by dx dy)
                                                                               (void))])
                                                (dict-set! symtab 'dx 3)
                                                (dict-set! symtab 'dy 7)
                                                symtab)
                                              (hasheq 'p
                                                      (list (ftl-ir-derivation 'Point
                                                                           'Endpoint
                                                                           (make-symbol-table '(x y bx by) (void))
                                                                           (hasheq)))))))))

; -----------------
; Angelic execution
; -----------------

(define example-ir (translate example-ast 'Root ftl-base-runtime))

(define (test g)
  (current-bitwidth 6)
  (evaluate-angelic example-deriv g ftl-base-runtime))

(define (interpret-angelic ftl root tree runtime)
  (current-bitwidth 6)
  (evaluate-angelic tree (translate (parse-ftl ftl) root runtime) runtime))

(define (symbolize grammar runtime derivation)
  (let* ([recurse (curry symbolize grammar runtime)]
         [oracle (λ (type)
                   (cdr (hash-ref (ftl-runtime-types runtime) type)))]
         [vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-ir-derivation-symbol derivation)]
         [option (ftl-ir-derivation-option derivation)]
         [attributes (ftl-ir-derivation-attributes derivation)]
         [children (ftl-ir-derivation-children derivation)]
         [alternative (dict-ref (dict-ref vocab symbol) option)]
         [labels (ftl-ir-alternative-labels alternative)]
         [typeof (λ (label)
                   (dict-ref labels label))])
    (for-each (λ (label)
                (when (void? (dict-ref attributes label))
                  (dict-set! attributes label ((oracle (typeof label))))))
              (dict-keys attributes))
    (for-each (λ (child)
                (if (list? child)
                    (for-each recurse child)
                    (recurse child)))
              (dict-values children))
    derivation))

(define (constrain grammar derivation) ; constrain angelic values
  (let* ([recurse (curry constrain grammar)]
         [vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-ir-derivation-symbol derivation)]
         [option (ftl-ir-derivation-option derivation)]
         [attributes (ftl-ir-derivation-attributes derivation)]
         [children (ftl-ir-derivation-children derivation)]
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
                                       (dict-ref (ftl-ir-derivation-attributes deriv)
                                                 label))]
                            [reduce (ftl-ir-reduction? action)]
                            [lookup (λ (child-attributes accumulator dependency)
                                      (let ([dep-obj (ftl-ir-dependency-object dependency)]
                                            [dep-idx (ftl-ir-dependency-index dependency)]
                                            [dep-lbl (ftl-ir-dependency-label dependency)])
                                        (cond
                                          [(and (eq? dep-obj def-obj)
                                                def-seq
                                                (eq? dep-idx 'index)) (dict-ref child-attributes dep-lbl)]
                                          [(and (eq? dep-obj def-obj)
                                                reduce
                                                (eq? dep-idx 'previous)) accumulator]
                                          [(and (eq? dep-obj 'self)
                                                (eq? dep-idx 'none)) (dict-ref attributes dep-lbl)]
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
                                          (let* ([child-attributes (ftl-ir-derivation-attributes child)]
                                                 [result (step-eval (vector-map (curry lookup child-attributes accum)
                                                                                step-deps))])
                                            (when def-seq
                                              (echo-assert (equal? (dict-ref child-attributes def-lbl) result))
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
                                          (let* ([child-attributes (ftl-ir-derivation-attributes child)]
                                                 [result (eval (vector-map (curry lookup child-attributes (void))
                                                                           deps))])
                                            (echo-assert (equal? (dict-ref child-attributes def-lbl) result))))])
                             (for-each proc iter))))))
    (for-each (λ (child)
                (if (list? child)
                    (for-each recurse child)
                    (recurse child)))
              (dict-values children))))

(define (evaluate-angelic derivation grammar runtime)
  (assert (eq? (ftl-ir-grammar-sentence grammar)
               (ftl-ir-derivation-symbol derivation))) ; assert that we actually were given a root
  (symbolize grammar runtime derivation)
  (define solution (solve (constrain grammar derivation)))
  (displayln "solution: ")
  (displayln solution)
  (evaluate derivation solution))

; ------------------
; Schedule structure
; ------------------

; schedule language
(struct schedule
  (traverse ; either 'pre or 'post
   compute ; hash table of lists of object-label pairs keyed by symbol-alternative pairs
;  parallel ; next schedule to run in parallel or (void)
   sequence ; next schedule to run in sequence or (void)
   ) #:transparent)

; ------------------
; Schedule execution
; ------------------

(define (evaluate-schedule derivation schedule)
  (void))

; ---------------------
; Schedule verification
; ---------------------

; 1. choose a symbol
; 2. choose an alternative
; 3. create derivation node
; 4. assign symbolic values to inputs (unless given by grammar)
(define (derivation* grammar)
  (let* ([symbol (choose* (hash-keys grammar))]
         [option (choose* (hash-keys (hash-ref grammar symbol)))])
    (ftl-ir-derivation symbol
                       option
                       (void)
                       (void))))

; FIXME: actually implement; template copied from angelic evaluation
(define (booleanize grammar runtime) ; booleanize grammar and runtime
  (let* ([vocabulary (ftl-ir-grammar-vocabulary grammar)]
         [sentence (ftl-ir-grammar-sentence grammar)]
         [booleanize-body (λ (body)
                            (void))]
         [booleanize-alt (λ (alt)
                           (booleanize-body alt))]
         [booleanize-prod (λ (prod)
                            (make-immutable-hasheq
                             (hash-map prod
                                       (λ (name alt)
                                         (cons name
                                               (booleanize-alt alt))))))]
         [booleanize-vocab (make-immutable-hasheq
                            (hash-map vocabulary
                                      (λ (symbol prod)
                                        (cons symbol
                                              (booleanize-prod prod)))))])
    (ftl-ir-grammar (booleanize-vocab vocabulary)
                sentence)))

; FIXME: actually implement; template copied from angelic evaluation
(define (verify-schedule-derivation grammar runtime schedule derivation)
  (let* ([recurse (curry symbolize grammar runtime)]
         [oracle (λ (type)
                   (cdr (hash-ref (ftl-runtime-types runtime) type)))]
         [vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-ir-derivation-symbol derivation)]
         [option (ftl-ir-derivation-option derivation)]
         [attributes (ftl-ir-derivation-attributes derivation)]
         [children (ftl-ir-derivation-children derivation)]
         [alternative (dict-ref (dict-ref vocab symbol) option)]
         [labels (ftl-ir-alternative-labels alternative)]
         [typeof (λ (label)
                   (dict-ref labels label))])
    (for-each (λ (label)
                (when (void? (dict-ref attributes label))
                  (dict-set! attributes label ((oracle (typeof label))))))
              (dict-keys attributes))
    (for-each (λ (child)
                (if (list? child)
                    (for-each recurse child)
                    (recurse child)))
              (dict-values children))
    derivation))

; 1. booleanize grammar (make all attributes boolean)
; 2. replace all evaluation rules by the conjunction of their dependencies
; 3. define fully-attributed? to be the conjunction of all attributes in a derivation
; 4. assert that the result of executing the schedule for all k-depth derivations of
;    the booleanized grammar is true
(define (verify-schedule grammar runtime schedule) ; constrain for verification for all (k-size) derivations of grammar, with singleton sequences
  (let* ([recurse (verify-schedule grammar runtime)])
    (void)))

; ------------------
; Schedule synthesis
; ------------------

; synthesize a schedule s such that (verify-schedule g s) is true
(define (schedule-ir g)
  (void))

; -------------------------------
; Topologically sorted scheduling
; -------------------------------

(define (toposort dag)
  (void))

(define (grammar->dag g)
  (void))

(define (list->schedule l)
  (void))

(define toposort-schedule
  (compose list->schedule
           toposort
           grammar->dag))
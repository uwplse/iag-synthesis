#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Angelic Evaluation (Interpretation)

(require "../utility.rkt"
         "parse.rkt"
         "translate.rkt"
         "runtime.rkt"
         "derivation.rkt")

; interpret : L(G_FTL) * L([[L(G_FTL)]]) -> L([[L(G_FTL)]])
(define (ftl-interpret-angelic runtime ftl root tree)
  (current-bitwidth 6)
  (ftl-evaluate-angelic runtime (ftl-ir-translate (parse-ftl ftl) root runtime) tree))

(define (interpret-example)
  (ftl-evaluate-angelic ftl-base-runtime example-ir example-deriv))

(define (ftl-evaluate-angelic runtime grammar input)
  ; assert that we actually were given a root
  (assert (eq? (ftl-ir-grammar-sentence grammar)
               (ftl-tree-symbol input)))
  ; let all output attribute values be symbolic
  (let ([output (symbolize runtime grammar input)])
    ; assert the actions' assignments in whatever order
    (constrain runtime grammar output)
    ; request that Rosette solve for the attributes' concrete values
    (define solution (solve #t))
    (displayln "solution: ")
    (displayln solution)
    ; substitute the solved concrete values in for the symbolic attribute values
    (evaluate output solution)))

; generate a version of the given derivation with symbolic output attributes
(define (symbolize runtime grammar derivation)
  (let ([oracle (λ (type)
                  (ftl-type-generate
                   (assoc-lookup (ftl-runtime-types runtime) type)))]
        [vocab (ftl-ir-grammar-vocabulary grammar)])

    ; recurse on either a child tree or list of child trees
    (define (traverse child)
      (if (list? child)
          (map recurse child)
          (recurse child)))
    
    (define (recurse tree)
      (let* ([symbol (ftl-tree-symbol tree)]
             [option (ftl-tree-option tree)]
             [bindings (ftl-tree-attributes tree)]
             [children (ftl-tree-children tree)]
             [production (assoc-lookup (assoc-lookup vocab symbol) option)]
             [labels (ftl-ir-production-labels production)]
             [noninputs (filter (λ (label)
                                  (not (associated? bindings (car label))))
                                labels)])
        ; bind all output attributes to symbolic values
        (define (bind* label-type bindings)
          (match-let* ([(cons label type) label-type]
                       [oracle (ftl-type-generate
                                (assoc-lookup
                                 (ftl-runtime-types runtime)
                                 type))])
            (cons (cons label (oracle))
                  bindings)))
        
        (ftl-tree symbol
                  option
                  (foldl bind* bindings noninputs)
                  (cdrmap traverse children))))

    (recurse derivation)))

; constrain the symbolic values of a loop's initial accumulator
(define (constrain-init actions self current)
  (for ([action actions])
    (match-let* ([(cons name defn) action]
                 [action (assoc-lookup actions name)]
                 [(ftl-ir-evaluation fun deps)
                  (ftl-ir-reduction-init (ftl-ir-definition-evaluate defn))]
                 [get (curry ftl-tree-load self (void) null null)]
                 [val (fun (vector-map get deps))]
                 [sym (assoc-lookup current name)])
      (assert (eq? val sym))))
  current)

; constrain the symbolic values of an intermediate step in a loop (i.e., assert
; equalities for assignments to the indexed node and/or the next accumulator)
(define (constrain-step actions child self indexed previous current)
  (for ([action actions])
    (match-let* ([(cons name defn) action]
                 [(cons object label) name]
                 [sym (assoc-lookup current name)]
                 [temp (ftl-ir-definition-evaluate defn)]
                 [(ftl-ir-evaluation fun deps)
                  (if (ftl-ir-reduction? temp)
                      (ftl-ir-reduction-step temp)
                      temp)]
                 [get (curry ftl-tree-load self indexed previous current)]
                 [val (fun (vector-map get deps))])
      ; if this action is a fold, then assert that this step's
      ; evaluated (concrete) value equals its next accumulator
      ; (symbolic) value
      (unless (void? sym)
        (assert (eq? val sym)))

      ; if this action defines attributes of a child sequence, then
      ; assert that this step's evaluated (concrete) value equals
      ; the indexed node's (symbolic) value
      (when (eq? object child)
        (assert (eq? val
                     (assoc-lookup (ftl-tree-attributes indexed) label))))))
  current)

; constrain output attributes by symbolic evaluation and assertion
(define (constrain runtime grammar tree) ; constrain angelic values
  (let* ([recurse (curry constrain runtime grammar)]
         [vocab (ftl-ir-grammar-vocabulary grammar)]
         [symbol (ftl-tree-symbol tree)]
         [option (ftl-tree-option tree)]
         [attributes (ftl-tree-attributes tree)]
         [children (ftl-tree-children tree)]
         [production (assoc-lookup (assoc-lookup vocab symbol) option)]
         [labels (ftl-ir-production-labels production)]
         [singletons (ftl-ir-production-sequences production)]
         [sequences (ftl-ir-production-sequences production)]
         [iterations (assoc-group (compose ftl-ir-definition-iterate cdr)
                                  (ftl-ir-production-definitions production))]
         [listify (λ (l) (if (list? l) l (list l)))]
         [typeof (λ (object-label)
                   (match-let ([(cons object label) object-label])
                     (if (eq? object 'self)
                         (assoc-lookup labels label)
                         (let* ([symbol (or (assoc-lookup sequences object)
                                            (assoc-lookup singletons object))]
                                ; note that this may err if an interface has
                                ; no class implementations
                                [production (car (assoc-lookup vocab symbol))]
                                [labels (ftl-ir-production-labels production)])
                           (assoc-lookup labels label)))))]
         ; symbolic value of, that is
         [valueof (λ (typename)
                    (ftl-type-generate
                     (assoc-lookup (ftl-runtime-types runtime)
                                   typename)))])
    ; handle definitions (note that we can completely ignore node mutation, as
    ; we only care that all necessary assertions are performed)
    (for ([iteration iterations])
      (if (void? (car iteration))
          ; iterate over nothing
          (for ([action (cdr iteration)])
            (match-let* ([(cons (cons object label)
                                (ftl-ir-definition _ eval)) action]
                         [get (curry ftl-tree-load tree (void) null null)]
                         [(ftl-ir-evaluation fun deps) eval]
                         [val (fun (vector-map get deps))]
                         [dep (ftl-ir-dependency object 'none label)]
                         [sym (get dep)])
              ; assert equality of attribute to its value
              (assert (eq? sym val))))

          ; iterate over child sequence
          (match-let* ([(cons child-name actions) iteration]
                       [folds (filter (compose ftl-ir-reduction?
                                               ftl-ir-definition-evaluate
                                               cdr)
                                      actions)]
                       [accum*-list (assoc-map-keys (compose valueof
                                                             typeof)
                                                    folds)]
                       [accum* (thunk (cdrmap (λ (oracle) (oracle))
                                              accum*-list))]
                       [final-accum
                        (for/fold ([accum (constrain-init folds tree (accum*))])
                                  ([child (assoc-lookup children child-name)])
                          (constrain-step actions
                                          child-name
                                          tree
                                          child
                                          accum
                                          (accum*)))])
            ; assert equalities of final accumulator values and attributes on
            ; unindexed nodes
            (for ([binding final-accum])
              (match-let* ([(cons (cons object label) value) binding]
                           [dep (ftl-ir-dependency object 'none label)])
                (unless (eq? object child-name)
                  (assert (eq? (ftl-tree-load tree (void) null null dep)
                               value)))))))
    ; recurse into children
    (for ([subtrees (ftl-tree-children tree)])
      (for-each recurse (listify (cdr subtrees)))))))

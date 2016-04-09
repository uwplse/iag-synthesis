#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Angelic Evaluation (Interpretation)

(require "../utility.rkt"
         "parse.rkt"
         "translate.rkt"
         "runtime.rkt"
         "derivation.rkt")

(provide ftl-angelic-interpret
         ftl-angelic-interpret-file
         ftl-angelic-evaluate)

; TODO: symbolically lift angelic evaluator

; interpret : L(G_FTL) * L([[L(G_FTL)]]) -> L([[L(G_FTL)]])
(define (ftl-angelic-interpret runtime ftl-port tree)
  (current-bitwidth 6) ; consider setting to #f in some instances
  (ftl-angelic-evaluate runtime
                        (ftl-ir-translate (parse-ftl ftl-port) runtime)
                        tree))

(define (ftl-angelic-interpret-file filename)
  (let* ([ftl-port (open-input-file filename #:mode 'text)]
         [example example-large-deriv]
         [solved (ftl-angelic-interpret ftl-base-runtime ftl-port example)])
    (close-input-port ftl-port)
    solved))

(define (ftl-angelic-interpret-example)
  (ftl-angelic-interpret-file "../examples/points.ftl"))

; fully annotate the given derivation of the given grammar by angelic evaluation
(define (ftl-angelic-evaluate runtime grammar derivation)
  ; let all output attribute values be symbolic
  (ftl-tree-symbolize! runtime grammar derivation)
  ; assert the actions' assignments in whatever order
  (ftl-angelic-constrain runtime grammar derivation)
  ; request that Rosette solve for the attributes' concrete values
  (define solution (solve #t)) ; putting constraints here hides error details
  (displayln "solution: ")
  (displayln solution)
  ; substitute the solved concrete values in for the symbolic attribute values
  (evaluate derivation solution))

; constrain the symbolic values of a loop's initial accumulator
(define (ftl-angelic-constrain-init actions self current)
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
(define (ftl-angelic-constrain-step actions child self indexed previous current)
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
(define (ftl-angelic-constrain runtime grammar tree) ; constrain angelic values
  (let* ([recurse (curry ftl-angelic-constrain runtime grammar)]
         [symbol (ftl-tree-symbol tree)]
         [option (ftl-tree-option tree)]
         [children (ftl-tree-children tree)]
         [production (assoc-lookup (assoc-lookup grammar symbol) option)]
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
                                [production (car (assoc-lookup grammar symbol))]
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
                        (for/fold ([accum (ftl-angelic-constrain-init folds
                                                                      tree
                                                                      (accum*))])
                                  ([child (assoc-lookup children child-name)])
                          (ftl-angelic-constrain-step actions
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

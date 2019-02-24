#lang rosette

; Automatic generation of attribute grammars and attribute trees.

(require rosette/lib/angelic
         "grammar/syntax.rkt"
         "utility.rkt"
         "symbolic-set.rkt"
         "tree.rkt")

(provide annotate-tree
         synthesize-forest)

; Construct a symbolic tree "skeleton" that lacks attributes.
(define (structure-tree class-ast class-instances depth width)
  (assert (>= depth 0))
  (tree (ag-class-name class-ast)
        #f ; no attributes for now
        (map (λ (child-ast)
               (let* ([label (ag-child-name child-ast)]
                      [iface (ag-child-interface child-ast)]
                      [classes (cdr (assq iface class-instances))]
                      [node* (thunk*
                              (structure-tree (apply choose* classes)
                                              class-instances
                                              (- depth 1)
                                              width))])
                 (cons label
                       (if (ag-child-sequence child-ast)
                           (build-list width node*)
                           (node*)))))
             (ag-class-children class-ast))))

; Annotate the (now concrete) tree with symbolic input attributes.
(define (annotate-tree grammar node)
  (let ([class-ast (get-class grammar (tree-class node))]
        [recurse (curry annotate-tree grammar)])
    (tree (tree-class node)
          (for/list ([label-ast (get-labels grammar class-ast)])
            (let ([label (ag-label-name label-ast)]
                  [input (ag-label-input label-ast)]
                  [type (ag-label-type label-ast)])
              (define-symbolic* value (cond
                                        [(eq? type 'int) integer?]
                                        [(eq? type 'bool) boolean?]))
              (cons label (box (if input value (void))))))
          (for/list ([child (tree-children node)])
            (cons (car child)
                  (if (list? (cdr child))
                      (map recurse (cdr child))
                      (recurse (cdr child))))))))

; Update the symbolic set of parent-child class interactions with those found in
; the given tree. The size of the update set is returned.
(define (update-interactions! grammar tree interactions)
  (define (recur tree)
    (let ([parent-class (tree-class tree)]
          [children (tree-children tree)])
      (for-each (λ (child)
                  (let ([child-label (car child)]
                        [child-nodes (listify (cdr child))])
                    (for-each (λ (child-node)
                                (let* ([child-class (tree-class child-node)]
                                       [interaction (list parent-class
                                                          child-label
                                                          child-class)])
                                  (symbolic-set-insert! interactions interaction)
                                  (recur child-node)))
                              child-nodes)))
                children)))

  (recur tree)

  (symbolic-set-size interactions))

; Create an initially empty symbolic set for parent-child class interactions.
(define (class-interactions class-list class-instances)
  (apply make-symbolic-set
         (for*/list ([parent-class class-list]
                     [child-ast (ag-class-children parent-class)]
                     [child-class (cdr (assq (ag-child-interface child-ast)
                                             class-instances))])
           (list (ag-class-name parent-class)
                 (ag-child-name child-ast)
                 (ag-class-name child-class)))))

; Create a list associating interface names to its class instances.
(define (class-instances grammar)
  (map (λ (class-ast-list)
         (cons (ag-class-interface (first class-ast-list)) class-ast-list))
       (group-by ag-class-interface grammar eq?)))

; Synthesize a list of attribute trees for the given attribute grammar for which
; correctness generalizes to the entire domain. If successful, the returned trees
; will include all possible parent-child class interactions.
(define (synthesize-forest grammar [init-depth 1] [width 3] #:annotate [annotate #f])
  (let* ([instances (class-instances grammar)]
         [interactions (class-interactions grammar instances)]
         [total-coverage (length (symbolic-set-universe interactions))]
         [root-class (get-class grammar (ag-grammar-root grammar))]
         [init-sketch (structure-tree root-class instances init-depth width)])

    (define (recur depth forest coverage0 sketch)
      (let* ([coverage (update-interactions! grammar sketch interactions)]
             [model (solve (assert (> coverage coverage0)))])
        (printf "forest size: ~a\ntree depth: ~a\ndomain coverage: ~a\nsatisfiable: ~a\n\n"
                (length forest) depth coverage0 (sat? model))
        (if (unsat? model)
            (let* ([depth (+ depth 1)]
                   [sketch (structure-tree root-class instances depth width)])
              (recur depth forest coverage0 sketch))
            (let* ([tree (evaluate sketch model)]
                   [coverage (evaluate coverage model)]
                   [forest (cons tree forest)])
              (set! interactions (evaluate interactions model))
              (if (= coverage total-coverage)
                  forest  ; the forest is sufficiently diverse
                  (recur depth forest coverage sketch))))))

    (define forest
      (for/list ([tree (recur init-depth null 0 init-sketch)])
        (when annotate
          (annotate-tree grammar tree))
        tree))

    (clear-asserts!)

    forest))

; Prune later trees that don't increase the domain coverage offered by earlier
; trees. NOTE: This actually couldn't prune any trees for both HVBox and Treemap.
(define (prune-forest grammar forest interactions [coverage0 0] [index 0])
  (if (null? forest)
      null
      (let* ([tree (car forest)]
             [coverage (update-interactions! grammar tree interactions)])
        (if (> coverage coverage0)
            (cons tree
                  (prune-forest grammar (cdr forest) interactions coverage (+ index 1)))
            (begin
              (printf "pruned tree ~a\n" index)
              (prune-forest grammar (cdr forest) interactions coverage (+ index 1)))))))

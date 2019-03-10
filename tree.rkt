#lang rosette

; Tree Data Structure

(require xml
         "utility.rkt"
         "grammar/syntax.rkt")

(provide (struct-out tree)
         tree-copy
         xml->tree
         xexpr->tree
         tree-object
         tree-load
         tree-annotate
         tree-validate
         tree-gather
         tree-examples)

(struct tree (class [fields #:mutable] children) #:transparent)

(define (tree-copy node #:rebox? [rebox? #f])
  (define rebox
    (if rebox? (compose box unbox) identity))

  (define fields
    (for/list ([field (tree-fields node)])
      (let ([label (car field)]
            [value (cdr field)])
        (cons label (rebox value)))))

  (define children
    (for/list ([child (tree-children node)])
      (let ([name (car child)]
            [nodes (cdr child)])
        (cons name
              (if (list? nodes)
                  (map tree-copy nodes)
                  (tree-copy nodes #:rebox? rebox?))))))

  (tree (tree-class node) fields children))

; derive a quoted instance of an FTL tree structure for a grammar in IR form
; given an XML string
(define (xml->tree grammar xml-string)
  (xexpr->tree grammar (string->xexpr xml-string)))

; Convert the X-expression tree into the internal tree representation. Tags are
; interpreted as classnames, and attribute values are parsed as Racket-serialized
; values (i.e., parsed with Racket's read). Children are assigned to the child
; labels in the order given by the grammar, with a sequence child consuming all
; remaining child X-expressions. Therefore, this procedure only supports
; attribute grammars whose classes have at most one sequence child as their last-
; declared child.
(define (xexpr->tree grammar xexpr #:boxed? [boxed? #f])
  (match xexpr
    [(list-rest classname
                (and xattributes (list (list (? symbol?) (? string?))...))
                xcontent)
     (let* ([child-xexpr-list (filter list? xcontent)] ; filter out text
            [child-tree-list (map (curry xexpr->tree grammar #:boxed? boxed?) child-xexpr-list)]
            [class-ast (get-class grammar classname)]
            [child-ast-list (ag-class-children class-ast)]
            [label-ast-list (get-labels grammar class-ast)]
            [read-value (compose read open-input-string)])

       (define-values (_ children)
         (for/fold ([input child-tree-list]
                    [output null])
                   ([child-ast child-ast-list])
           (let ([label (ag-child-name child-ast)])
             (if (ag-child-sequence child-ast)
                 (values null
                         (cons (cons label input) output))
                 (values (rest input)
                         (cons (cons label (first input)) output))))))

       (define attributes
         (if boxed?
             (for/list ([label-ast label-ast-list])
               (cons (ag-label-name label-ast) (box (void))))
             null))

       (for ([xattribute xattributes])
         (match-let ([(list label value) xattribute])
           (if boxed?
               (set-box! (cdr (assoc label attributes)) (read-value value))
               (set! attributes (cons (cons label (read-value value)) attributes)))))

       (tree classname attributes children))]

    [(list-rest classname children)
     (xexpr->tree grammar (list classname null children))]

    [_
     (raise-arguments-error 'xexpr->tree
                            "could not convert X-expression to tree"
                            'xexpr xexpr)]))

; Find the object (self, child node, or child node sequence) with the given name.
(define (tree-object self object)
  (if (eq? object 'self)
      self
      (cdr (assoc object (tree-children self)))))

; Select the appropriate store for the object and index.
(define (tree-load self reference lookup child-seq previous current virtual)
  (let ([object (ag-expr-reference-object reference)]
        [index (ag-expr-reference-index reference)]
        [label (ag-expr-reference-label reference)])
    (cond
      [(equal? index 'previous)
       (lookup previous (cons object label))]
      [(and (equal? object child-seq) (or (equal? index 'current) (not index)))
       (lookup (tree-fields current) label)]
      [(equal? index 'current)
       ;(lookup current label)
       (lookup virtual (cons object label))
       ]
      [(not index)
       (lookup (tree-fields (tree-object self object)) label)]
      [(equal? index 'first)
       (lookup (tree-fields (first (tree-object self object))) label)]
      [(equal? index 'last)
       (lookup (tree-fields (last (tree-object self object))) label)])))

; Annotate the tree with attribute information.
(define (tree-annotate grammar node emp new upd)
  (let ([class-ast (get-class grammar (tree-class node))]
        [recurse (λ (node) (tree-annotate grammar node emp new upd))])
    (tree (tree-class node)
          (for/fold ([record (emp)])
                    ([label-ast (get-labels grammar class-ast)])
            (match-let* ([(ag-label input label type) label-ast]
                         [record (new record label)])
              (if input
                  (upd record label type)
                  record)))
          (for/list ([child (tree-children node)])
            (cons (car child)
                  (if (list? (cdr child))
                      (map recurse (cdr child))
                      (recurse (cdr child))))))))

; Validate some property of every output attribute value.
(define (tree-validate grammar tree validate)
  (let ([class-ast (get-class grammar (tree-class tree))]
        [store (tree-fields tree)])
    (for ([label-ast (get-labels grammar class-ast)]
          #:unless (ag-label-input label-ast))
      (validate store (ag-label-name label-ast)))
    (for* ([child (tree-children tree)]
           [node (listify (cdr child))])
      (tree-validate grammar node validate))))

; Collect all the nodes in the tree into a post-order list.
(define (tree-gather tree [nodes null])
  (for*/fold ([nodes (cons tree nodes)])
             ([child (tree-children tree)]
              [node (listify (cdr child))])
    (tree-gather node nodes)))

; Count the nodes in the tree.
(define (tree-size tree)
  (+ (for*/sum ([child (tree-children tree)]
                [node (listify (cdr child))])
       (tree-size node))
     1))

; Return a set of example tree skeletons that include every parent-child class
; pairing permitted by the grammar.
(define (tree-examples grammar)
  (define instances
    (associate-by ag-class-interface identity (ag-grammar-classes grammar)))

  (define (find-leaf-instance iface)
    (let ([class-ast-list (cdr (assoc iface instances))])
      (findf (compose null? ag-class-children) class-ast-list)))

  (define interactions
    (for*/list ([parent-class-ast (ag-grammar-classes grammar)]
                [child-ast (ag-class-children parent-class-ast)])
      (cons (cons parent-class-ast (ag-child-name child-ast))
            (list->mutable-set (cdr (assoc (ag-child-interface child-ast) instances))))))

  (define (construct class-ast)
    (define children-list
      (match (ag-class-children class-ast)
        [(list) (list null)]
        [(list (ag-child names #t ifaces) ...)
         (list
          (for/list ([name names]
                     [iface ifaces])
            (let ([class-ast-set (cdr (assoc (cons class-ast name) interactions))])
              (if (set-empty? class-ast-set)
                  (let ([leaf-class (find-leaf-instance iface)])
                    (cons name (construct leaf-class)))
                  (let ([class-ast-list (set->list class-ast-set)])
                    (set-clear! class-ast-set)
                    (cons name (append-map construct class-ast-list)))))))]
        [(list (ag-child name #f iface))
         (let ([class-ast-set (cdr (assoc (cons class-ast name) interactions))])
           (if (set-empty? class-ast-set)
               (let ([leaf-class (find-leaf-instance iface)])
                 (map (curry cons name) (construct leaf-class)))
               (for*/list ([class-ast class-ast-set]
                           #:when (set-member? class-ast-set class-ast)
                           [subtree (construct class-ast)])
                 (list (cons name subtree)))))]))

    (for/list ([children children-list])
      (tree (ag-class-name class-ast) #f children)))

  (append-map construct (cdr (assoc (ag-grammar-root grammar) instances))))

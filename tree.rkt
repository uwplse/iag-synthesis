#lang rosette

; Tree Data Structure

(require racket/dict
         xml
         "utility.rkt"
         "grammar/syntax.rkt")

(provide (struct-out tree)
         tree-copy
         xml->tree
         xexpr->tree
         file->tree
         tree-bind
         tree-annotate
         tree-validate
         tree-gather
         tree-examples)

; TODO: Should really represent contents with a prefix tree.
(struct tree (class [fields #:mutable] children) #:transparent)

(define (tree-copy node #:rebox? [rebox? #f])
  (define rebox
    (if rebox? (compose box unbox) identity))

  (define fields
    (for/list ([(label value) (in-dict (tree-fields node))])
      (cons label (rebox value))))

  (define children
    (for/list ([(name subtree) (in-dict (tree-children node))])
      (cons name
            (if (list? subtree)
                (map (λ (t) (tree-copy t #:rebox? rebox?)) subtree)
                (tree-copy subtree #:rebox? rebox?)))))

  (tree (tree-class node) fields children))

; derive a quoted instance of an FTL tree structure for a grammar in IR form
; given an XML string
(define (xml->tree G xml-string)
  (xexpr->tree G (string->xexpr xml-string)))

; Convert the X-expression tree into the internal tree representation. Tags are
; interpreted as classnames, and attribute values are parsed as Racket-serialized
; values (i.e., parsed with Racket's read). Children are assigned to the child
; labels in the order given by the grammar, with a sequence child consuming all
; remaining child X-expressions. Therefore, this procedure only supports
; attribute grammars whose classes have at most one sequence child as their last-
; declared child.
(define (xexpr->tree G xexpr #:boxed? [boxed? #f])
  (let ([recur (curry xexpr->tree G #:boxed? boxed?)])
    (match xexpr
      [(list-rest class-name
                  (and xattributes (list (list (? symbol?) (? string?)) ...))
                  xcontent)
       (define children
         (let*-values ([(subtrees) (map recur (filter list? xcontent))]
                       [(child-nodes child-sequences)
                        (get-class-children G class-name)]
                       [(singles sequence)
                        (split-at subtrees (length child-nodes))])
           (append (for/list ([(child-name _) (in-dict child-nodes)]
                              [subtree singles])
                     (cons child-name subtree))
                   (match child-sequences
                     [null null]
                     [(list name) (cons name sequence)]
                     [_
                      (raise-user-error 'xexpr->tre
                                        "class ~a has multiple child sequences"
                                        class-name)]))))

       (define attributes
         (if boxed?
             (for/list ([attribute (get-class-attributes G class-name)])
               (cons (attribute (box (void)))))
             null))

       (for ([(label value) (in-dict xattributes)])
         (let ([value (read (open-input-string (first value)))])
           (if boxed?
               (set-box! (cdr (assoc label attributes)) value)
               (set! attributes (cons (cons label value) attributes)))))

       (tree class-name attributes children)]
    
      [(list-rest classname children)
       (recur (list classname null children))]

      [_
       (raise-arguments-error 'xexpr->tree
                              "could not convert X-expression to tree"
                              'xexpr xexpr)])))

(define (file->tree G filename)
  (xml->tree G (file->string filename #:mode 'text)))

(define/match (search lst p)
  [((cons (cons q v) lst) p)
   (let ([r (shift p q)])
     (if r (cons r v) (search lst p)))]
  [(null _)
   #f])
  
; Select the appropriate store for the object and index.
(define (tree-bind self proc path)
  (match (search (tree-children self) path)
    [(cons rest subtree)
     (proc (tree-fields subtree) `(self . ,rest))]
    [#f
     (proc (tree-fields self) path)]))

; Annotate the tree with attribute information.
(define (tree-annotate G tree emp new upd)
  (let* ([inputs (get-class-inputs G (tree-class tree))]
         [outputs (get-class-outputs G (tree-class tree))]
         [store (foldl (λ (l s) (new s l)) (emp) (append inputs outputs))])
    (for-each (curry upd store) inputs)
    (tree (tree-class tree)
          store
          (for/list ([(child subtree) (in-dict (tree-children tree))])
            (cons child
                  (if (list? subtree)
                      (for ([node subtree])
                        (tree-annotate G node emp new upd))
                      (tree-annotate G subtree emp new upd)))))))

; Validate some property of every output attribute value.
(define (tree-validate G tree validate)
  (for ([label (get-class-outputs G (tree-class tree))])
    (validate (tree-fields tree) label))
  (for* ([(_ subtree) (in-dict (tree-children tree))]
         [node (listify subtree)])
    (tree-validate G node validate)))

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

(define (set-remove? st v)
  (begin0
    (set-member? st v)
    (set-remove! st v)))

(define (build-leaves G)
  (for/list ([(iface-name class-list) (in-dict (associate-classes G))])
    (cons iface-name
          ; TODO: Could support interior-only node classes too
          (for/list ([(class-name class-body) (in-dict class-list)]
                     #:when (ag-class-leaf? class-body))
            (tree class-name #f null)))))

(define (build-children G)
  (let ([make-leaf (curry lookup (build-leaves G))])
    (for/list ([(class-name class-body) (in-dict (ag-grammar-classes G))])
      (let ([singletons
             (map (on-cdr make-leaf)
                  (ag-class-singletons class-body))]
            [sequences
             (map (on-cdr (compose list make-leaf))
                  (ag-class-sequences class-body))])
        (cons class-name (tree class-name #f (append singletons sequences)))))))

; Return a set of example tree skeletons that include every parent-child class
; pairing permitted by the grammar.
(define (tree-examples G root)
  (define instances (associate-classes G))

  (define leaf-instances
    (for/list ([(iface-name class-list) (in-dict instances)])
      (cons iface-name
            (for/list ([(class-name class-body) (in-dict class-list)]
                       #:when (ag-class-leaf? class-body))
              (cons class-name class-body)))))

  (define memo (make-hash))

  (define pending (list->mutable-set (map car (ag-grammar-classes G))))

  (define (get-child-classes class-name child-name child-kind)
    (lookup (if (set-remove? pending (cons class-name child-name))
                instances
                leaf-instances)
            child-kind))

  (define child-nodes (build-children G))

;  (define (build iface-name)
;    (for/list ([(class-name class-body) (lookup instances iface-name)])
;      (if (hash-has-key? memo class-name)
;          (hash-ref memo class-name)
;          (begin
;            (hash-set! memo class-name (lookup child-nodes class-name))
;            (tree class-name
;                  #f
;                  ??)))))

  (define/match (construct class)
    [((cons class-name class-body))
     (define singleton-children
       (for/list ([(child-name child-kind) (in-dict (ag-class-singletons class-body))])
         (map (compose (curry cons child-name) construct)
              (get-child-classes class-name child-name child-kind))))

     (define sequence-children
       (for/list ([(child-name child-kind) (in-dict (ag-class-sequences class-body))])
         (let ([child-classes (get-child-classes class-name child-name child-kind)])
           (list (cons child-name (append-map construct child-classes))))))

     (for/list ([subtree-list
                 (apply cartesian-product (append singleton-children sequence-children))])
       (tree class-name #f subtree-list))])

  (append-map construct (lookup instances root))

  )

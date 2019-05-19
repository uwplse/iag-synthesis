#lang rosette

; Tree Data Structure

(require racket/dict
         (prefix-in xml: xml)
         "utility.rkt"
         "grammar/syntax.rkt")

(provide (struct-out tree)
         tree-copy
         xml->tree
         xexpr->tree
         file->tree
         tree-index
         tree-annotate
         tree-validate
         tree-gather
         tree-examples)

; TODO: Maybe represent fields with a prefix tree.
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
  (xexpr->tree G (xml:string->xexpr xml-string)))

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
         (let ([subtrees (map recur (filter list? xcontent))])
           (match (class-children G class-name)
             [(list (child1 child-names _) ...)
              (map cons child-names subtrees)]
             [(list (or (child* child-name _) (child+ child-name _)))
              (cons child-name subtrees)])))

       (define fields
         (if boxed?
             (for/list ([attr (class-attributes G class-name)])
               (cons (attribute-name attr) (box (void))))
             null))

       (for ([(label value) (in-dict xattributes)])
         (let ([value (read (open-input-string (first value)))])
           (if boxed?
               (set-box! (lookup fields label) value)
               (set! fields (cons (cons label value) fields)))))

       (tree class-name fields children)]
    
      [(list-rest class-name children)
       (recur (list class-name null children))]

      [_
       (raise-arguments-error 'xexpr->tree
                              "could not convert X-expression to tree"
                              'xexpr xexpr)])))

(define (file->tree G filename)
  (xml->tree G (file->string filename #:mode 'text)))

;(define/match (search lst p)
;  [((cons (cons q v) lst) p)
;   (let ([r (shift p q)])
;     (if r (cons r v) (search lst p)))]
;  [(null _)
;   #f])

;(define (tree-bind self proc path)
;  (match (search (tree-children self) path)
;    [(cons rest subtree)
;     (proc (tree-fields subtree) `(self . ,rest))]
;    [#f
;     (proc (tree-fields self) path)]))

; Select the field store indexed by the object reference.
(define (tree-index self object #:current [curr #f] #:virtual [virt #f]
                    #:predecessor [pred #f] #:successor [succ #f])
  (define subtree (curry lookup (tree-children self)))
  (match object
    [(object1 'self) (tree-fields self)]
    [(object1 child) (tree-fields (subtree child))]
    [(or (object$- 'self) (object$+ 'self)) virt]
    [(or (object$- child) (object$+ child)) #:when (tree? (subtree child)) virt]
    [(object$0 child) (tree-fields (first (subtree child)))]
    [(object$- child) (tree-fields pred)]
    [(object$i child) (tree-fields curr)]
    [(object$+ child) (tree-fields succ)]
    [(object$$ child) (tree-fields (last (subtree child)))]))

; Annotate the tree with attribute information.
(define (tree-annotate G tree emp new upd)
  (define store
    (for/fold ([store (emp)])
              ([attr (class-attributes G (tree-class tree))])
      (let* ([label (attribute-name attr)]
             [store (new store label)])
        (when (input? attr)
          (upd store label))
        store)))
  (define children
    (for/list ([(child subtree) (in-dict (tree-children tree))])
      (cons child
            (if (list? subtree)
                (for/list ([node subtree])
                  (tree-annotate G node emp new upd))
                (tree-annotate G subtree emp new upd)))))
  (tree (tree-class tree) store children))

; Validate some property of every output attribute value.
(define (tree-validate G tree validate)
  (for ([attr (class-attributes G (tree-class tree))])
    (validate (tree-fields tree) (attribute-name attr)))
  (for ([(name subtree) (in-dict (tree-children tree))])
    (if (list? subtree)
        (for ([node subtree])
          (tree-validate G node validate))
        (tree-validate G subtree validate))))

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

(define (build-leaves G)
  (for/list ([(iface-name class-list) (in-dict (associate-classes G))])
    (cons iface-name
          ; TODO: What if every class of an interface is interior-only?
          (for/fold ([leaf-nodes null])
                    ([(class-name class-body) (in-dict class-list)])
            (match (ag-class-children class-body)
              [(list (child* names _) ...)
               (cons (tree class-name #f (map list names)))]
              [_
               leaf-nodes])))))

(define (build-children G)
  (define leaf-variants (build-leaves G))
  (for/list ([(iface-name class-decls) (in-dict (associate-classes G))])
    (define class-variants
      (for/list ([(class-name class-body) (in-dict class-decls)])
        (define child-variants
          (for/list ([child-decl (ag-class-children class-body)])
            (match child-decl
              [(child1 name kind)
               (map (curry cons name) (lookup leaf-variants kind))]
              [(or (child* name kind) (child+ name kind))
               (list (cons name (lookup leaf-variants kind)))])))
        (map (curry tree class-name #f)
             (apply cartesian-product child-variants))))
    (cons iface-name (append* class-variants))))

; TODO: Complete this cleaner version of tree-examples
(define (tree-examples* G root)
  (define variants (associate-classes G))

  (define queue
    (for/hasheq ([(iface-name class-decls) (in-dict variants)])
      (let ([class-names (map class-name class-decls)])
        (values iface-name (apply mutable-seteq class-names)))))
  (define (dequeue! iface-name class-name)
    (set-remove! (hash-ref queue iface-name) class-name))
  
  (define forest (make-hasheq))
  (define (plant! iface-name class-name node)
    (hash-update! forest (tree-class node) (curry cons node) null))
  
  (for ([(class-name class-body) (in-dict (ag-grammar-classes G))]
        #:when (andmap child*? (ag-class-children class-body)))
    (let ([iface-name (ag-class-interface class-body)]
          [child-names (map child-name (ag-class-children class-body))])
      (plant! (tree class-name #f (map list child-names)))
      (when (null? child-names)
        (dequeue! iface-name class-name))))
  
  ; until queue empty do:
  ;   find class in queue s.t. children class-saturated in forest:
  ;   construct nodes of class for cartesian product of child classes
  ;   // always pick shortest or tallest?
  ;   // once a node is adopted, there's no sense in duplicating it elsewhere.
  ;   // how can we optimize for many short trees rather than a few very tall trees?
  ;   insert nodes into forest
  ;   remove class from queue
  #f)

; Return a set of example tree skeletons that include every parent-child class
; pairing permitted by the grammar.
(define (tree-examples G root)
  (define variants (build-children G))

  (define/match (construct class)
    [((cons class-name class-body))
     (define children
       (for/list ([child-decl (ag-class-children class-body)])
         (match child-decl
           [(child1 name kind)
            (map (curry cons name) (lookup variants kind))]
           [(or (child* name kind) (child+ name kind))
            (list (cons name (lookup variants kind)))])))
     (map (curry tree class-name #f) (apply cartesian-product children))])

  (append-map construct (lookup (associate-classes G) root)))

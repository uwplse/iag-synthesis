#lang rosette

; Tree Data Structure

(require xml
         "utility.rkt"
         "grammar/syntax.rkt")

(provide (struct-out tree)
         tree-copy
         xml->tree
         xexpr->tree
         make-virtual-node
         tree-object
         tree-load
         tree-read
         tree-write
         tree-check
         tree-inputs
         tree-partition-fields)

(struct tree
  (class
   fields
   children
   ) #:transparent)

(define (tree-copy node)
  (define fields
    (for/list ([field (tree-fields node)])
      (let ([label (car field)]
            [value (unbox (cdr field))])
        (cons label (box value)))))

  (define children
    (for/list ([child (tree-children node)])
      (let ([name (car child)]
            [nodes (cdr child)])
        (cons name
              (if (list? nodes)
                  (map tree-copy nodes)
                  (tree-copy nodes))))))

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
(define (xexpr->tree grammar xexpr)
  (match xexpr
    [(list-rest classname
                (and xattributes (list (list (? symbol?) (? string?))...))
                xcontent)
     (let* ([child-xexpr-list (filter list? xcontent)] ; filter out text
            [child-tree-list (map (curry xexpr->tree grammar) child-xexpr-list)]
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
         (for/list ([label-ast label-ast-list])
           (cons (ag-label-name label-ast) (box (void)))))

       (for ([xattribute xattributes])
         (match-let* ([(list label value) xattribute]
                      [cell (cdr (assq label attributes))])
           (set-box! cell (read-value value))))

       (tree classname attributes children))]

    [(list-rest classname children)
     (xexpr->tree grammar (list classname null children))]

    [_
     (raise-arguments-error 'xexpr->tree
                            "could not convert X-expression to tree"
                            'xexpr xexpr)]))

; Construct a virtual tree node, which is one with only output fields and no
; children. A virtual node is only ever used to store the initial accumulators
; for folds in a loop. (The amount of storage needed is overapproximated by
; simply allocating a field for every output attribute.)
(define (make-virtual-node self object)
  (let ([template (first (tree-object self object))])
    (tree (tree-class template)
          (for/list ([field (tree-fields template)])
            (cons (car field) (box (void))))
          null)))

; Find the object (self, child node, or child node sequence) with the given name.
(define (tree-object node object)
  (if (eq? object 'self)
      node
      (cdr (assq object (tree-children node)))))

; Load an attribute reference from the current node, the indexed node, or the
; previously indexed (virtual) node. The returned value is the attribute's memory
; cell (a box), allowing both reads and writes. The loop argument should indicate
; which child is being looped over, if any (#f otherwise).
(define (tree-load current loop previous indexed reference)
  (let ([object (ag-expr-reference-object reference)]
        [index (ag-expr-reference-index reference)]
        [label (ag-expr-reference-label reference)])
  (cond
    [(and (eq? object 'self) (not index))
     (cdr (assq label (tree-fields current)))]
    [(not index)
     (let ([child (if (eq? object loop)
                      indexed
                      (cdr (assq object (tree-children current))))])
       (cdr (assq label (tree-fields child))))]
    [(eq? index 'previous)
     (unless (eq? object loop)
       (raise-user-error 'interpret
                         "invalid reference to '~a$-.~a' in loop over '~a'"
                         object
                         label
                         loop))
     (cdr (assq label (tree-fields previous)))]
    [(eq? index 'first)
     (let ([child (first (cdr (assq object (tree-children current))))])
       (cdr (assq label (tree-fields child))))]
    [(eq? index 'last)
     (let ([child (last (cdr (assq object (tree-children current))))])
       (cdr (assq label (tree-fields child))))])))

(define (tree-read current loop previous indexed reference)
  (unbox (tree-load current loop previous indexed reference)))

(define (tree-write current loop previous indexed reference value)
  (set-box! (tree-load current loop previous indexed reference) value))

; Assert that every field in the tree is initialized (i.e., not void).
(define (tree-check tree)
  (for ([field (tree-fields tree)])
    (assert (not (void? (unbox (cdr field))))))
  (for* ([child (tree-children tree)]
         [node (listify (cdr child))])
    (tree-check node)))

; Collect every input field in the tree, according to the grammar, appending the
; memory cells to the given list of previously found inputs.
(define (tree-inputs grammar tree [inputs null])
  (let ([labels (get-labels grammar (get-class grammar (tree-class tree)))]
        [fields (tree-fields tree)])
    (define new-inputs
      (for*/fold ([result inputs])
                 ([child (tree-children tree)]
                  [node (listify (cdr child))])
        (tree-inputs grammar node result)))
    (for/fold ([result new-inputs])
              ([label labels]
               #:when (ag-label-input label))
      (let ([input-field (cdr (assq (ag-label-name label) fields))])
        (cons input-field result)))))

; Return two lists: those fields that satisfy the predicate and the rest.
(define (tree-partition-fields predicate tree [positive null] [negative null])
  (define-values (updated-positive updated-negative)
    (for/fold ([positive positive]
               [negative negative])
              ([field (tree-fields tree)])
      (if (predicate (unbox (cdr field)))
          (values (cons (cdr field) positive) negative)
          (values positive (cons (cdr field) negative)))))
  (for*/fold ([positive updated-positive]
              [negative updated-negative])
             ([child (tree-children tree)]
              [node (listify (cdr child))])
    (tree-partition-fields predicate node positive negative)))

#lang rosette

; Functional Tree Language (FTL) intepreter
; Tree Traversals

(require rosette/lib/angelic
         "../grammar/tree.rkt")

(provide ftl-tree-preorder
         ftl-tree-inorder
         ftl-tree-postorder
         ftl-tree-partial-preorder
         ftl-tree-partial-postorder)


; pre-order traversal of a tree
(define (ftl-tree-preorder visit tree)
  (for*/all ([tree tree]
             [children (ftl-tree-children tree)])
    (begin
      (visit tree)
      (for ([child children])
        (if (list? (cdr child))
            (for-each (curry ftl-tree-preorder visit) (cdr child))
            (ftl-tree-preorder visit (cdr child)))))))

; post-order traversal of a tree
(define (ftl-tree-postorder visit tree)
  (for*/all ([tree tree]
             [children (ftl-tree-children tree)])
    (begin
      (for ([child children])
        (if (list? (cdr child))
            (for-each (curry ftl-tree-postorder visit) (cdr child))
            (ftl-tree-postorder visit (cdr child))))
      (visit tree))))

; in-order traversal of a tree
(define (ftl-tree-inorder visit tree)
  (for*/all ([tree tree]
             [children (ftl-tree-children tree)])
    (visit tree)))

; partial pre-order traversal of a tree, returns new affected region
(define (ftl-tree-partial-preorder ascend? descend? depth visit tree)
  ; TODO: actually ascend to parent if necessary and return new root+depth
  ; (when (ascend? tree)
  ;     (visit parent))
  ; pseudocode:
  ;   if parent dirty:
  ;      ascend to parent, expanding affected region
  ;   if depth >= 0:
  ;      incrementally visit self
  ;      for child in children:
  ;          if depth > 0 or child dirty:
  ;             recurse down to child
  ;   return new root and relative depth of affected region
  (when (>= depth 0)
    (visit tree)
    (let ([listify (λ (l) (if (list? l) l (list l)))]
          [depth (- depth 1)]
          [ascend? (λ (_) #f)])
      (for ([child (map listify (ftl-tree-children tree))])
        (ftl-tree-partial-preorder ascend? depth visit child)))))

; partial post-order traversal of a tree, returns new affected region
(define (ftl-tree-partial-postorder ascend? descend? depth visit tree)
  ; TODO: actually ascend to parent if necessary and return new root+depth
  ; pseudocode:
  ;   if depth >= 0:
  ;      incrementally visit self
  ;      for child in children:
  ;          if depth > 0 or child dirty:
  ;             descend to child
  ;   if parent dirty:
  ;      ascend to parent, expanding affected region
  ;   return new root and relative depth of affected region
  (for ([child (ftl-tree-children tree)])
    (if (list? (cdr child))
        (for-each (curry ftl-tree-postorder visit) (cdr child))
        (ftl-tree-postorder visit (cdr child)))))

#lang rosette

(require "../../tree.rkt")

(provide hvbox-skeleton-forest)

; A list of tree skeletons whose correctness is sufficient to genealize to the
; whole domain.
(define hvbox-skeleton-forest
  (list
   (tree 'Root #f (list (cons 'root (tree 'Leaf #f '()))))
   (tree
    'Root
    #f
    (list
     (cons
      'root
      (tree
       'VBox
       #f
       (list
        (list
         'childs
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'root
      (tree
       'HBox
       #f
       (list
        (list
         'childs
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'root
      (tree
       'HBox
       #f
       (list
        (list
         'childs
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())
         (tree
          'HBox
          #f
          (list
           (list
            'childs
            (tree 'Leaf #f '())
            (tree 'Leaf #f '())
            (tree 'Leaf #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'root
      (tree
       'HBox
       #f
       (list
        (list
         'childs
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())
         (tree
          'VBox
          #f
          (list
           (list
            'childs
            (tree 'Leaf #f '())
            (tree 'Leaf #f '())
            (tree 'Leaf #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'root
      (tree
       'VBox
       #f
       (list
        (list
         'childs
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())
         (tree
          'HBox
          #f
          (list
           (list
            'childs
            (tree 'Leaf #f '())
            (tree 'Leaf #f '())
            (tree 'Leaf #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'root
      (tree
       'VBox
       #f
       (list
        (list
         'childs
         (tree
          'VBox
          #f
          (list
           (list
            'childs
            (tree 'Leaf #f '())
            (tree 'Leaf #f '())
            (tree 'Leaf #f '()))))
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())))))))))

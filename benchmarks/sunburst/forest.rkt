#lang rosette

(require "../../tree.rkt")

(provide sunburst-skeleton-forest)

; A list of tree skeletons whose correctness is sufficient to genealize to the
; whole domain.
(define sunburst-skeleton-forest
  (list
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Radial
       #f
       (list
        (list
         'child
         (tree 'Leaf #f '())
         (tree
          'Radial
          #f
          (list
           (list
            'child
            (tree 'Leaf #f '())
            (tree 'Leaf #f '())
            (tree 'Leaf #f '()))))
         (tree 'Leaf #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Radial
       #f
       (list
        (list
         'child
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())
         (tree 'Leaf #f '())))))))
   (tree 'Root #f (list (cons 'child (tree 'Leaf #f '()))))))

#lang rosette

(require "../../tree.rkt")

(provide treemap-skeleton-forest)

; A list of tree skeletons whose correctness is sufficient to genealize to the
; whole domain.
(define treemap-skeleton-forest
  (list
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'District
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'District
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'District
       #f
       (list
        (list
         'childs
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'Region
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'VSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Region
       #f
       (list
        (list
         'childs
         (tree
          'CountryContainer
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'HSquare
       #f
       (list
        (list
         'childs
         (tree
          'VSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'HSquare
       #f
       (list
        (list
         'childs
         (tree
          'Region
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'District
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'VSquare
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'District
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Region
       #f
       (list
        (list
         'childs
         (tree
          'VSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'Region
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'VSquare
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'VSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'District
       #f
       (list
        (list
         'childs
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Region
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'Region
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'VSquare
       #f
       (list
        (list
         'childs
         (tree
          'CountryContainer
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'VSquare
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'Region
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'HSquare
       #f
       (list
        (list
         'childs
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'CountryContainer
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'HSquare
       #f
       (list
        (list
         'childs
         (tree
          'District
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Region
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Region
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree
          'District
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'District
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'CountryContainer
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'CountryContainer
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree
          'Region
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'CountryContainer
       #f
       (list
        (list
         'childs
         (tree
          'CountryContainer
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'District
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))
         (tree
          'VSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'CountryContainer
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree
          'CountryContainer
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'CountryContainer
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree
          'HSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'CountryContainer
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree
          'VSquare
          #f
          (list
           (list
            'childs
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '())
            (tree 'PollingPlace #f '()))))))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'District
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'VSquare
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'HSquare
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'Region
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree
    'Root
    #f
    (list
     (cons
      'child
      (tree
       'CountryContainer
       #f
       (list
        (list
         'childs
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())
         (tree 'PollingPlace #f '())))))))
   (tree 'Root #f (list (cons 'child (tree 'PollingPlace #f '()))))))

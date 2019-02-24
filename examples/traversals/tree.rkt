#lang rosette

(provide (struct-out node)
         (struct-out inner)
         (struct-out leaf)
         evil-tree
         small-tree
         medium-tree
         large-tree)

(struct node (x y z) #:transparent)

(struct inner node (left right) #:transparent)

(struct leaf node () #:transparent)

(define evil-tree ; this tree breaks the assumption that leaf fields are inputs
  (inner (box (void))
         (box (void))
         (box (void))
         (leaf (box (void))
               (box (void))
               (box (void)))
         (leaf (box (void))
               (box (void))
               (box (void)))))

(define small-tree
  (inner (box (void))
         (box (void))
         (box (void))
         (leaf 1 2 3)
         (leaf 4 5 6)))

(define medium-tree
  (inner (box (void))
         (box (void))
         (box (void))
         (inner (box (void))
                (box (void))
                (box (void))
                (leaf 1 2 3)
                (leaf 4 5 6))
         (inner (box (void))
                (box (void))
                (box (void))
                (leaf  7  8  9)
                (leaf 10 11 12))))

(define large-tree
  (inner (box (void))
         (box (void))
         (box (void))
         (inner (box (void))
                (box (void))
                (box (void))
                (inner (box (void))
                       (box (void))
                       (box (void))
                       (leaf 1 2 3)
                       (leaf 4 5 6))
                (inner (box (void))
                       (box (void))
                       (box (void))
                       (leaf  7  8  9)
                       (leaf 10 11 12)))
         (inner (box (void))
                (box (void))
                (box (void))
                (inner (box (void))
                       (box (void))
                       (box (void))
                       (leaf 13 14 15)
                       (leaf 16 17 18))
                (inner (box (void))
                       (box (void))
                       (box (void))
                       (leaf 19 20 21)
                       (leaf 22 23 24)))))


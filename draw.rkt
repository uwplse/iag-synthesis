#lang rosette

(require 2htdp/image
         "core/utility.rkt"
         "core/tree.rkt")

(provide draw
         blank)

(define blank
  (empty-scene 256 128))

(define/match (draw tree scene)
  [((ftl-tree 'Top _ _ (list (cons 'root root))) _)
   (draw root scene)]
  [((ftl-tree 'HVBox class attributes children) _)
   (let ([w (assoc-lookup attributes 'width)]
         [h (assoc-lookup attributes 'height)]
         [r (assoc-lookup attributes 'right)]
         [b (assoc-lookup attributes 'bottom)])
     (if (eq? class 'Leaf)
         (place-image (rectangle w h 'solid 'purple)
                      r
                      b
                      scene)
         (for/fold ([scene (place-image (rectangle w h 'outline 'gold)
                                        r
                                        b
                                        scene)])
                   ([child (cdar children)])
           (draw child scene))))])

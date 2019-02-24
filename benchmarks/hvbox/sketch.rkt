#lang rosette

(require rosette/lib/angelic
         "../../schedule/syntax.rkt")

(provide hvbox-sketch)

(define hvbox-sketch
  (let ([Root-steps '((root . height_out)
                      (root . width_out)
                      (root . bottom)
                      (root . right))]
        [HBox-steps '((childs . height_out)
                      (self . height)
                      (childs . width_out)
                      (self . width)
                      (childs . bottom)
                      (childs . right))]
        [VBox-steps '((childs . height_out)
                      (self . height)
                      (childs . width_out)
                      (self . width)
                      (childs . bottom)
                      (childs . right))]
        [Leaf-steps '((self . height)
                      (self . width))])
  (sched-comp
   'seq
   (sched-trav
    'post
    `((Root)
      (HBox ,(apply choose* HBox-steps)
            ,(apply choose* HBox-steps)
            ,(apply choose* HBox-steps)
            ,(apply choose* HBox-steps))
      (VBox ,(apply choose* VBox-steps)
            ,(apply choose* VBox-steps)
            ,(apply choose* VBox-steps)
            ,(apply choose* VBox-steps))
      (Leaf ,(apply choose* Leaf-steps)
            ,(apply choose* Leaf-steps))))
   (sched-trav
    'pre
    `((Root ,(apply choose* Root-steps)
            ,(apply choose* Root-steps)
            ,(apply choose* Root-steps)
            ,(apply choose* Root-steps))
      (HBox ,(apply choose* HBox-steps)
            ,(apply choose* HBox-steps))
      (VBox ,(apply choose* VBox-steps)
            ,(apply choose* VBox-steps))
      (Leaf))))))

#lang rosette

(require rosette/lib/synthax
         "../core/runtime.rkt"
         "../core/tree.rkt"
         "../compile/parse.rkt"
         "../compile/generate.rkt"
         "../schedule/evaluate.rkt"
         "../angelic/constrain.rkt")

(define (read-grammar filename)
  (let* ([port (open-input-file filename #:mode 'text)]
         [grammar (ftl-ir-translate ftl-base-runtime (parse-ftl port))])
    (close-input-port port)
    grammar))

(define (read-tree grammar sentence filename)
  (xml->ftl-tree ftl-base-runtime
                 grammar
                 sentence
                 (port->string (open-input-file filename #:mode 'text))))

(define hvbox-grammar
  (read-grammar "../examples/hvbox.ftl"))

(define hvbox-tree
  (read-tree hvbox-grammar 'Top "../examples/hvbox.xml"))

(define hvbox-sched
  (ftl-sched-seq
   (ftl-sched-trav 'post
                   `(((Top . Root) . ())
                     ((HVBox . HBox) . (,(ftl-visit-iter 'childs
                                                         '((self . childsWidth)
                                                           (self . childsHeight)))
                                        ,(ftl-visit-eval '((self . width)
                                                           (self . height)))))
                     ((HVBox . VBox) . (,(ftl-visit-iter 'childs
                                                         '((self . childsWidth)
                                                           (self . childsHeight)))
                                        ,(ftl-visit-eval '((self . width)
                                                           (self . height)))))
                     ((HVBox . Leaf) . (,(ftl-visit-eval '((self . width)
                                                           (self . height)))))))
   (ftl-sched-trav 'pre
                   `(((Top . Root) . (,(ftl-visit-eval '((root . right)
                                                         (root . bottom)))))
                     ((HVBox . HBox) . (,(ftl-visit-iter 'childs
                                                         '((childs . right)
                                                           (childs . bottom)))))
                     ((HVBox . VBox) . (,(ftl-visit-iter 'childs
                                                         '((childs . right)
                                                           (childs . bottom)))))
                     ((HVBox . Leaf) . ())))))


(define hvbox-sched-sketch
  (ftl-sched-seq
   (ftl-sched-trav (choose 'pre 'post)
                   `(((Top . Root) . ())
                     ((HVBox . HBox) . (,(ftl-visit-iter 'childs
                                                         '((self . childsWidth)
                                                           (self . childsHeight)))
                                        ,(ftl-visit-eval '((self . width)
                                                           (self . height)))))
                     ((HVBox . VBox) . (,(ftl-visit-iter 'childs
                                                         '((self . childsWidth)
                                                           (self . childsHeight)))
                                        ,(ftl-visit-eval '((self . width)
                                                           (self . height)))))
                     ((HVBox . Leaf) . (,(ftl-visit-eval '((self . width)
                                                           (self . height)))))))
   (ftl-sched-trav (choose 'pre 'post)
                   `(((Top . Root) . (,(ftl-visit-eval '((root . right)
                                                         (root . bottom)))))
                     ((HVBox . HBox) . (,(ftl-visit-iter 'childs
                                                         '((childs . right)
                                                           (childs . bottom)))))
                     ((HVBox . VBox) . (,(ftl-visit-iter 'childs
                                                         '((childs . right)
                                                           (childs . bottom)))))
                     ((HVBox . Leaf) . ())))))


(define points-grammar
  (read-grammar "../examples/points.ftl"))

(define points-tree
  (read-tree points-grammar 'Root "../examples/points.xml"))

(define points-sched
  (ftl-sched-seq
   (ftl-sched-trav 'pre
                   `(((Root . Origin) . (,(ftl-visit-iter 'p '((p . bx)
                                                               (p . by)))))
                     ((Point . Relative) . (,(ftl-visit-eval '((self . x)
                                                               (self . y)))
                                            ,(ftl-visit-iter 'p '((p . bx)
                                                                  (p . by)))))
                     ((Point . Fixed) . (,(ftl-visit-eval '((self . x)
                                                            (self . dx)
                                                            (self . y)
                                                            (self . dy)))
                                         ,(ftl-visit-iter 'p '((p . bx)
                                                               (p . by)))))
                     ((Point . Endpoint) . (,(ftl-visit-eval '((self . x)
                                                               (self . y)))))))
   (ftl-sched-trav 'pre
                   `(((Root . Origin) . (,(ftl-visit-iter 'p '((self . w)
                                                               (self . h)))))
                     ((Point . Relative) . (,(ftl-visit-iter 'p '((self . w)
                                                                  (self . h)))))
                     ((Point . Fixed) . (,(ftl-visit-iter 'p '((self . w)
                                                               (self . h)))))
                     ((Point . Endpoint) . ())))))

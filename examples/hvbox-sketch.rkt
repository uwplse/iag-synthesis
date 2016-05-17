#lang rosette

(require rosette/lib/angelic
         "../schedule/syntax.rkt")

(provide hvbox-sketch)


(define hvbox-sketch
  (let ([root-steps
         (list (ftl-step-eval '(root . right))
               (ftl-step-eval '(root . bottom)))]
        [hvbox-steps
         (list (ftl-step-eval '(self . width))
               (ftl-step-eval '(self . height)))])
    (ftl-sched-seq
     (ftl-sched-trav 'post
                     `(((Top . Root))

                       ((HVBox . HBox)
                        ,(ftl-step-iter (list (ftl-step-eval '(self . childsWidth))
                                              (ftl-step-eval '(self . childsHeight))))
                        ,(apply choose* hvbox-steps)
                        ,(apply choose* hvbox-steps))

                       ((HVBox . VBox)
                        ,(ftl-step-iter (list (ftl-step-eval '(self . childsWidth))
                                              (ftl-step-eval '(self . childsHeight))))
                        ,(apply choose* hvbox-steps)
                        ,(apply choose* hvbox-steps))

                       ((HVBox . Leaf)
                        ,(apply choose* hvbox-steps)
                        ,(apply choose* hvbox-steps))))
     (ftl-sched-trav 'pre
                     `(((Top . Root)
                        ,(apply choose* root-steps)
                        ,(apply choose* root-steps))

                       ((HVBox . HBox)
                        ,(choose* (ftl-step-iter (list (ftl-step-eval '(childs . right))
                                                       (ftl-step-eval '(childs . bottom))))
                                  (ftl-step-iter (list (ftl-step-eval '(childs . bottom))
                                                       (ftl-step-eval '(childs . right))))))

                       ((HVBox . VBox)
                        ,(ftl-step-iter (list (choose* (void)
                                                       (ftl-step-eval '(childs . right))
                                                       (ftl-step-eval '(childs . bottom)))
                                              (choose* (void)
;                                                       (ftl-step-eval '(childs . right))
                                                       (ftl-step-eval '(childs . bottom))))))

                       ((HVBox . Leaf)))))))

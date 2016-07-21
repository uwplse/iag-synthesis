#lang rosette

(require rosette/lib/angelic
         "../schedule/syntax.rkt")

(provide treemap-sketch)

(define (choose-n n xs)
  (build-list n (thunk* (apply choose* xs))))

(define treemap-sketch
  (let* ([loopify (compose ftl-step-iter list ftl-step-eval)]
         [root-steps (map ftl-step-eval '((self . totalMag)
                                          (self . votesUR)
                                          (childs . w)
                                          (childs . h)
                                          (childs . rx)
                                          (childs . by)
                                          (childs . minTunrout)
                                          (childs . maxTurnout)
                                          (childs . showFraud)
                                          (childs . showProjected)
                                          (childs . showJavascript)
                                          (childs . fixWidth)))]
         [node-steps (append (map ftl-step-eval '((self . x)
                                                  (self . y)
                                                  (self . render)))
                             (map loopify '((self . totalMag)
                                            (self . votesUR)
                                            (childs . w)
                                            (childs . h)
                                            (childs . rx)
                                            (childs . by)
                                            (childs . minTurnout)
                                            (childs . maxTurnout)
                                            (childs . showFraud)
                                            (childs . showProjected)
                                            (childs . showJavascript)
                                            (childs . fixWidth))))]
         [leaf-steps (map ftl-step-eval '((self . calcProjectedColor)
                                          (self . calcRegularColor)
                                          (self . calcVotesColor)
                                          (self . calcFraudColor)
                                          (self . render)
                                          (self . x)
                                          (self . y)
                                          (self . magnitude)
                                          (self . totalMag)
                                          (self . votesUR)))])
    (ftl-sched-seq
     (ftl-sched-trav 'pre
                     `(((IRoot . Root) . ,(choose-n 8 root-steps))
                       ((Node . CountryContainer) . ,(choose-n 6 node-steps))
                       ((Node . Region) . ,(choose-n 6 node-steps))
                       ((Node . District) . ,(choose-n 6 node-steps))
                       ((Node . VSquare) . ,(choose-n 6 node-steps))
                       ((Node . HSquare) . ,(choose-n 6 node-steps))
                       ((Node . PollingPlace) . ,(choose-n 7 leaf-steps))))
     (ftl-sched-seq
      (ftl-sched-trav 'post
                      `(((IRoot . Root) . ,(choose-n 4 root-steps))
                        ((Node . CountryContainer) . ,(choose-n 2 node-steps))
                        ((Node . Region) . ,(choose-n 2 node-steps))
                        ((Node . District) . ,(choose-n 2 node-steps))
                        ((Node . VSquare) . ,(choose-n 2 node-steps))
                        ((Node . HSquare) . ,(choose-n 2 node-steps))
                        ((Node . PollingPlace) . ,(choose-n 0 leaf-steps))))
      (ftl-sched-seq
       (ftl-sched-trav 'pre
                       `(((IRoot . Root) . ,(choose-n 0 root-steps))
                         ((Node . CountryContainer) . ,(choose-n 7 node-steps))
                         ((Node . Region) . ,(choose-n 7 node-steps))
                         ((Node . District) . ,(choose-n 7 node-steps))
                         ((Node . VSquare) . ,(choose-n 7 node-steps))
                         ((Node . HSquare) . ,(choose-n 7 node-steps))
                         ((Node . PollingPlace) . ,(choose-n 2 leaf-steps))))
       (ftl-sched-seq
        (ftl-sched-trav 'post
                        `(((IRoot . Root) . ,(choose-n 0 root-steps))
                          ((Node . CountryContainer) . ,(choose-n 0 node-steps))
                          ((Node . Region) . ,(choose-n 0 node-steps))
                          ((Node . District) . ,(choose-n 0 node-steps))
                          ((Node . VSquare) . ,(choose-n 0 node-steps))
                          ((Node . HSquare) . ,(choose-n 0 node-steps))
                          ((Node . PollingPlace) . ,(choose-n 0 leaf-steps))))
        (ftl-sched-trav 'pre
                        `(((IRoot . Root) . ,(choose-n 0 root-steps))
                          ((Node . CountryContainer) . ,(choose-n 0 node-steps))
                          ((Node . Region) . ,(choose-n 0 node-steps))
                          ((Node . District) . ,(choose-n 0 node-steps))
                          ((Node . VSquare) . ,(choose-n 0 node-steps))
                          ((Node . HSquare) . ,(choose-n 0 node-steps))
                          ((Node . PollingPlace) . ,(choose-n 0 leaf-steps))))))))))

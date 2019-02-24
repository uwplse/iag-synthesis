#lang rosette

(require rosette/lib/angelic
         "../../schedule/syntax.rkt")

(provide treemap-sketch)

(define (choose-n n xs)
  (build-list n (thunk* (apply choose* xs))))

(define treemap-sketch
  (let ([Root-steps '((child . totalMag_sum)
                      (self . totalMag)
                      (child . votesUR_sum)
                      (self . votesUR)
                      (child . w)
                      (child . h)
                      (child . rx)
                      (child . by)
                      (child . minTunrout)
                      (child . maxTurnout)
                      (child . showFraud)
                      (child . showProjected)
                      (child . showJavascript)
                      (child . fixWidth)
                      'nop 'nop 'nop 'nop)]
        [Node-steps '((self . x)
                      (self . y)
                      (self . render)
                      (self . totalMag)
                      (self . votesUR)
                      (childs . totalMag_sum)
                      (childs . votesUR_sum)
                      (childs . w)
                      (childs . h)
                      (childs . rx)
                      (childs . by)
                      (childs . minTurnout)
                      (childs . maxTurnout)
                      (childs . showFraud)
                      (childs . showProjected)
                      (childs . showJavascript)
                      (childs . fixWidth)
                      'nop 'nop 'nop 'nop)]
        [Leaf-steps '((self . calcProjectedColor)
                      (self . calcRegularColor)
                      (self . calcVotesColor)
                      (self . calcFraudColor)
                      (self . render)
                      (self . x)
                      (self . y)
                      (self . magnitude)
                      (self . totalMag)
                      (self . votesUR))])
    (sched-comp
     'seq
     (sched-trav
      'pre
      `((Root . ,(choose-n 10 Root-steps))
        (CountryContainer . ,(choose-n 8 Node-steps))
        (Region . ,(choose-n 8 Node-steps))
        (District . ,(choose-n 8 Node-steps))
        (VSquare . ,(choose-n 8 Node-steps))
        (HSquare . ,(choose-n 8 Node-steps))
        (PollingPlace . ,(choose-n 7 Leaf-steps))))
     (sched-comp
      'seq
      (sched-trav
       'post
       `((Root . ,(choose-n 4 Root-steps))
         (CountryContainer . ,(choose-n 4 Node-steps))
         (Region . ,(choose-n 4 Node-steps))
         (District . ,(choose-n 4 Node-steps))
         (VSquare . ,(choose-n 4 Node-steps))
         (HSquare . ,(choose-n 4 Node-steps))
         (PollingPlace . ,(choose-n 1 Leaf-steps))))
      (sched-comp
       'seq
       (sched-trav
        'pre
        `((Root . ,(choose-n 2 Root-steps)) ; ??
          (CountryContainer . ,(choose-n 5 Node-steps))
          (Region . ,(choose-n 5 Node-steps))
          (District . ,(choose-n 5 Node-steps))
          (VSquare . ,(choose-n 5 Node-steps))
          (HSquare . ,(choose-n 5 Node-steps))
          (PollingPlace . ,(choose-n 2 Leaf-steps))))
       (sched-comp
        'seq
        (sched-trav
         'post
         `((Root . ,(choose-n 2 Root-steps))
           (CountryContainer . ,(choose-n 4 Node-steps))
           (Region . ,(choose-n 4 Node-steps))
           (District . ,(choose-n 4 Node-steps))
           (VSquare . ,(choose-n 4 Node-steps))
           (HSquare . ,(choose-n 4 Node-steps))
           (PollingPlace . ,(choose-n 0 Leaf-steps))))
        (sched-trav
         'pre
         `((Root . ,(choose-n 0 Root-steps))
           (CountryContainer . ,(choose-n 0 Node-steps))
           (Region . ,(choose-n 0 Node-steps))
           (District . ,(choose-n 0 Node-steps))
           (VSquare . ,(choose-n 0 Node-steps))
           (HSquare . ,(choose-n 0 Node-steps))
           (PollingPlace . ,(choose-n 0 Leaf-steps))))))))))

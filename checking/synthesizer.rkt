#lang rosette

; Checking synthesizer for language of tree traversal schedules

(require rosette/lib/angelic
         racket/promise
         "../enumeration.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "interpreter.rkt")

(provide (all-defined-out))

(define (synthesize-schedule grammar schedule-skeleton forest)
  (let ([sketch (make-sketch grammar schedule-skeleton choose*)]
        [initial-time (current-milliseconds)])

    (with-handlers ([exn:fail? (const #f)])
      (for ([tree forest])
        (let-values ([(outputs _) (tree-partition-fields void? tree)])
          (interpret grammar sketch tree)
          (for ([location outputs])
            (assert (not (void? (unbox location))))))))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list model) overhead-time solving-time _)
                        (time-apply force (list (delay (solve #t))))])
      (displayln (if (sat? model) "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint size: ~a nodes and ~a variables\n" nodes variables)
      (and (sat? model) (evaluate sketch model)))))

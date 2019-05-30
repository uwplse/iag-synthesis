#lang rosette

; Checking synthesizer for language of tree traversal schedules

(require rosette/lib/angelic
         racket/promise
         "../schedule/enumerate.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "interpreter.rkt")

(provide complete-sketch)

(define (multichoose* n xs)
  (build-list n (thunk* (apply choose* xs))))

(define (complete-sketch G sketch examples)
  (let ([schedule (instantiate-sketch multichoose* G sketch)]
        [initial-time (current-milliseconds)])

    (for ([tree examples])
      (with-handlers ([exn:fail? (const #f)])
        (let ([tree (tree-annotate G tree emp new upd)])
          (interpret G schedule tree)
          (tree-validate G tree lkup))))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list model) overhead-time solving-time _)
                        (time-apply force (list (delay (solve #t))))])
      (displayln (if (sat? model) "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint size: ~a nodes and ~a variables\n" nodes variables)
      (and (sat? model) (evaluate schedule model)))))

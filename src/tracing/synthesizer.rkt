#lang rosette

; Tracing synthesizer for language of tree traversal schedules

(require "../schedule/enumerate.rkt"
         "../grammar/tree.rkt"
         "../utility.rkt"
         "../trace.rkt"
         "interpreter.rkt")

(provide complete-sketch)

(define (complete-sketch G sketch examples)
  (parameterize ([*box* ref]
                 [*box?* ref?]
                 [*unbox* deref]
                 [*set-box!* set-ref!]
                 [*denotation* abstract-denotation])
    (let ([schedule (instantiate-sketch permute G sketch)]
          [initial-time (current-milliseconds)])

      (for ([tree examples])
        (let ([tree (tree-annotate tree)])
          (interpret G schedule tree)
          (tree-validate G tree deref)
          (break)))

      (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                         [(nodes variables) (formula-size)]
                         [((list concretize) overhead-time solving-time _)
                          (time-apply solve null)])
        (displayln (if concretize "SAT" "UNSAT"))
        (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
        (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
        (printf "Constraint Size: ~a nodes and ~a variables\n" nodes variables)
        (and concretize (concretize schedule))))))

#lang rosette

; Tracing synthesizer for language of tree traversal schedules

(require "../schedule/enumerate.rkt"
         "../grammar/syntax.rkt"
         "../grammar/tree.rkt"
         "../utility.rkt"
         "../trace.rkt"
         "interpreter.rkt")

(provide complete-sketch)

(define (concretize* subst S)
  (match S
    [(ag:sequential sched-1 sched-2)
     (ag:sequential (concretize* subst sched-1)
                    (concretize* subst sched-2))]
    [(ag:parallel sched-1 sched-2)
     (ag:parallel (concretize* subst sched-1)
                  (concretize* subst sched-2))]
    [(ag:traversal order visitor-list)
     (ag:traversal order
                  (map (curry concretize* subst) visitor-list))]
    [(ag:visitor class command-list)
     (ag:visitor class (subst command-list))]))

(define (permute* . xs)
  (permute (length xs) xs))

(define (complete-sketch G sketch examples)
  (parameterize ([*box* ref]
                 [*box?* ref?]
                 [*unbox* deref]
                 [*set-box!* set-ref!]
                 [*denotation* abstract-denotation]
                 [*multichoose* permute*])

    (define schedule (instantiate-sketch G sketch))

    (define initial-time (current-milliseconds))

    (for ([tree examples])
      (let ([tree (tree-annotate tree)])
        (interpret schedule tree)
        (tree-validate tree deref)
        (break)))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list concretize) overhead-time solving-time _)
                        (time-apply solve null)])
      (displayln (if concretize "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint Size: ~a nodes and ~a variables\n" nodes variables)
      (and concretize (concretize* concretize schedule)))))

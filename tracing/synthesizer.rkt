#lang rosette

; Tracing synthesizer for language of tree traversal schedules

(require "../schedule/enumerate.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "../trace.rkt"
         "interpreter.rkt")

(provide complete-sketch)

(define (complete-sketch G sketch examples)
  (let ([schedule (instantiate-sketch permute G sketch)]
        [initial-time (current-milliseconds)])

    (for ([tree examples])
      (let* ([allocate (λ (table name) table)]
             [initialize (λ (table name) (table-def! table name))]
             [tree (tree-annotate G tree make-table allocate initialize)])
        (interpret G schedule tree)
        (tree-validate G tree table-ref!)
        (break)))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list concretize) overhead-time solving-time _)
                        (time-apply solve null)])
      (displayln (if concretize "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint Size: ~a nodes and ~a variables\n" nodes variables)
      (and concretize (concretize schedule)))))

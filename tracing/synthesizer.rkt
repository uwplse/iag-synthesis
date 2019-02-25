#lang rosette

; Tracing synthesizer for language of tree traversal schedules

(require "../schedule/syntax.rkt"
         "../schedule/enumerate.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "../trace.rkt"
         "interpreter.rkt")

(provide (all-defined-out))

(define (complete-sketch grammar hole-range sketch examples)
  (let ([schedule (instantiate-sketch multichoose hole-range grammar sketch)]
        [initial-time (current-milliseconds)])

    (for ([tree examples])
      (let* ([allocate (λ (store name) store)]
             [initialize (λ (store name type) (write store name) store)]
             [tree (tree-annotate grammar tree empty allocate initialize)])
        (interpret grammar schedule tree)
        (tree-validate grammar tree output)
        (break)))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list concretize) overhead-time solving-time _)
                        (time-apply solve null)])
      (displayln (if concretize "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint size: ~a nodes and ~a variables\n" nodes variables)
      (and concretize (concretize schedule)))))

;(define (substitute schedule concretize)
;  (match schedule
;    [(sched-parallel left right)
;     (sched-parallel (substitute left concretize)
;                     (substitute right concretize))]
;    [(sched-sequential left right)
;     (sched-sequential (substitute left concretize)
;                       (substitute right concretize))]
;    [(sched-traversal order visitors)
;     (sched-traversal order
;                      (for/list ([visitor visitors])
;                        (cons (car visitor)
;                              (for/list ([block (cdr visitor)])
;                                (if (multichoice? block)
;                                    (concretize block)
;                                    (map concretize block))))))]))

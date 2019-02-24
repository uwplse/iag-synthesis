#lang rosette

; Tracing synthesizer for language of tree traversal schedules

(require "../schedule/syntax.rkt"
         "../enumeration.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "../trace.rkt"
         "interpreter.rkt")

(provide (all-defined-out))

(define (synthesize-schedule grammar schedule-skeleton forest)
  (let ([sketch (make-sketch grammar schedule-skeleton trace-choose)]
        [initial-time (current-milliseconds)])

    (for ([tree forest])
      (let-values ([(outputs inputs) (tree-partition-fields void? tree)])
        (trace-start)
        (interpret grammar sketch tree)
        (trace-stop inputs outputs)))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list concretize) overhead-time solving-time _)
                        (time-apply trace-solve null)])
      (displayln (if concretize "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint size: ~a nodes and ~a variables\n" nodes variables)
      (and concretize (substitute sketch concretize)))))

(define (substitute sketch concretize)
  (match sketch
    [(sched-comp comp left-sketch right-sketch)
     (sched-comp comp
                 (substitute left-sketch concretize)
                 (substitute right-sketch concretize))]
    [(sched-trav order visits)
     (sched-trav order
                 (for/list ([visit visits])
                   (cons (car visit)
                         (for/list ([slot (cdr visit)])
                           (if (trace-choice? slot)
                               (let ([slot (concretize slot)])
                                 (if (list? slot)
                                     (cons (car slot) (map concretize (cdr slot)))
                                     slot))
                               slot)))))]))

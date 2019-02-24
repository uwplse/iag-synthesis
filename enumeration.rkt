#lang rosette

(require "grammar/syntax.rkt"
         "schedule/syntax.rkt")

(provide (all-defined-out))

; FIXME: We *really* need a principled strategy for sketch enumeration.
; Huh, I guess these "skeletons" are essentially metasketches.

(define (enumerate-sequential grammar bound)
  (reverse (enumerate-skeletons grammar bound (list 'seq))))

(define (enumerate-parallel grammar bound)
  (reverse (enumerate-skeletons grammar bound (list 'par 'seq))))

(define (enumerate-skeletons grammar depth compositions)
  (let ([empty-visits (map (compose list ag-class-name)
                           (ag-grammar-classes grammar))])

    (define (recur depth)
      (if (= depth 0)
          (list (sched-trav 'pre empty-visits) (sched-trav 'post empty-visits))
          (let ([subschedules (recur (- depth 1))])
            (for*/fold ([schedules subschedules])
                       ([left subschedules]
                        [right subschedules]
                        [comp compositions])
              (cons (sched-comp comp left right) schedules)))))

    (recur depth)))

; Create a symbolic schedule sketch from a schedule skeleton and the given
; symbolic choice procedure, splitting the number of slots at each parallel
; composition by the given factor.
(define (make-sketch grammar skeleton choose [parallels 0] #:split [split 1/2])
  (match skeleton
    [(sched-comp comp left-sketch right-sketch)
     (let ([parallels (+ (match comp ['seq 0] ['par 1]) parallels)])
       (sched-comp comp
                   (make-sketch grammar left-sketch choose parallels #:split split)
                   (make-sketch grammar right-sketch choose parallels #:split split)))]
    [(sched-trav order visits)
     (sched-trav order
                 (let ([scale (expt split parallels)])
                   (for/list ([visit visits])
                     (make-visit grammar (car visit) scale choose))))]))

(define (make-visit grammar classname scale choose)
  (define-values (looped unlooped)
    (partition (compose ag-loop? ag-rule-right)
               (ag-class-rules (get-class grammar classname))))

  (define (multichoose n xs)
    (build-list n (thunk* (apply choose xs))))

  (define stmt-slots
    (cons 'nop (map ag-rule-left unlooped)))

  (define loop-slots
    (map (λ (group)
           (cons (ag-loop-object (ag-rule-right (first group)))
                 (multichoose (length group) (cons 'nop (map ag-rule-left group)))))
         (group-by (compose ag-loop-object ag-rule-right) looped)))

  (define width
    (ceiling (* scale (+ (length loop-slots) (length stmt-slots)))))

  (define holes
    (multichoose width (append loop-slots stmt-slots)))

  (cons classname holes))

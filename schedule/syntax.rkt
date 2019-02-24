#lang rosette

; Abstract Syntax for Language of Tree Traversal Schedules

(provide (struct-out sched-parallel)
         (struct-out sched-sequential)
         (struct-out sched-traversal)
         (struct-out sched-slot-skip)
         (struct-out sched-slot-eval)
         (struct-out sched-hole)
         sched?
         sched-flatten)

; parallel schedule composition
(struct sched-parallel (left right) #:transparent)

; sequential schedule composition
(struct sched-sequential (left right) #:transparent)

; tree traversal
(struct sched-traversal (order visitors) #:transparent)

; block slots
(struct sched-slot-skip () #:transparent)
(struct sched-slot-eval (object label) #:transparent)

; program hole (for intermediate sketches only; will crash interpreter)
(struct sched-hole () #:transparent)

(define (sched? s)
  (or (sched-parallel? s) (sched-sequential? s) (sched-traversal? s)))

(define (sched-flatten schedule [traversals null])
  (match schedule
    [(sched-parallel left right) (sched-flatten left (sched-flatten right traversals))]
    [(sched-sequential left right) (sched-flatten left (sched-flatten right traversals))]
    [traversal (cons traversal traversals)]))


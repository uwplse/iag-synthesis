#lang rosette

; Abstract Syntax for Language of Tree Traversal Schedules

(provide (struct-out sched-parallel)
         (struct-out sched-sequential)
         (struct-out sched-traversal)
         (struct-out sched-iterate)
         (struct-out sched-recur)
         (struct-out sched-slot-skip)
         (struct-out sched-slot-call)
         (struct-out sched-slot-hole)
         sched?
         sched-flatten)

; parallel schedule composition
(struct sched-parallel (left right) #:transparent)

; sequential schedule composition
(struct sched-sequential (left right) #:transparent)

; tree traversal
(struct sched-traversal (order visitors) #:transparent)

; iterative loop
(struct sched-iterate (child body) #:transparent)

; recursive call (iterative when necessary)
(struct sched-recur (child) #:transparent)

; statement slots
(struct sched-slot-hole () #:transparent)
(struct sched-slot-skip () #:transparent)
(struct sched-slot-call (method params) #:transparent)

(define (sched? s)
  (or (sched-parallel? s) (sched-sequential? s) (sched-traversal? s)))

(define (sched-flatten schedule [traversals null])
  (match schedule
    [(sched-parallel left right) (sched-flatten left (sched-flatten right traversals))]
    [(sched-sequential left right) (sched-flatten left (sched-flatten right traversals))]
    [traversal (cons traversal traversals)]))


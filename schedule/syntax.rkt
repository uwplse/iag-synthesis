#lang rosette

; Abstract Syntax for Language of Tree Traversal Schedules

(provide (struct-out sched-comp)
         (struct-out sched-trav)
         sched?)

(define (sched? s)
  (or (sched-comp? s)
      (sched-trav? s)))

; parallel schedule composition
(struct sched-comp
  (; really just 'seq or 'par
   type
   ; first schedules to compose in parallel
   left
   ; second schedules to compose in parallel
   right
   ) #:transparent)

; traversal descriptor
(struct sched-trav
  (; 'pre, 'post, or 'rec
   order
   ; list associating each classname symbol to a list of steps
   visits
   ) #:transparent)

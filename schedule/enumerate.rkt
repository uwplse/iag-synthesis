#lang rosette

(require "../grammar/syntax.rkt"
         "syntax.rkt")

(provide instantiate-sketch
         enumerate-sketches
         synthesize-schedules)

; Collect the possible method calls in a class's visitor.
(define (hole-range G class-name)
  (map car (ag-class-methods (get-class G class-name))))

; Instantiate a traversal sketch for an attribute grammar G, given an
; interpretation of (multi-)choice. Used to denote a traversal sketch as a
; symbolic traversal.
(define (instantiate-traversal-sketch multichoose G order visitors)
  (let ([traversal (get-traversal G order)])
    (sched-traversal order
                     (for/list ([(class-name visitor-body) (in-dict visitors)])
                       (cons class-name
                             (for/list ([slot visitor-body])
                               (if (sched-slot-hole? slot)
                                   (let* ([range (hole-range G class-name)]
                                          [k (length range)])
                                     (multichoose k range))
                                   ; TODO: What about iteration?
                                   slot)))))))

; Instantiate a schedule sketch for an attribute grammar G, given an
; interpretation of (multi-)choice. Used to denote a schedule sketch as a
; symbolc schedule.
(define (instantiate-sketch multichoose G sketch)
  (match sketch
    [(sched-sequential left right)
     (sched-sequential (instantiate-sketch multichoose G left)
                       (instantiate-sketch multichoose G right))]
    [(sched-parallel left right)
     (sched-parallel (instantiate-sketch multichoose G left)
                     (instantiate-sketch multichoose G right))]
    [(sched-traversal order visitors)
     (instantiate-traversal-sketch multichoose G order visitors)]))

(define/match (sketch-traversal statement)
  [((ag-recur child))
   (sched-recur child)]
  [((ag-visit))
   (sched-slot-hole)]
  [((ag-iterate child body))
   (sched-iterate child (sketch-traversal body))])

; Enumerate traversal sketches for an attribute grammar G.
(define (enumerate-traversal-sketches G)
  (for/list ([(order visitor-list) (in-dict (ag-grammar-traversals G))])
    (sched-traversal order
                     null ; FIXME
                     (for/list ([(class-name visitor-body) (in-dict visitor-list)])
                       (cons class-name (map sketch-traversal visitor-body))))))

; Enumerate schedule sketches for an attribute grammar G, bounded by the number
; of traversal passes up to n, using the provided list of traversal sketches.
(define (enumerate-schedule-sketches G n trav-sketch-list)
  (if (= n 1)
      trav-sketch-list
      (append
       (if (= n 2)
           (for*/list ([left trav-sketch-list]
                       [right trav-sketch-list])
             (sched-parallel left right))
           null)
       (for*/list ([left trav-sketch-list]
                   [right (enumerate-schedule-sketches G (- n 1) trav-sketch-list)])
         (sched-sequential left right)))))

; Enumerate schedule sketches for an attribute grammar G, bounded by the number
; of traversal passes up to n.
(define (enumerate-sketches G n)
  (enumerate-schedule-sketches G n (enumerate-traversal-sketches G)))

(define (synthesize-schedules G examples n synthesizer)
  (for ([sketch (shuffle (enumerate-sketches G n))])
    (printf "\n\n\n--------------------------------\n")
    (displayln sketch)
    (synthesizer G sketch examples)
    (clear-asserts!)))


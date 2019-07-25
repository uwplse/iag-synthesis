#lang rosette

(require "../grammar/syntax.rkt")

(provide instantiate-sketch
         enumerate-sketches
         synthesize-schedules)

(define multichoose (make-parameter (curry list 'choose)))

(define (enumerate-commands class #:order [order #f] #:iterator [iterator #f])
  (for/list ([rule (ag:class-rules* class)]
             #:when (eq? (ag:rule-iteration rule) iterator)
             #:when (implies (ag:rule-order rule)
                             (eq? (ag:rule-order rule) order)))
    (ag:eval (ag:rule-attribute rule))))

(define (instantiate-command-sketch class command [order #f] [iterator #f])
  (match command
    [(ag:iter/left child slots)
     (define steps
       (for/list ([slot slots])
         (instantiate-command-sketch class slot 'left child)))
     (ag:iter/left child steps)]
    [(ag:iter/right child slots)
     (define steps
       (for/list ([slot slots])
         (instantiate-command-sketch class slot 'right child)))
     (ag:iter/right child steps)]
    [(ag:hole)
     (let ([range (enumerate-commands class #:order order #:iterator iterator)])
       ((multichoose) (length range) range))]
    [(ag:eval _) command]
    [(ag:recur _) command]))

; Instantiate a traversal sketch for an attribute grammar G, given an
; interpretation of (multi-)choice. Used to denote a traversal sketch as a
; symbolic traversal.
(define (instantiate-traversal-sketch G traversal)
  (define visitors
    (for/list ([visitor (ag:traversal-visitors traversal)])
      (define class (ag:grammar-ref/class G (ag:visitor-class visitor)))
      (ag:visitor (ag:visitor-class visitor)
                  (map (curry instantiate-command-sketch class)
                       (ag:visitor-commands visitor)))))

  (ag:traversal (ag:traversal-name traversal) visitors))

; Instantiate a schedule sketch for an attribute grammar G, given an
; interpretation of (multi-)choice. Used to denote a schedule sketch as a
; symbolc schedule.
(define (instantiate-sketch G sketch)
  (match sketch
    [(ag:sequential left-sched right-sched)
     (ag:sequential (instantiate-sketch G left-sched)
                    (instantiate-sketch G right-sched))]
    [(ag:parallel left-sched right-sched)
     (ag:parallel (instantiate-sketch G left-sched)
                  (instantiate-sketch G right-sched))]
    [(ag:traverse order)
     (instantiate-traversal-sketch G (ag:grammar-ref/traversal G order))]
    [(ag:traversal order visitors)
     (instantiate-traversal-sketch G sketch)]))

; Enumerate schedule sketches for an attribute grammar G, bounded by the number
; of traversal passes up to n.
(define (enumerate-sketches G n)
  (let ([traversals (ag:grammar-traversals G)])
  (if (= n 1)
      traversals
      (append
       (if (= n 2)
           (for*/list ([left-sched traversals]
                       [right-sched traversals])
             (ag:parallel left-sched right-sched))
           null)
       (for*/list ([left-sched traversals]
                   [right-sched (enumerate-sketches G (- n 1))])
         (ag:sequential left-sched right-sched))))))

(define (synthesize-schedules G examples n synthesizer)
  (for ([sketch (shuffle (enumerate-sketches G n))])
    (printf "\n\n\n--------------------------------\n")
    (displayln sketch)
    (synthesizer G sketch examples)
    (clear-asserts!)))

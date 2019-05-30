#lang rosette

(require "../grammar/syntax.rkt")

(provide instantiate-sketch
         enumerate-sketches
         synthesize-schedules)

(define (enumerate-method-calls class-body)
  (for/list ([method-decl (ag-class-methods class-body)])
    `(call ,(method-name method-decl))))

(define (enumerate-commands class-body #:iterate [iter #f])
  (remove-duplicates
   (for/list ([rule-decl (ag-class-rules class-body)]
              #:when (object-iterated<=? (rule-iterates rule-decl) iter))
     (match-let ([(rule (object node _) label _) rule-decl])
       `(eval ,node ,label)))))

; Instantiate a traversal sketch for an attribute grammar G, given an
; interpretation of (multi-)choice. Used to denote a traversal sketch as a
; symbolic traversal.
(define (instantiate-traversal-sketch multichoose G order traversal)
;  (define hole-range ; FIXME: Fuck, need to check loop context too!
;    (for/hasheq ([(class-name class-body) (in-dict (ag-grammar-classes G))])
;      (values class-name
;              (enumerate-commands class-body )
;              (append (enumerate-method-calls class-body)
;                      (enumerate-rule-evals class-body)))))
;  (define (hole-fill* class-name)
;    (let ([range (hash-ref hole-range class-name)])
;      (multichoose (length range) range)))
  (define (instantiate-command-sketch class-name command)
    (define class-body (grammar-class G class-name))
    (define (aux command #:iterate [iter #f])
      (match command
        [`(iter-left ,child ,slots)
         (define steps
           (for/list ([slot slots])
             (aux slot #:iterate `(succ ,child))))
        `(iter-left ,child ,steps)]
        [`(iter-right ,child ,slots)
         (define steps
           (for/list ([slot slots])
             (aux slot #:iterate `(pred ,child))))
         `(iter-right ,child ,steps)]
        [`(hole)
         (let ([range (enumerate-commands class-body #:iterate iter)])
           (multichoose (length range) range))]
        [`(eval ,_ ,_) command]
        [`(call ,_) command]
        [`(recur ,_) command]))
    (aux command))
  (define visitors
    (for/list ([(class-name commands) (in-dict traversal)])
      (cons class-name
            (map (curry instantiate-command-sketch class-name) commands))))
  `(trav ,order ,visitors))

; Instantiate a schedule sketch for an attribute grammar G, given an
; interpretation of (multi-)choice. Used to denote a schedule sketch as a
; symbolc schedule.
(define (instantiate-sketch multichoose G sketch)
  (match sketch
    [`(seq ,left-sched ,right-sched)
     `(seq ,(instantiate-sketch multichoose G left-sched)
           ,(instantiate-sketch multichoose G right-sched))]
    [`(par ,left-sched ,right-sched)
     `(par ,(instantiate-sketch multichoose G left-sched)
           ,(instantiate-sketch multichoose G right-sched))]
    [`(trav ,order ,visitors)
     (instantiate-traversal-sketch multichoose G order visitors)]))

; Enumerate schedule sketches for an attribute grammar G, bounded by the number
; of traversal passes up to n.
(define (enumerate-sketches G n)
  (let ([trav-sketches (ag-grammar-traversals G)])
  (if (= n 1)
      trav-sketches
      (append
       (if (= n 2)
           (for*/list ([left-sched trav-sketches]
                       [right-sched trav-sketches])
             `(par ,left-sched ,right-sched))
           null)
       (for*/list ([left-sched trav-sketches]
                   [right-sched (enumerate-sketches G (- n 1) trav-sketches)])
         `(seq ,left-sched ,right-sched))))))

(define (synthesize-schedules G examples n synthesizer)
  (for ([sketch (shuffle (enumerate-sketches G n))])
    (printf "\n\n\n--------------------------------\n")
    (displayln sketch)
    (synthesizer G sketch examples)
    (clear-asserts!)))


#lang rosette

(require "../grammar/syntax.rkt"
         "syntax.rkt")

(provide (all-defined-out))

(define (block-contexts form)
  (reverse
   (for/fold ([result null])
             ([part form])
     (match part
       [(ag-trav-visit) (cons #f result)]
       [(ag-trav-loop child-seq loop-form)
        (for/fold ([result result])
                  ([loop-part loop-form])
          (match loop-part
            [(ag-trav-visit) (cons child-seq result)]
            [_ result]))]
       [_ result]))))

; Analyze an attribute grammar and return a function mapping a classname and a
; loop context (child-sequence name or #f) to the range of a program hole for a
; slot in a visitor block for that class for a loop over the named child, if given.
(define (make-hole-range grammar)
  (define (rule->slot rule)
    (match-let ([(ag-rule (cons object label) _) rule])
      (sched-slot-eval object label)))

  (define looped? (compose ag-loop? ag-rule-right))

  (define loops-over (compose ag-loop-object ag-rule-right))

  (define unlooped-slots-by-class
    (for/list ([class-ast (ag-grammar-classes grammar)])
      (let ([unlooped-rules (filter-not looped? (ag-class-rules class-ast))])
        (cons (ag-class-name class-ast)
              (cons (sched-slot-skip) (map rule->slot unlooped-rules))))))

  (define looped-slots-by-child-by-class
    (for/list ([class-ast (ag-grammar-classes grammar)])
      (let ([looped-rules (filter looped? (ag-class-rules class-ast))])
        (cons (ag-class-name class-ast)
              (for/list ([looped-rules-for-child (group-by loops-over looped-rules)])
                (cons (loops-over (first looped-rules-for-child))
                      (cons (sched-slot-skip) (map rule->slot looped-rules-for-child))))))))

  (λ (classname context)
    (if context
        (cdr (assoc context (cdr (assoc classname looped-slots-by-child-by-class))))
        (cdr (assoc classname unlooped-slots-by-class)))))

(define (instantiate-traversal-sketch multichoose hole-range grammar order visitors split)
  (let ([traversal (get-traversal grammar order)])
    (sched-traversal order
                     (for/list ([visitor visitors])
                       (let* ([classname (car visitor)]
                              [form (cdr (assoc classname (ag-traversal-forms traversal)))])
                         (cons classname
                               (for/list ([block (cdr visitor)]
                                          [context (block-contexts form)])
                                 (let* ([range (hole-range classname context)]
                                        [n (length range)]
                                        [k (min n (ceiling (/ n split)))])
                                   (if (sched-hole? block)
                                       (multichoose k range)
                                       (for/list ([slot block])
                                         (if (sched-hole? slot)
                                             (multichoose 1 range)
                                             slot)))))))))))

; Instantiate a schedule sketch using a provided construct for (multi-)choice to
; construct symbolic representations of program holes of slots and/or blocks.
(define (instantiate-sketch multichoose hole-range grammar sketch [split 1])
  (match sketch
    [(sched-sequential left right)
     (sched-sequential (instantiate-sketch multichoose hole-range grammar left split)
                       (instantiate-sketch multichoose hole-range grammar right split))]
    [(sched-parallel left right)
     (sched-parallel (instantiate-sketch multichoose hole-range grammar left split)
                     (instantiate-sketch multichoose hole-range grammar right split))]
    [(sched-traversal order visitors)
     (instantiate-traversal-sketch multichoose hole-range grammar order visitors split)]))

(define (enumerate-traversal-sketches grammar)
  (for/list ([traversal (ag-grammar-traversals grammar)])
    (sched-traversal (ag-traversal-name traversal)
                     (for/list ([classname-form (ag-traversal-forms traversal)])
                       (match-let ([(cons classname form) classname-form])
                         (cons classname
                               (map (const (sched-hole)) (block-contexts form))))))))

(define (enumerate-schedule-sketches grammar passes traversals)
  (if (= passes 1)
      traversals
      (append
       (if (= passes 2)
           (for*/list ([left traversals]
                       [right traversals])
             (sched-parallel left right))
           null)
       (for*/list ([left traversals]
                   [right (enumerate-schedule-sketches grammar (- passes 1) traversals)])
         (sched-sequential left right)))))

(define (enumerate-sketches grammar passes)
  (enumerate-schedule-sketches grammar passes (enumerate-traversal-sketches grammar)))


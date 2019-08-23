#lang rosette

; Interpreter for language of tree traversal schedules

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../grammar/tree.rkt"
         "../trace.rkt")

(provide denotation *denotation* concrete-denotation abstract-denotation
         interpret traverse)

(struct denotation (ops fns ite) #:transparent)

(define concrete-denotation
  (denotation (match-lambda ['+ +] ['- -] ['* *] ['/ /]
                            ['< <] ['<= <=] ['== =] ['>= >=] ['> >]
                            ['! not]
                            ['&& (λ (e1 e2) (and e1 e2))]
                            ['\|\| (λ (e1 e2) (or e1 e2))])
              (match-lambda ['max max] ['min min])
              (λ (e1 e2 e3) (if e1 e2 e3))))

(define abstract-denotation
  (denotation (const void)
              (const void)
              (const void)))

(define *denotation* (make-parameter concrete-denotation))

(define (denote-op op . xs)
  (apply ((denotation-ops (*denotation*)) op) xs))

(define (denote-fn fn . xs)
  (apply ((denotation-fns (*denotation*)) fn) xs))

(define (denote-ite if then else)
  ((denotation-ops (*denotation*)) if then else))

(define (accumulator self)
  (for/list ([attr (ag:class-counters (tree-class self))])
    (cons attr (box #f))))

(define (interpret schedule tree)
  (match schedule
    [(ag:sequential left-sched right-sched)
     (interpret left-sched tree)
     (interpret right-sched tree)]
    [(ag:parallel left-sched right-sched)
     (join (interpret left-sched tree)
           (interpret right-sched tree))]
    [(ag:traversal order visitors)
     (traverse schedule tree)]))

(define (traverse trav self)
  (define class (tree-class self))
  (define visitor (ag:traversal-ref/visitor trav class))
  (for*/permuted ([command (ag:visitor-commands visitor)])
    (match command
      [(ag:recur child)
       (define subtree (tree-ref/child self child))
       (if (list? subtree)
           (for ([node subtree])
             (traverse trav node))
           (traverse trav subtree))]
      [(ag:iter/left child commands)
       (iterate self child identity commands trav)]
      [(ag:iter/right child commands)
       (iterate self child reverse commands trav)]
      [(ag:eval attr)
       (define expr (ag:rule-formula (ag:class-ref*/rule class attr)))
       (set-box! (tree-ref*/field self attr) (evaluate self expr))]
      [(ag:skip)
       (void)])))

(define (iterate self child order commands trav)
  (define class (tree-class self))
  (define state0 (accumulator self))
  (define iterator (order (tree-ref/child self child)))

  (for*/permuted ([command commands])
    (match command
      [(ag:recur _)
       (void)]
      [(ag:eval attr)
       (define expr (ag:rule-init (ag:class-ref*/rule class attr)))
       (when expr
         (set-box! (dict-ref state0 attr) (evaluate self expr)))]
      [(ag:skip)
       (void)]))

  (define state$
    (for/fold ([state@ state0])
              ([cursor iterator]
               [follow (rest (append iterator (list #f)))])
      (define state+ (accumulator self))
      (for*/permuted ([command commands])
        (match command
          [(ag:recur (== child))
           (traverse trav cursor)]
          [(ag:eval (cons (== child) label))
           (define rule (ag:class-ref*/rule class (cons child label)))
           (define value (evaluate self (ag:rule-step rule) child state@ cursor follow))
           (set-box! (tree-ref/field cursor label) value)
           (when (ag:rule-iterative? rule)
             (set-box! (dict-ref state+ (cons child label)) value))]
          [(ag:eval attr)
           (define expr (ag:rule-next (ag:class-ref*/rule class attr)))
           (set-box! (dict-ref state+ attr) (evaluate self expr child state@ cursor follow))]
          [(ag:skip)
           (void)]))

      state+))

  (for*/permuted ([command commands])
    (match command
      [(ag:recur _)
       (void)]
      [(ag:eval (cons (== child) label))
       ; TODO: Stash these values somewhere.
       (void)]
      [(ag:eval attr)
       (set-box! (tree-ref*/field self attr) (unbox (dict-ref state$ attr)))]
      [(ag:skip)
       (void)])))

(define (evaluate self term [iter #f] [accum #f] [this #f] [next #f])
  (define/match (recur term)
    [((ag:const val))
     val]
    [((ag:field/get attr))
     (unbox (tree-ref*/field self attr))]
    [((ag:field/cur (cons (== iter) label)))
     (unbox (tree-ref/field this label))]
    [((ag:field/acc attr))
     (unbox (dict-ref accum attr))]
    ; XXX: Unimplemented but also unused.
    ; [((ag:field/sup attr))
    ;  (unbox (dict-ref super attr))]
    [((ag:field/peek (cons (== iter) label) default))
     (if next
         (unbox (tree-ref/field next label))
         (recur default))]
    [((ag:field/first (cons child label) default))
     (define nodes (tree-ref/child self child))
     (if (null? nodes)
         (recur default)
         (unbox (tree-ref/field (first nodes) label)))]
    [((ag:field/last (cons child label) default))
     (define nodes (tree-ref/child self child))
     (if (null? nodes)
         (recur default)
         (unbox (tree-ref/field (last nodes) label)))]
    [((ag:length child))
     (length (tree-ref/child self child))]
    [((ag:branch if then else))
     (denote-ite (recur if) (recur then) (recur else))]
    [((ag:expr operator operands))
     (denote-op operator (map recur operands))]
    [((ag:call function arguments))
     (denote-fn function (map recur arguments))]
    [((ag:invoke receiver function arguments))
     (denote-fn function (map recur (cons receiver arguments)))])
  (recur term))

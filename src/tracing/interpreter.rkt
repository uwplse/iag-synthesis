#lang rosette

; Interpreter for language of tree traversal schedules

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../grammar/expression.rkt"
         "../grammar/tree.rkt"
         "../trace.rkt")

(provide denotation *denotation* concrete-denotation abstract-denotation
         interpret traverse)

(struct denotation (ops cmps fns ite) #:transparent)

(define concrete-denotation
  (denotation (match-lambda ['+ +] ['- -] ['* *] ['/ /]
                            ['! not]
                            ['&& (λ (e1 e2) (and e1 e2))]
                            ['\|\| (λ (e1 e2) (or e1 e2))])
              (match-lambda ['< <] ['<= <=] ['== =] ['>= >=] ['> >])
              (match-lambda ['max max] ['min min])
              (λ (e1 e2 e3) (if e1 e2 e3))))

(define abstract-denotation
  (denotation (const void)
              (const void)
              (const void)
              (const void)))

(define *denotation* (make-parameter concrete-denotation))

(define (denote-op op xs)
  (apply ((denotation-ops (*denotation*)) op) xs))

(define (denote-cmp op l r)
  (apply ((denotation-cmps (*denotation*)) op) (list l r)))

(define (denote-fn fn xs)
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
  (define super (accumulator self))
  (for*/permuted ([command (ag:visitor-commands visitor)])
    (match command
      [(ag:recur child)
       (define subtree (tree-ref/child self child))
       (if (list? subtree)
           (for ([node subtree])
             (traverse trav node))
           (traverse trav subtree))]
      [(ag:iter/left child commands)
       (iterate self super child identity commands trav)]
      [(ag:iter/right child commands)
       (iterate self super child reverse commands trav)]
      [(ag:eval attr)
       (define expr (ag:rule-formula (ag:class-ref*/rule class attr)))
       (set-box! (tree-ref*/field self attr) (evaluate self super expr))]
      [(ag:skip)
       (void)])))

(define (iterate self super child order commands trav)
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
         (set-box! (dict-ref state0 attr) (evaluate self super expr)))]
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
           (define expr (ag:rule-step rule))
           (define value (evaluate self super expr state@ cursor follow))
           (set-box! (tree-ref/field cursor label) value)
           (when (ag:rule-reductive? rule)
             (set-box! (dict-ref state+ (cons child label)) value))]
          [(ag:eval attr)
           (define expr (ag:rule-next (ag:class-ref*/rule class attr)))
           (define value (evaluate self super expr state@ cursor follow))
           (set-box! (dict-ref state+ attr) value)]
          [(ag:skip)
           (void)]))

      state+))

  (for*/permuted ([command commands])
    (match command
      [(ag:recur _)
       (void)]
      [(ag:eval attr)
       (when (ag:rule-reductive? (ag:class-ref*/rule class attr))
         (set-box! (dict-ref super attr) (unbox (dict-ref state$ attr))))]
      [(ag:skip)
       (void)]))

  (for*/permuted ([command commands])
    (match command
      [(ag:recur _)
       (void)]
      [(ag:eval (cons (== child) label))
       (void)]
      [(ag:eval attr)
       (set-box! (tree-ref*/field self attr) (unbox (dict-ref state$ attr)))]
      [(ag:skip)
       (void)])))

(define (evaluate self super term [accum #f] [this #f] [next #f])
  (define/match (recur term)
    [((ex:const val))
     val]
    [((ex:field/get attr))
     (unbox (tree-ref*/field self attr))]
    [((ex:field/cur (cons _ label)))
     (unbox (tree-ref/field this label))]
    [((ex:field/acc attr))
     (unbox (dict-ref accum attr))]
    [((ex:field/sup attr))
     (unbox (dict-ref super attr))]
    [((ex:field/peek (cons _ label) default))
     (if next
         (unbox (tree-ref/field next label))
         (recur default))]
    [((ex:field/first (cons child label) default))
     (define nodes (tree-ref/child self child))
     (if (null? nodes)
         (recur default)
         (unbox (tree-ref/field (first nodes) label)))]
    [((ex:field/last (cons child label) default))
     (define nodes (tree-ref/child self child))
     (if (null? nodes)
         (recur default)
         (unbox (tree-ref/field (last nodes) label)))]
    [((ex:length child))
     (length (tree-ref/child self child))]
    [((ex:branch if then else))
     (denote-ite (recur if) (recur then) (recur else))]
    [((ex:logic operator operands))
     (denote-op operator (map recur operands))]
    [((ex:order comparison left right))
     (denote-cmp comparison (recur left) (recur right))]
    [((ex:arith operator operands))
     (denote-op operator (map recur operands))]
    [((ex:call function arguments))
     (denote-fn function (map recur arguments))]
    [((ex:invoke receiver function arguments))
     (denote-fn function (map recur (cons receiver arguments)))])
  (recur term))

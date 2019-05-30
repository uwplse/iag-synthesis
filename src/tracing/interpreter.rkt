#lang rosette

; Interpreter for language of tree traversal schedules

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../grammar/tree.rkt"
         "../trace.rkt")

(provide interpret/concrete interpret/symbolic
         traverse)

; TODO: Would be nice to factor out symbolic forking as well...
(struct model (alloc lookup update denote))
(define semantics (make-parameter #f))

(define denotation
  (list (cons '+ +) (cons '- -) (cons '* *) (cons '/ /)
        (cons '< <) (cons '<= <=) (cons '== =) (cons '>= >=) (cons '> >)
        (cons 'max max) (cons 'min min)
        (cons '! not)
        (cons '&& (λ (e1 e2) (and e1 e2)))
        (cons '\|\| (λ (e1 e2) (or e1 e2)))
        (cons 'ite (λ (e1 e2 e3) (if e1 e2 e3)))))

(define concrete-semantics
  (model (const null)
         (λ (store label) (cdr (assoc label store eq?)))
         (λ (store label) (cons (cons label #t) store))
         (λ (operator) (cdr (assoc operator denotation eq?)))))

(define symbolic-semantics
  (model make-table
         table-ref!
         (λ (store label _) (table-def! store label))
         (const void)))

(define (alloc!)
  ((model-alloc (semantics))))
(define (lookup! store label)
  ((model-lookup (semantics)) store label))
(define (update! store label value)
  ((model-update (semantics)) store label value))
(define (denote operator)
  ((model-denote (semantics)) operator))

(define (interpret/concrete G schedule tree)
  (parameterize ([semantics concrete-semantics])
    (interpret G schedule tree)))

(define (interpret/symbolic G schedule tree)
  (parameterize ([semantics symbolic-semantics])
    (interpret G schedule tree)))

(define (interpret G schedule tree)
  (match schedule
    [`(seq ,left-sched ,right-sched)
     (interpret G left-sched tree)
     (interpret G right-sched tree)]
    [`(par ,left-sched ,right-sched)
     (join (interpret G left-sched tree)
           (interpret G right-sched tree))]
    [`(trav ,order ,visitors)
     (traverse G visitors tree)]))

(define (traverse G visitors self)
  ; NOTE: Using just the attribute label for virtual nodes
  ; is an unreliable hack.
  (define class-body (grammar-class G (tree-class self)))
  (for*/permuted ([command (lookup visitors (tree-class self))])
    (match command
      [`(recur ,child)
       (define subtree (lookup (tree-children self) child))
       (if (list? subtree)
           (for ([node subtree])
             (traverse G visitors node))
           (traverse G visitors subtree))]
      [`(iter-left ,child ,commands)
       (iter-left! G visitors self class-body child commands)]
      [`(iter-right ,child ,commands)
       (iter-right! G visitors self class-body child commands)]
      [`(eval ,node ,label)
       (define value
         (eval! self (class-rule-unit class-body node label)))
       (update! (tree-select self `(unit ,node)) label value)]
      [`(call ,method)
       (call! self class-body method)]
      [`(skip)
       (void)])))

(define (iter-left! G visitors self class-body child commands)
  (define virt$0 (alloc!))
  (init! self class-body commands virt$0)
  (for/fold ([virt$- virt$0]
             [pred #f])
            ([curr (lookup (tree-children self) child)])
    (define virt$+ (alloc!))
    (for*/permuted ([command commands])
      (match command
        [`(recur ,(== child))
         (traverse G visitors curr)]
        [`(eval ,node ,label)
         (let* ([expr (class-rule-iter class-body node label (not pred))]
                [value (eval! self expr #:current curr
                              #:virtual virt$- #:predecessor pred)]
                [store (tree-select self `(curr ,node) #:current curr
                                    #:virtual virt$+)])
           (update! store label value))]
        [`(skip)
         (void)]))
    (values virt$+ curr)))

(define (iter-right! G visitors self class-body child commands)
  (define virt$$ (alloc!))
  (init! self class-body commands virt$$)
  (for/fold ([virt$+ virt$$]
             [succ #f])
            ([curr (reverse (lookup (tree-children self) child))])
    (define virt$- (alloc!))
    (for*/permuted ([command commands])
      (match command
        [`(recur ,(== child))
         (traverse G visitors curr)]
        [`(eval ,node ,label)
         (let* ([expr (class-rule-iter class-body node label (not succ))]
                [value (eval! self expr #:current curr
                              #:virtual virt$+ #:successor succ)]
                [store (tree-select self `(curr ,node) #:current curr
                                    #:virtual virt$-)])
           (update! store label value))]
        [`(skip)
         (void)]))
    (values virt$- curr)))

(define (init! self class-body commands virtual)
  (for*/permuted ([command commands])
    (match command
      [`(recur ,child)
       (void)]
      [`(eval ,node ,label)
       (let ([expr (class-rule-init class-body node label)])
         (when expr
           (update! virtual label (eval! self expr))))]
      [`(skip)
       (void)])))

(define (call! self class-body method-name #:current [curr #f]
               #:predecessor [pred #f] #:successor [succ #f])
  (define method-decl (class-method class-body method-name))
  (for ([ref (method-inflow method-decl)])
    (match-let ([(reference object label) ref])
      (lookup! (tree-select self object #:current curr
                            #:predecessor pred #:successor succ)
               label)))
  (for ([ref (method-outflow method-decl)])
    (match-let ([(reference object label) ref])
      (update! (tree-select self object #:current curr) label #t))))

(define (eval! self expr #:current [curr #f] #:virtual [virt #f]
               #:predecessor [pred #f] #:successor [succ #f])
  (define/match (recur expr)
    [((or #t #f (? number?)))
     expr]
    [((reference object label))
     (lookup! (tree-select self object #:current curr #:virtual virt
                           #:predecessor pred #:successor succ)
              label)]
    [((list (symbol op) exprs ...))
     (apply (denote op) (map recur exprs))])
  (recur expr))

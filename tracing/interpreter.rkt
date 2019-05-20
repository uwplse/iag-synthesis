#lang rosette

; Tracing interpreter for language of tree traversal schedules

(require "../utility.rkt"
         "../grammar/syntax.rkt"
         "../tree.rkt"
         "../trace.rkt")

(provide interpret
         traverse)

(define tree-read! (tree-bind! table-ref!))
(define tree-write! (tree-bind! table-def!))

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
  (for ([command (lookup visitors (tree-class self))])
    (match command
      [`(recur ,child)
       (define subtree (lookup (tree-children self) child))
       (if (list? subtree)
           (for ([node subtree])
             (traverse G visitors node))
           (traverse G visitors subtree))]
      [`(iter-left ,child (,block))
       (iter-left! self class-body child block)]
      [`(iter-right ,child (,block))
       (iter-right! self class-body child block)]
      [block
       (for/permuted ([command block])
         (match command
           [`(eval ,node ,label)
            (eval! self (class-rule-unit class-body node label))
            (tree-write! self `(unit ,node) label)]
           [`(call ,method)
            (call! self class-body method)]
           [`(skip)
            (void)]))])))

(define (iter-left! self class-body child block)
  (define virt$0 (make-table))
  (init! self class-body block virt$0)
  (for/fold ([virt$- virt$0]
             [pred #f])
            ([curr (lookup (tree-children self) child)])
    (define virt$+ (make-table))
    (for/permuted ([command block])
      (match command
        [`(eval ,node ,label)
         (eval! self (class-rule-iter class-body node label (not pred))
               #:current curr #:virtual virt$- #:predecessor pred)
         (tree-write! self `(curr ,node) label #:current curr #:virtual virt$+)]
        [`(skip)
         (void)]))
    (values virt$+ curr)))

(define (iter-right! self class-body child block)
  (define virt$$ (make-table))
  (init! self class-body block virt$$)
  (for/fold ([virt$+ virt$$]
             [succ #f])
            ([curr (reverse (lookup (tree-children self) child))])
    (define virt$- (make-table))
    (for/permuted ([command block])
      (match command
        [`(eval ,node ,label)
         (eval! self (class-rule-iter class-body node label (not succ))
               #:current curr #:virtual virt$+ #:successor succ)
         (tree-write! self `(curr ,node) label #:current curr #:virtual virt$-)]
        [`(skip)
         (void)]))
    (values virt$- curr)))

(define (init! self class-body block virtual)
  (for/permuted ([command block])
    (match command
      [`(eval ,node ,label)
       (let ([expr (class-rule-init class-body node label)])
         (when expr
           (eval! self expr)
           (table-def! virtual label)))]
      [`(skip)
       (void)])))

(define (call! self class-body method-name #:current [curr #f]
              #:predecessor [pred #f] #:successor [succ #f])
  (define method-decl (class-method class-body method-name))
  (for ([ref (method-inflow method-decl)])
    (match-let ([(reference object label) ref])
      (tree-read! self object label #:current curr
                  #:predecessor pred #:successor succ)))
  (for ([ref (method-outflow method-decl)])
    (match-let ([(reference object label) ref])
      (tree-write! self object label #:current curr))))

(define (eval! self expr #:current [curr #f] #:virtual [virt #f]
              #:predecessor [pred #f] #:successor [succ #f])
  (define/match (recur expr)
    [((? integer?))
     (void)]
    [(`(! ,expr))
     (recur expr)]
    [(`(+ ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(- ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(* ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(/ ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(< ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(<= ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(== ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(!= ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(>= ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(> ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(&& ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(\|\| ,left-expr ,right-expr))
     (recur left-expr)
     (recur right-expr)]
    [(`(ite ,cond-expr ,then-expr ,else-expr))
     (recur cond-expr)
     (recur then-expr)
     (recur else-expr)]
    [(`(call ,(symbol fun) ,arg-exprs))
     (for-each recur arg-exprs)]
    [((reference object label))
     (tree-read! self object label #:current curr #:virtual virt
                 #:predecessor pred #:successor succ)])
  (recur expr))
  
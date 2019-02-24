#lang rosette

(require "tree.rkt")

(provide statements
         interpret
         check
         writes
         reads)

; A DSL for simple statements to schedule in a tree traversal.

; The statements that need to be scheduled.
(define statements
  '(() ; no-op
    ([self . x] := [self . y])
    ([self . y] := [left . x])
    ([self . z] := [right . z])))

(define (deref self ref)
  (let ([object (car ref)]
        [label (cdr ref)])
    (define node
      (cond
        [(eq? object 'self) self]
        [(eq? object 'left) (inner-left self)]
        [(eq? object 'right) (inner-right self)]))
    (cond
      [(eq? label 'x) (node-x node)]
      [(eq? label 'y) (node-y node)]
      [(eq? label 'z) (node-z node)])))

(define (read self ref)
  (let* ([cell (deref self ref)]
         [value (if (box? cell) (unbox cell) cell)])
    (assert (not (void? value)))
    value))

(define (write self ref val)
  (let ([cell (deref self ref)])
    (assert (void? (unbox cell)))
    (set-box! cell val)))

(define (interpret self s)
  (unless (null? s) ; skip no-op statements
    (let ([lhs (first s)]
          [rhs (third s)])
      (write self lhs (read self rhs)))))

(define (writes self s)
  (if (null? s)
      null
      (list (deref self (first s)))))

(define (reads self s)
  (if (null? s)
      null
      (let ([rhs (deref self (third s))])
        (if (box? rhs)
            (list rhs)
            null))))

(define (check self)
  (when (inner? self)
    (assert (not (void? (unbox (node-x self)))))
    (assert (not (void? (unbox (node-y self)))))
    (assert (not (void? (unbox (node-z self)))))
    (check (inner-left self))
    (check (inner-right self))))

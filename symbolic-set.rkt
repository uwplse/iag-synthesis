#lang rosette

(provide make-symbolic-set
         symbolic-set?
         symbolic-set-universe
         symbolic-set-update!
         symbolic-set-insert!
         symbolic-set-remove!
         symbolic-set-clear!
         symbolic-set-copy
         symbolic-set-size
         symbolic-set->list)

; Like the usual Racket member, but returns information that is actually useful:
; the element's index in the list (or #f if not found).
(define (index-of x xs [same? equal?])
  (define (recurse i xs)
    (cond
      [(null? xs) #f]
      [(same? (first xs) x) i]
      [else (recurse (+ i 1) (rest xs))]))
  (recurse 0 xs))

(struct symbolic-set (universe membership))

(define (make-symbolic-set . universe)
  (let ([membership (make-vector (length universe) 0)])
    (symbolic-set universe membership)))

(define (symbolic-set-update! set element contains)
  (let* ([universe (symbolic-set-universe set)]
         [membership (symbolic-set-membership set)]
         [index (index-of element universe)])
    (assert index)
    (vector-set! membership index (if contains 1 0))))

(define (symbolic-set-insert! set element)
  (symbolic-set-update! set element #t))

(define (symbolic-set-remove! set element)
  (symbolic-set-update! set element #f))

(define (symbolic-set-clear! set)
  (vector-fill! (symbolic-set-membership set) 0))

(define (symbolic-set-copy set)
  (let ([universe (symbolic-set-universe set)]
        [membership (symbolic-set-membership set)])
    (symbolic-set universe (vector-copy membership))))

(define (symbolic-set-size set)
  (let ([membership (symbolic-set-membership set)])
    (apply + (vector->list membership))))

(define (symbolic-set->list set)
  (let ([universe (symbolic-set-universe set)]
        [membership (symbolic-set-membership set)])
    (for/list ([element universe]
               [bit membership]
               #:when (= bit 1))
      element)))

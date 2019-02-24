#lang rosette

(provide make-partition
         partition-add!
         partition-all?
         partition-any?
         partition-both?)

; A partition is a simple abstract data type whose primary operation is a test
; to determine whether all (or any) elements in a given set belong to the same
; class (i.e., grouping) in the partition. Elements are assumed to be equal?-
; distinguishable, and classes are assumed to be disjoint.

(struct partition (mapping classes) #:mutable)

(define (test->hash test)
  (cond
    [(equal? test equal?) make-hash]
    [(equal? test eq?) make-hasheq]
    [(equal? test eqv?) make-hasheqv]
    [else (raise-user-error 'partition "unsupported equivalence test")]))

(define (make-partition  #:same same . classes)
  (let ([part (partition ((test->hash same)) 0)])
    (for-each (curry partition-add! part) classes)
    part))

(define (partition-add! part elements)
  (let ([mapping (partition-mapping part)]
        [number (partition-classes part)])
    (for ([element elements])
      (hash-set! mapping element number))
    (set-partition-classes! part (+ number 1))))

(define (partition-all? part elements)
  (let* ([mapping (partition-mapping part)]
         [numbers (map (curry hash-ref mapping) elements)])
    (= (length (remove-duplicates numbers)) 1)))

(define (partition-any? part elements)
  (let* ([mapping (partition-mapping part)]
         [numbers (map (curry hash-ref mapping) elements)])
    (< (length (remove-duplicates numbers)) (length numbers))))

(define (partition-both? part element1 element2)
  (let* ([mapping (partition-mapping part)])
    (= (hash-ref mapping element1) (hash-ref mapping element2))))

#lang s-exp rosette

(require racket/dict)

(provide lookup
         defined
         unique?
         carmap
         cdrmap
         domain
         mapthunk
         choose*
         decide*
         number*
         hole*)

; --------------------------
; Association list functions
; --------------------------

; whether each key in an association list has been uniquely assigned
(define (unique? alist)
  (not (not (check-duplicates alist eq? #:key car))))

(define (defined alist symbol)
  (not (not (member (cons symbol (void))
                    alist
                    (λ (b0 b1) ; equate two bindings
                      (eq? (car b0) (car b1)))))))

; lookup the value of a symbol in an association list or void
(define (lookup alist symbol)
  (match (assoc symbol alist eq?)
    [(cons _ value) value]
    [#f (void)]))

; ------------------------
; Random utility functions
; ------------------------

(define (hash-modify! hash key modifier default)
  (if (hash-has-key? hash key)
      (hash-update! hash key modifier)
      (hash-set! hash key (modifier default))))

(define (hash-modify hash key modifier default)
  (if (hash-has-key? hash key)
      (hash-update hash key modifier)
      (hash-set hash key (modifier default))))

(define (carmap f xs)
  (map (λ (p)
         (cons (f (car p))
               (cdr p)))
       xs))

(define (cdrmap f xs)
  (map (λ (p)
         (cons (car p)
               (f (cdr p))))
       xs))

(define (zip xs ys)
  (for/list ([x xs]
             [y ys])
    (cons x y)))

(define (curry f xs)
  (λ ys (apply f (append xs ys))))

(define (mapthunk f n)
    (letrec ([rec (lambda (i) (if (= i 0)
                                  null
                                  (cons (f)
                                        (rec (- i 1)))))])
      (rec n)))

(define (repeat x n)
    (letrec ([rec (λ (i) (if (= i 0)
                             null
                             (cons x
                                   (rec (- i 1)))))])
      (rec n)))

; -------------------------
; Enhanced solver interface
; -------------------------

; Choose an expression dynamically
(define (choose* . xs)
  (define-symbolic* i number?)
  (assert (>= i 0))
  (assert (< i (length xs)))
  (list-ref xs i))

; Dynamically create a decision/boolean
(define (decide*)
  (define-symbolic* b boolean?)
  b)

; Dynamically create a number
(define (number* a b)
  (define-symbolic* n number?)
  (assert (>= n a))
  (assert (< n b))
  n)

; Dynamically create a constant hole
(define (hole*)
  (define-symbolic* h number?)
  h)

; Return the "domain" of the solver
(define domain
  (thunk
   (let ([n (expt 2 (- (current-bitwidth) 1))])
     (list
      (- n)
      (- n 1)))))
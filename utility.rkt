#lang rosette

(require rosette/lib/angelic)

(provide assoc-lookup
         associated?
         assoc-unique?
         assoc-group
         assoc-map-keys
         carmap
         cdrmap
         domain
         mapthunk
         choose*
         boolean*
         integer*
         number*)

; --------------------------
; Association list functions
; --------------------------

; whether each key in an association list has been uniquely assigned
(define (assoc-unique? alist)
  (not (not (check-duplicates alist eq? #:key car))))

; whether a given symbol bound to (associated with) some value
(define (associated? alist symbol)
  (not (not (memf (λ (binding)
                    (eq? symbol (car binding)))
                  alist))))

; lookup the value of a symbol in an association list; if the symbol is unbound,
; then return the value of failure-result (or its invocation if a procedure)
(define (assoc-lookup alist symbol [failure-result void])
  (match (assoc symbol alist eq?)
    [(cons _ value) value]
    [#f (if (procedure? failure-result)
            (failure-result)
            failure-result)]))

; transform a list into a list associating symbols with sublists using the given
; key projection function
(define (assoc-group key list)
  (map (λ (group)
         (cons (key (car group))
               group))
       (group-by key list eq?)))

(define (assoc-map-keys f alist)
  (map (λ (p)
         (let ([k (car p)])
           (cons k (f k))))
         alist))

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

; Dynamically create a symbolic boolean
(define (boolean*)
  (define-symbolic* b boolean?)
  b)

; Dynamically create a symbolic integer
(define (integer*)
  (define-symbolic* n integer?)
  n)

; Dynamically create a symbolic ("real") number
(define (number*)
  (define-symbolic* n real?)
  n)

; Return the "domain" of the solver
(define domain
  (thunk
   (let ([n (expt 2 (- (current-bitwidth) 1))])
     (list
      (- n)
      (- n 1)))))

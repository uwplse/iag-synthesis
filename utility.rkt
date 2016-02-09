#lang s-exp rosette

(require racket/dict)

(provide (struct-out symtab)
         make-symtab
         symtab-keys
         symtab-ref
         symtab-set!
         symtab-map!
         domain
         mapthunk
         choose*
         decide*
         number*
         hole*)

; ---------------------
; Symbol (lookup) table
; ---------------------

(struct symtab (symbols lookup table) #:transparent)

(define (make-symtab symbols default)
  (define (make-lookup symbols index)
    (if (null? symbols)
        (λ (_) -1) ; symbol not found
        (let ([symbol (first symbols)]
              [recurse (make-lookup (rest symbols) (+ index 1))])
          (λ (input)
            (if (eq? input symbol)
                index
                (recurse input))))))
  (let* ([size (length symbols)]
         [lookup (make-lookup symbols 0)]
         [table (make-vector size default)])
    (symtab symbols lookup table)))

(define (symtab-keys symtab)
  (symtab-symbols symtab))

(define (symtab-ref symtab
                    symbol
                    [default (thunk
                              (error "symbol not found: "
                                     (symbol->string symbol)))])
  (let ([index ((symtab-lookup symtab) symbol)])
    (if (eq? index -1)
        (if (procedure? default)
            (default)
            default)
        (vector-ref (symtab-table symtab) index))))

(define (symtab-set! symtab
                     symbol
                     value)
  (let ([index ((symtab-lookup symtab) symbol)]
        [table (symtab-table symtab)])
    (if (eq? index -1)
        (error "symbol not found: "
               (symbol->string symbol))
        (vector-set! table index value))))

(define (symtab-map! symtab
                     function)
  (let ([symbols (symtab-symbols symtab)]
        [lookup (symtab-lookup symtab)]
        [table (symtab-table symtab)])
    (for-each
     (λ (p)
       (let ([i (cdr p)])
         (vector-set! table
                      i
                      (function (car p) (vector-ref table i)))))
     (zip symbols (in-naturals)))))

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

(define (zip xs ys)
  (for/list ([x xs]
             [y ys])
    (cons x y)))

(define (lookup k alist)
  (cdr (assoc k alist eq?)))

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
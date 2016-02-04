#lang s-exp rosette

(require racket/dict)

(provide symbol-table?
         (struct-out symbol-table)
         make-symbol-table
         domain
         mapthunk
         choose*
         decide*
         number*
         hole*)

; --------------------
; Fixed-key dictionary
; --------------------

(define (vector-iterate-first vec)
  (dict-iterate-first vec))

(define (vector-iterate-next vec pos)
  (dict-iterate-next vec pos))

(define (vector-iterate-key vec pos)
  (dict-iterate-key vec pos))

(define (vector-iterate-value vec pos)
  (dict-iterate-key vec pos))

; dictionary implementation optimized for symbolic evaluation
(struct symbol-table (index store)
  #:transparent
  #:methods gen:dict
  [(define (dict-ref symtab symbol [default (λ () (raise exn:fail:contract))])
     (vector-ref (symbol-table-store symtab)
              (hash-ref (symbol-table-index symtab)
                        symbol
                        default)))
   (define (dict-set _0 _1 _2)
     (raise exn:fail:contract))
   (define (dict-set! symtab symbol value)
     (vector-set! (symbol-table-store symtab)
               (hash-ref (symbol-table-index symtab)
                         symbol
                         (thunk (raise exn:fail:contract)))
               value))
   (define (dict-remove _0 _1)
     (raise exn:fail:contract))
   (define (dict-remove! _0 _1)
     (raise exn:fail:contract))
   (define (dict-count symtab)
     (vector-length (symbol-table-store symtab)))
   (define (dict-copy symtab)
     (symbol-table (symbol-table-index symtab)
                (vector-copy (symbol-table-store symtab))))
   (define (dict-values symtab)
     (vector->list (symbol-table-store symtab)))
   (define (dict-iterate-first symtab)
     (vector-iterate-first (symbol-table-index symtab)))
   (define (dict-iterate-next symtab pos)
     (vector-iterate-next (symbol-table-index symtab) pos))
   (define (dict-iterate-key symtab pos)
     (vector-iterate-key (symbol-table-index symtab) pos))
   (define (dict-iterate-value symtab pos)
     (vector-ref (symbol-table-store symtab)
                 (vector-iterate-value (symbol-table-index symtab)
                                       pos)))])

(define (make-symbol-table symbols default)
  (let* ([size (length symbols)]
         [index (make-immutable-hasheq (zip symbols (in-naturals)))]
         [store (make-vector size default)])
    (symbol-table index store)))

(define symbol-table-ref dict-ref)

(define symbol-table-set! dict-set!)

(define symbol-table-copy dict-copy)

(define symbol-table-size dict-count)

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
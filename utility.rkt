#lang rosette

; Helper functions and common initialization.

(require racket/struct)

(provide (all-defined-out))

(current-bitwidth #f)

(define (oracle type)
  (match type
    ['int
     (define-symbolic* int integer?)
     int]
    ['bool
     (define-symbolic* bool boolean?)
     bool]))

(define (symbol-append . xs)
  (string->symbol (string-append* (map symbol->string xs))))

(define symbol-upcase
  (compose string->symbol string-upcase symbol->string))

(define symbol-downcase
  (compose string->symbol string-downcase symbol->string))

; If it isn't a list, make it a singleton list.
(define (listify x)
  (if (list? x) x (list x)))

(define (map-or-app f x)
  (if (list? x) (map f x) (f x)))

(define (const* f) (thunk* (f)))

; Compute the approximate size of a formula, where each unique occurrence of an
; AST node counts as 1. Returns two values: the number of unique AST nodes in the
; assertion store and the number of unique variables in the assertion store.
(define (formula-size)
  (let ([subformulae (mutable-seteq)]
        [nodes 0]
        [variables 0])
    (define (recurse term)
      (unless (set-member? subformulae term)
        (set-add! subformulae term)
        (match term
          [(expression _ terms ...)
           (set! nodes (+ nodes 1))
           (for-each recurse terms)]
          [(constant name _)
           (set! variables (+ variables 1))]
          [_
           (void)])))
    (for-each recurse (asserts))
    (values nodes variables)))

(define-syntax-rule (-- x)
  (begin
    (set! x (- x 1))
    x))

(define-syntax-rule (++ x)
  (begin
    (set! x (+ x 1))
    x))

(define (associate-by key val lst [same? equal?])
  (map (λ (g) (cons (key (first g)) (val g))) (group-by key lst same?)))

(define (vector-sum vec)
  (apply + (vector->list vec)))

(struct matrix (height width rows) #:transparent) ; row-major vector representation

(define (build-matrix n m f)
  (matrix n m
          (build-vector n (λ (i) (build-vector m (λ (j) (f i j)))))))

(define (matrix-ref mat i j)
  (vector-ref (vector-ref (matrix-rows mat) i) j))

(define (matrix-transpose mat)
  (let ([n (matrix-height mat)]
        [m (matrix-width mat)])
    (matrix m n
            (for/vector #:length m ([j (in-range m)])
              (for/vector #:length n ([i (in-range n)])
                (matrix-ref mat i j))))))

(define (matrix-columns mat)
  (matrix-rows (matrix-transpose mat)))

(define (struct-map f str)
  (let-values ([(str-typ _) (struct-info str)])
    (apply (struct-type-make-constructor str-typ)
           (map f (struct->list str)))))

(define-syntax-rule (push! name expr)
  (set! name (cons expr name)))

(define-syntax-rule (pop! name)
  (begin0
    (first name)
    (set! name (rest name))))

(define-syntax shadow!
  (syntax-rules ()
    [(shadow! ([name expr]) body ...)
     (let ([initial name])
       (set! name expr)
       (begin0
         (begin body ...)
         (set! name initial)))]
    [(shadow! (binder binders ...) body ...)
     (shadow! (binder) (shadow! (binders ...) body ...))]))
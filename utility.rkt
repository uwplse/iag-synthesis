#lang rosette

; Helper functions and common initialization.

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

#lang rosette

; Helper functions and common initialization.

(provide (all-defined-out))

(current-bitwidth #f)

(define (symbol-append . xs)
  (string->symbol (string-append* (map symbol->string xs))))

; If it isn't a list, make it a singleton list.
(define (listify x)
  (if (list? x) x (list x)))

; Compute the approximate size of a formula, where each unique occurrence of an
; AST node counts as 1. Returns two values: the number of unique AST nodes in the
; assertion store and the number of unique variables in the assertion store.
; FIXME: Does pruning of common subexpressions in constraints actually make sense?
; I think probably not for arithmetic equations, at the very least.
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

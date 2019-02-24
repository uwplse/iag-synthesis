#lang rosette

(require rosette/lib/angelic)

; This is how one would naively approach task scheduling with symbolic evaluation.

(define a (box (void)))
(define b (box (void)))
(define c (box (void)))
(define d (box (void)))
(define e (box (void)))
(define f (box (void)))

(define tasks
  '((a := 0)
    (b := 1)
    (c := a)
    (d := c)
    (e := b)
    (f := b)))

(define (lookup variable)
  (match variable
    ['a a]
    ['b b]
    ['c c]
    ['d d]
    ['e e]
    ['f f]
    [_ (box variable)]))

(define (concrete-interpret task)
  ; NOTE: This explicit symbolic reflection is necessary.
  (for*/all ([output (first task)]
             [input (third task)])
    (let ([target (lookup output)]
          [source (lookup input)])
      (assert (void? (unbox target)))
      (assert (not (void? (unbox source))))
      (set-box! target (unbox source)))))

; Check that all memory cells (a-f) are initialized.
(define (check)
  (assert (not (void? a)))
  (assert (not (void? b)))
  (assert (not (void? c)))
  (assert (not (void? d)))
  (assert (not (void? e)))
  (assert (not (void? f))))

(define program tasks)

(define program* (for/list ([_ tasks]) (apply choose* tasks)))

(define (test program)
  (for ([task program])
    (concrete-interpret task))
  (check) ; put here so constraints show up in (asserts)
  (let ([model (solve #t)])
    (displayln (if (sat? model) "SAT" "UNSAT"))
    (for ([hole program]
          [index (in-naturals)])
      (printf "program[~a] = ~a\n" index (evaluate hole model)))))

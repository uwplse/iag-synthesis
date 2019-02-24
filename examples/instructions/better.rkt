#lang rosette

(require "../../evaluation/trace.rkt")

; This is an example of the same task scheduling problem but using our cool
; trace reasoning.

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
    [_ #t]))

(define (abstract-interpret task)
  (let ([target (first task)]
        [source (third task)])
    (trace-read (lookup source))
    (trace-write (lookup target))
    (trace-step)))

(define program tasks)

(define program* (for/list ([_ tasks]) (apply trace-choose tasks)))

(define (test program)
  (trace-start)
  (for ([task program])
    (trace-branch abstract-interpret task))
  (trace-stop (list #t) (list a b c d e f))
  (let ([schedule (trace-solve)])
    (displayln (if schedule "SAT" "UNSAT"))
    (for ([hole program]
          [index (in-naturals)])
      (printf "program[~a] = ~a\n" index (schedule hole)))))

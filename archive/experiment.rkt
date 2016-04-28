#lang rosette

;; Incrementality experiment

(require "../utility.rkt")

; Sum List pseudo-grammar:
; SumI -> Sum [terms : Term] (sum : int)
;   terms.sum := fold 0 .. terms$-.sum + terms$i.add
;   self.sum = terms[$].sum
; TermI -> Term (add : int) (sum  : int)

(struct sumlist
  (sum
   term
   next
   ) #:transparent #:mutable)

(define (

(define (random-data n k)
  (if (= n 0)
      null
      (mcons (random k)
             (random-sumlist))

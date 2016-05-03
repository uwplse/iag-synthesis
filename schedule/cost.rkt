#lang rosette

; Functional Tree Language (FTL) intepreter
; Schedule DSL Cost Model

(require "../core/utility.rkt"
         "../core/grammar.rkt"
         "syntax.rkt")

(provide ftl-sched-weight)

; -------------------
; Schedule cost model
; -------------------

; weight the approximate cost of a schedule on a particular tree
(define (ftl-sched-weight grammar schedule tree)
  (void))

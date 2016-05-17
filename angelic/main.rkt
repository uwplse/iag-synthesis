#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Angelic Interpreter (End-to-end Evaluator)

(require "../grammar/parse.rkt"
         "../grammar/generate.rkt"
         "../grammar/runtime.rkt"
         "constrain.rkt")

(provide ftl-angelic-interpret
         ftl-angelic-evaluate)

; interpret : L(G_FTL) * L([[L(G_FTL)]]) -> L([[L(G_FTL)]])
(define (ftl-angelic-interpret ftl-port tree #:runtime [runtime ftl-base-runtime])
  (ftl-angelic-evaluate runtime
                        (ftl-ir-generate (ftl-ast-parse ftl-port) runtime)
                        tree))

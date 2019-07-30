#lang rosette

(require "src/grammar/syntax.rkt"
         "src/grammar/parse.rkt"
         "src/backend/generate.rkt"
         "src/backend/printer.rkt"
         racket/pretty)

(define G (file->grammar "benchmarks/css/toy.grammar"))
(define S
  (ag:traversal 'layout
                (list (ag:visitor (ag:grammar-ref/class G 'Block)
                                  (list (ag:eval '(self . underflow))
                                        ;(ag:eval '(self . mb_clear))
                                        ;(ag:eval '(self . mb_clear_prev))
                                        ;(ag:eval '(self . height))
                                        (ag:iter/left 'children
                                                      (list (ag:eval '(self . intrinsic_height))
                                                            (ag:recur 'children))))))))
(define P (generate-program G S))
(print-program P)

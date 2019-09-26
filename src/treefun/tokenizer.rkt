#lang racket

(require brag/support
         "lexer.rkt")

(provide make-tokenizer)

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (λ () (lex port)))

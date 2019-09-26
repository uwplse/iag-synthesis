#lang racket

(require syntax/strip-context
         "tokenizer.rkt"
         "parser.rkt")

(provide read-syntax
         read-datum)

(define (read-syntax path port)
  (define syntax-tree (parse path (make-tokenizer port path)))
  (strip-context
   #`(module attribute-grammar "expander.rkt"
       #,syntax-tree)))

(define (read-datum path port)
  (parse-to-datum (make-tokenizer port path)))

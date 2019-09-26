#lang racket

(provide (rename-out [module-begin #%module-begin]))

(define-syntax-rule (module-begin definition ...)
  #`(#%module-begin definition ...))

(define-syntax-rule (define-traversal id visitor ...)
  #f)

(define-syntax-rule (define-interface id field ...)
  #f)

(define-syntax-rule (define-trait id component ...)
  #f)

(define-syntax-rule (define-class id (trait ...) interface component ...)
  #f)
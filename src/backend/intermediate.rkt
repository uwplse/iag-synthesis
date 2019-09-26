#lang rosette

(require "../utility.rkt"
         "../grammar/syntax.rkt")

(provide (prefix-out ir: (all-defined-out)))

(struct foreach (child rev? body) #:transparent)
(struct declare (lhs rhs) #:transparent)
(struct assign (lhs rhs) #:transparent)
(struct invoke (method) #:transparent)
(struct skip () #:transparent)
(define statement? (disjoin foreach? declare? assign? invoke? skip?))

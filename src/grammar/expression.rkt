#lang rosette

(provide (prefix-out ex: (except-out (all-defined-out)
                                     make-field)))

; XXX: Delete in favor of `expression` module
; terms
(struct const (value) #:transparent)
(struct field (attribute) #:transparent #:constructor-name make-field)
(struct field/get field () #:transparent)
(struct field/cur field () #:transparent)
(struct field/acc field () #:transparent)
(struct field/sup field () #:transparent)
(struct field/peek field (default) #:transparent)
(struct field/pred field/peek () #:transparent)
(struct field/succ field/peek () #:transparent)
(struct field/first field (default) #:transparent)
(struct field/last field (default) #:transparent)
(struct length (child) #:transparent)
(struct logic (operator operands) #:transparent)
(struct order (comparison left right) #:transparent)
(struct arith (operator operands) #:transparent)
(struct call (function arguments) #:transparent)
(struct invoke (receiver method arguments) #:transparent)
(struct branch (if then else) #:transparent)
(define term? (disjoin const? field? length? logic? arith? call? invoke? branch?))

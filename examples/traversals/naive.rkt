#lang rosette

(require rosette/lib/angelic)
(require "tree.rkt")
(require "statements.rkt")

(provide traverse)

(define traverse1 (apply choose* statements))
(define traverse2 (apply choose* statements))
(define traverse3 (apply choose* statements))
(define traverse4 (apply choose* statements))

(define (traverse self)
  (when (inner? self)
    (interpret self traverse1)
    (traverse (inner-left self))
    (interpret self traverse2)
    (traverse (inner-right self))
    (interpret self traverse3)
    (interpret self traverse4)))

(define (test)
  (traverse large-tree)
  (check large-tree)
  (let ([solution (solve #t)])
    (printf "traverse[1]: ~a\n" (evaluate traverse1 solution))
    (printf "traverse[2]: ~a\n" (evaluate traverse2 solution))
    (printf "traverse[3]: ~a\n" (evaluate traverse3 solution))
    (printf "traverse[4]: ~a\n" (evaluate traverse4 solution))))

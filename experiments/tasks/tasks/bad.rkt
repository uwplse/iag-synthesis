#lang rosette

(require rosette/lib/synthax)

; schedule synthesis by existential validation
;
; This is the naive approach to synthesis, where a schedule validator is defined
; and the synthesis problem is posed as whether a schedule sketch can be
; completed such that it is valid.
;
; In terms of tracing, this method directly traces the interpretation of all
; feasible schedules

; check-trace : Deps -> Trace -> Bool
; trace-sched : Sched -> Tree -> Trace[ -> Bool]?

; the most obvious way to represent and check a schedule
(define (validate sched depends)
  ; list of all attributes
  (define attributes (map car depends))

  ; check that each attribute is computed before its dependencies
  (define state
    (for/fold ([state null])
              ([attr sched])
      (for/all ([attr attr])
        (begin
          (assert (not (memq attr state)))
          (for ([dep (cdr (assq attr depends))])
            (assert (memq dep state)))))
      (cons attr state)))

  ; check that every attribute was computed
  (for ([attr attributes])
    (assert (memq attr state))))

(define dependencies
  '((a . ())
    (b . (a))
    (c . (a b))
    (d . (a b c))
    (e . (a b c d))
    (f . (a b c d e))
    (g . (a b c d e f))))

(define schedule
  '(a b c d e f g))

(define sketch
  (list (choose 'a 'b 'c 'd 'e 'f 'g)
        (choose 'a 'b 'c 'd 'e 'f 'g)
        (choose 'a 'b 'c 'd 'e 'f 'g)
        (choose 'a 'b 'c 'd 'e 'f 'g)
        (choose 'a 'b 'c 'd 'e 'f 'g)
        (choose 'a 'b 'c 'd 'e 'f 'g)
        (choose 'a 'b 'c 'd 'e 'f 'g)))

(define (synthesize)
  (validate sketch dependencies)
  ; dump the assertion store, if you dare (139 not-so-concise constraints)
  ;(displayln (asserts))
  (let ([solution (solve #t)])
    ; dump the solution model
    (displayln solution)
    (evaluate sketch solution)))

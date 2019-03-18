#lang rosette

; Checking synthesizer for language of tree traversal schedules

(require rosette/lib/angelic
         racket/promise
         "../schedule/enumerate.rkt"
         "../utility.rkt"
         "../tree.rkt"
         "interpreter.rkt")

(provide complete-sketch)

(define (multichoose* n xs)
  (build-list n (thunk* (apply choose* xs))))

(define (complete-sketch grammar hole-range sketch examples)
  (let ([schedule (instantiate-sketch multichoose* hole-range grammar sketch)]
        [initial-time (current-milliseconds)])

    (for ([tree examples])
      (with-handlers ([exn:fail? (const #f)])
        (let* (;; [allocate (λ (store name)
               ;;             (cons (cons name (box #f)) store))]
               ;; [initialize (λ (store name type)
               ;;               (set-box! (lookup name store) #t)
               ;;               store)]
               ;; [validate (λ (store name)
               ;;             (assert (unbox (lookup name store))))]
               [allocate (λ (record label) record)]
               [initialize (λ (record label type) (cons (cons label #t) record))]
               [validate (λ (record label) (assert (lookup record label)))]
               [tree (tree-annotate grammar tree (const null) allocate initialize)])
          (interpret grammar schedule tree)
          (tree-validate grammar tree validate))))

    (match-let-values ([(running-time) (- (current-milliseconds) initial-time)]
                       [(nodes variables) (formula-size)]
                       [((list model) overhead-time solving-time _)
                        (time-apply force (list (delay (solve #t))))])
      (displayln (if (sat? model) "SAT" "UNSAT"))
      (printf "Symbolic Evaluation: ~ams\n" (+ running-time overhead-time))
      (printf "Constraint Solving: ~ams\n" (- solving-time overhead-time))
      (printf "Constraint size: ~a nodes and ~a variables\n" nodes variables)
      (and (sat? model) (evaluate schedule model)))))

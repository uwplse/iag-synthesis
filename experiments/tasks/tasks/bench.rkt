#lang rosette

(require (prefix-in ex: "constraints/existential.rkt")
         (prefix-in co: "constraints/compositional.rkt")
         (prefix-in as: "constraints/associative.rkt")
         "synthesis.rkt"
         rosette/lib/angelic)

(define (random-dependencies number degree)
  (define v (make-vector number))
  (for ([n (in-range number)])
    (vector-set! v
                 n
                 (cons (gensym "attribute") ; 'attribute####
                       (for/list ([_ (in-range (min degree n))])
                         (car (vector-ref v (random n)))))))
  (asserts)
  v)

(define (vector-average vec)
  (/ (for/fold ([sum 0])
               ([add vec])
       (+ sum add))
     (vector-length vec)))

(define (mean xs)
  (/ (apply + xs) (length xs)))

(define (stddev xs)
  (let* ([mu (mean xs)]
         [dev (λ (x) (- x mu))]
         [n (length xs)])
    (sqrt (/ (apply + (map (compose sqr dev) xs))
             n))))

(define (print-statistics tasks dependencies)
  (define number (vector-length tasks))
  (define indegrees (make-vector number 0))
  (define longest-paths (make-vector number 0))

  ; iterate over the dependency graph in topological order (i.e., backwards)
  (for ([source (in-range (- number 1) -1 -1)])
    (let ([longest (vector-ref longest-paths source)])
      (for ([dependency (cdr (vector-ref dependencies source))])
        (let ([target (vector-memq dependency tasks)])
          (vector-set! indegrees target (+ (vector-ref indegrees target) 1))
          (when (< (vector-ref longest-paths target) (+ longest 1))
            (vector-set! longest-paths target (+ longest 1)))))))

  (printf "number of vertices: ~a\n" (vector-length tasks))

  (printf "maximum longest path: ~a\n" (vector-argmax identity longest-paths))
  (printf "average longest path: ~a\n" (vector-average longest-paths))

  (printf "maximum indegree: ~a\n" (vector-argmax identity indegrees))
  (printf "average indegree: ~a\n" (vector-average indegrees)))

(define (vector-assq v vec)
  (define (recurse i)
    (cond
      [(eq? (vector-length vec) i) #f]
      [(eq? (car (vector-ref vec i)) v) (vector-ref vec i)]
      [else (recurse (+ i 1))]))
  (recurse 0))

(define (close-transitivity dependency-map)
  (for ([i (in-range (vector-length dependency-map))])
    (match-define (cons task dependencies) (vector-ref dependency-map i))
    (define closure
      (remove-duplicates
       (append*
        (for/list ([dependency dependencies])
          (let ([closure (cdr (vector-assq dependency dependency-map))])
            (cons dependency closure))))
       eq?))
    (vector-set! dependency-map i (cons task closure))))

(define (benchmark-transitive-closure size outdegrees sets)
  (solve #t) ; intialize the solver
  (printf "size: ~a\n" size)
  (for ([outdegree outdegrees])
    (printf "outdegree: ~a\n" outdegree)
    (define times
      (for/list ([_ (in-range sets)])
        (let* ([dependencies (random-dependencies size 3)]
               [tasks (vector-map car dependencies)]
               [sketch (build-list (vector-length tasks) (const '?))])
          ; control benchmark
          (clear-asserts!)
          (let ([tracer (co:init-tracer #f #t #t tasks)])
            (trace-symbolic tracer dependencies)
            (verify-schedule tracer sketch))
          (match-define-values ((list control-solution) _ control-time _)
                               (time-apply (thunk (solve #t)) null))
          (when (unsat? control-solution)
            (displayln "UNSAT!!!"))
          ; variable benchmark
          (clear-asserts!)
          (let ([tracer (co:init-tracer #f #t #t tasks)])
            ; augment the dependency graph with the transitive closure
            (close-transitivity dependencies)
            (trace-symbolic tracer dependencies)
            (verify-schedule tracer sketch))
          (match-define-values ((list variable-solution) _ variable-time _)
                               (time-apply (thunk (solve #t)) null))
          (when (unsat? variable-solution)
            (displayln "UNSAT!!!"))
          (cons control-time variable-time))))
    (define control-times (map car times))
    (printf "(control) mean: ~a min: ~a max: ~a stddev: ~a\n"
            (mean control-times)
            (apply min control-times)
            (apply max control-times)
            (stddev control-times))
    (define variable-times (map cdr times))
    (printf "(variable) mean: ~a min: ~a max: ~a stddev: ~a\n"
            (mean variable-times)
            (apply min variable-times)
            (apply max variable-times)
            (stddev variable-times))))

(define (benchmark-connectivity deps-per-task size sets)
  (solve #t) ; intialize the solver
  (printf "size: ~a\n" size)
  (for ([num-deps deps-per-task])
    (define times
      (for/list ([_ (in-range sets)])
        (define dependencies (random-dependencies size num-deps))
        (define tasks (vector-map car dependencies))
        (define sketch (build-list (vector-length tasks) (const '?)))
        (transitively-close dependencies)
        (clear-asserts!)
        (define-values (_1 _2 constrain-time _3)
          (time-apply (thunk
                       (let ([tracer (co:init-tracer #f #t #t tasks)])
                         (trace-symbolic tracer dependencies)
                         (verify-schedule tracer sketch)))
                      null))
        (match-define-values ((list solution) _ solve-time _)
                             (time-apply (thunk (solve #t)) null))
        ;            (clear-asserts!)
        (when (unsat? solution)
          (displayln "UNSAT!!!"))
        (cons solve-time constrain-time)))
    (define solve-times (map car times))
    (printf "(~a:solve) mean: ~a stddev: ~a\n"
            num-deps
            (mean solve-times)
            (stddev solve-times))
    (define constrain-times (map cdr times))
    (printf "(~a:constrain) mean: ~a stddev: ~a\n"
            num-deps
            (mean constrain-times)
            (stddev constrain-times))))

(define (benchmark-tracers sizes runs)
  (define tracer-makers
    (list (cons "∃step+UIF"
                (curry ex:init-tracer #t #f #t))
          (cons "∃task+UIF"
                (curry ex:init-tracer #f #t #t))
          (cons "∃step+∃task+UIF"
                (curry ex:init-tracer #t #t #t))
          (cons "task→task+UIF"
                (curry co:init-tracer #f #t #t))
          (cons "step→step+UIF"
                (curry co:init-tracer #f #t #t))
          (cons "task→task+step→step+UIF"
                (curry co:init-tracer #t #t #t))
          (cons "∃step"
                (curry as:init-tracer 'step #t))
          (cons "∃task"
                (curry as:init-tracer 'task #t))))
  (for ([size sizes])
    (solve #t) ; intialize the solver
    (printf "size: ~a\n" size)
    (define dependencies (random-dependencies size 3))
    (define tasks (vector-map car dependencies))
    (define sketch (build-list (vector-length tasks) (const '?)))
    (for ([tracer-maker tracer-makers])
      (match-let* ([(cons name make-tracer) tracer-maker])
        (define times
          (for/list ([_ (in-range runs)])
            (clear-asserts!)
            (define-values (_1 _2 constrain-time _3)
              (time-apply (thunk
                           (let ([tracer (make-tracer tasks)])
                             (trace-symbolic tracer dependencies)
                             (verify-schedule tracer sketch)))
                          null))
            (match-define-values ((list solution) _ solve-time _)
                                 (time-apply (thunk (solve #t)) null))
;            (clear-asserts!)
            (when (unsat? solution)
              (displayln "UNSAT!!!"))
            (cons solve-time constrain-time)))
        (define solve-times (map car times))
        (printf "(~a:solve) mean: ~a stddev: ~a\n"
                name
                (mean solve-times)
                (stddev solve-times))
        (define constrain-times (map cdr times))
        (printf "(~a:constrain) mean: ~a stddev: ~a\n"
                name
                (mean constrain-times)
                (stddev constrain-times))))))

(define (make-sketch-with-symbolic-choices task-vector)
  (let ([task-list (vector->list task-vector)])
    (build-list (vector-length task-vector)
                (thunk* (apply choose* task-list)))))

(define (make-sketch-with-syntactic-holes task-vector)
  (build-list (vector-length task-vector) (const '?)))

(define (make-sketch-with-syntactic-choices task-vector)
  (let ([task-list (vector->list task-vector)])
    (build-list (vector-length task-vector) (const task-list))))

(define (benchmark-sketches sizes runs)
  (define sketch-makers
    (list (cons "symbolic choices" make-sketch-with-symbolic-choices)
          (cons "syntactic holes" make-sketch-with-syntactic-holes)
          (cons "syntactic choices" make-sketch-with-syntactic-choices)))
  (for ([size sizes])
    (solve #t) ; intialize the solver
    (printf "size: ~a\n" size)
    (define dependencies (random-dependencies size 3))
    (define tasks (vector-map car dependencies))
    (for ([sketch-maker sketch-makers])
      (match-let* ([(cons name make-sketch) sketch-maker]
                   [sketch (make-sketch tasks)])
        (define times
          (for/list ([_ (in-range runs)])
            (clear-asserts!)
            (define-values (_1 _2 constrain-time _3)
              (time-apply (thunk
                           (let ([tracer (as:init-tracer 'task #t tasks)])
                             (trace-symbolic tracer dependencies)
                             (verify-schedule tracer sketch)))
                          null))
            (match-define-values ((list solution) _ solve-time _)
                                 (time-apply (thunk (solve #t)) null))
            ;            (clear-asserts!)
            (when (unsat? solution)
              (displayln "UNSAT!!!"))
            (cons solve-time constrain-time)))
        (define solve-times (map car times))
        (printf "(~a:solve) mean: ~a stddev: ~a\n"
                name
                (mean solve-times)
                (stddev solve-times))
        (define constrain-times (map cdr times))
        (printf "(~a:constrain) mean: ~a stddev: ~a\n"
                name
                (mean constrain-times)
                (stddev constrain-times))))))

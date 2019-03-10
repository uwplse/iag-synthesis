#lang rosette

; Benchmark driver for schedule synthesis and verification.

(require "grammar/parse.rkt"
         "grammar/syntax.rkt"
         "grammar/typecheck.rkt"
         "schedule/syntax.rkt"
         "schedule/parse.rkt"
         "schedule/enumerate.rkt"
         "tree.rkt"
         (prefix-in tracing: "tracing/interpreter.rkt")
         (prefix-in tracing: "tracing/synthesizer.rkt")
         (prefix-in checking: "checking/interpreter.rkt")
         (prefix-in checking: "checking/synthesizer.rkt"))

(provide (all-from-out "grammar/syntax.rkt")
         (all-from-out "schedule/syntax.rkt")
         (all-from-out "schedule/enumerate.rkt")
         (all-from-out "checking/interpreter.rkt")
         (all-from-out "checking/synthesizer.rkt")
         (all-from-out "tracing/interpreter.rkt")
         (all-from-out "tracing/synthesizer.rkt")
         (all-from-out "tree.rkt")
         read-grammar
         display-grammar
         read-schedule
         display-schedule
         read-tree
         synthesize-schedules
         elaborate-schedule)

; Read and parse the attribute grammar in the named file, returning the list of
; class ASTs (interface ASTs unnecessary for interpretation).
(define (read-grammar filename [root 'Root])
  (let* ([port (open-input-file filename #:mode 'text)]
         [grammar (ag-normalize root (ag-parse port))])
    (close-input-port port)
    (ag-typecheck grammar)
    grammar))

(define (display-grammar grammar)
  (displayln (ag-serialize grammar)))

; Read and parse the tree traversal schedule in the named file, returning the
; AST.
(define (read-schedule filename)
  (let* ([port (open-input-file filename #:mode 'text)]
         [schedule (sched-parse port)])
    (close-input-port port)
    schedule))

(define (display-schedule schedule)
  (displayln (sched-serialize schedule)))

(define (read-tree grammar filename)
  (xml->tree grammar (file->string filename #:mode 'text)))

(define (synthesize-schedules grammar examples passes synthesizer)
  (let ([hole-range (make-hole-range grammar)]
        [sketches (enumerate-sketches grammar passes)])
    (for ([sketch (shuffle sketches)])
      (printf "\n\n\n--------------------------------\n")
      (displayln sketch)
      (synthesizer grammar hole-range sketch examples)
      (clear-asserts!))))

(define (elaborate-schedule grammar schedule)
  (match schedule
    [(sched-sequential left right)
     `(sequential
       ,(elaborate-schedule grammar left)
       ,(elaborate-schedule grammar right))]
    [(sched-parallel left right)
     `(parallel
       ,(elaborate-schedule grammar left)
       ,(elaborate-schedule grammar right))]
    [(sched-traversal order visitors)
     (let ([template (ag-traversal-forms (get-traversal grammar order))])
       `(traverse ,order (match . ,(elaborate-visitors template visitors))))]))

(define (elaborate-visitors template visitors)
  (for/list ([visitor visitors])
    (let* ([class-name (car visitor)]
           [blocks (box (cdr visitor))]
           [schema (cdr (assoc class-name template))])
      `(case ,class-name ,(elaborate-blocks schema blocks)))))

(define (elaborate-blocks schema blocks)
  `(do .
       ,(append*
         (for/list ([form schema])
           (match form
             [(ag-trav-visit)
              (let ([block (first (unbox blocks))])
                (set-box! blocks (rest (unbox blocks)))
                (for/list ([statement block])
                  (match statement
                    [(sched-slot-skip) `skip]
                    [(sched-slot-eval object label) `(eval ,object ,label)])))]
             [(ag-trav-loop child schema)
              (list `(loop child ,(elaborate-blocks schema blocks)))]
             [(ag-trav-recur child)
              (list `(recur ,child))])))))

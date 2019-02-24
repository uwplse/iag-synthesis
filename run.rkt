#lang rosette

; Benchmark driver for schedule synthesis and verification.

(require "grammar/parse.rkt"
         "grammar/syntax.rkt"
         "grammar/typecheck.rkt"
         "schedule/syntax.rkt"
         "schedule/parse.rkt"
         "enumeration.rkt"
         "generation.rkt"
         "tree.rkt"
         "benchmarks/hvbox/forest.rkt"
         "benchmarks/sunburst/forest.rkt"
         "benchmarks/treemap/forest.rkt"
         (prefix-in tracing: "tracing/interpreter.rkt")
         (prefix-in tracing: "tracing/synthesizer.rkt")
         (prefix-in checking: "checking/interpreter.rkt")
         (prefix-in checking: "checking/synthesizer.rkt"))

(provide (all-defined-out))

(define (tracing:test-hvbox)
  (tracing:synthesize-schedule hvbox-grammar hvbox-skeleton hvbox-forest))

(define (tracing:test-sunburst)
  (tracing:synthesize-schedule sunburst-grammar sunburst-skeleton sunburst-forest))

(define (tracing:test-treemap)
  (tracing:synthesize-schedule treemap-grammar treemap-skeleton treemap-forest))

(define (checking:test-hvbox)
  (checking:synthesize-schedule hvbox-grammar hvbox-skeleton hvbox-forest))

(define (checking:test-sunburst)
  (checking:synthesize-schedule sunburst-grammar sunburst-skeleton sunburst-forest))

(define (checking:test-treemap)
  (checking:synthesize-schedule treemap-grammar treemap-skeleton treemap-forest))

; Read and parse the attribute grammar in the named file, returning the list of
; class ASTs (interface ASTs unnecessary for interpretation).
(define (read-grammar filename [root 'Root])
  (let* ([port (open-input-file filename #:mode 'text)]
         [grammar (ag-normalize root (ag-parse port))])
    (close-input-port port)
    (ag-typecheck grammar)
    grammar))

; Read and parse the tree traversal schedule in the named file, returning the
; AST.
(define (read-schedule filename)
  (let* ([port (open-input-file filename #:mode 'text)]
         [schedule (sched-parse port)])
    (close-input-port port)
    schedule))

(define (read-tree grammar filename)
  (xml->tree grammar (file->string filename #:mode 'text)))

(define hvbox-schedule (read-schedule "benchmarks/hvbox/hvbox.sched"))

(define hvbox-grammar (read-grammar "benchmarks/hvbox/hvbox.ftl"))

(define hvbox-skeleton
  (sched-comp 'seq
              (sched-trav 'post `((Root) (HBox) (VBox) (Leaf)))
              (sched-trav 'pre `((Root) (HBox) (VBox) (Leaf)))))

(define hvbox-skeleton-parallel
  (sched-comp 'par hvbox-skeleton hvbox-skeleton))

(define hvbox-forest
  ; NOTE: The below is literally the cached output from this procedure.
  ;(synthesize-forest hvbox-grammar 'Root)
  (map (curry annotate-tree hvbox-grammar) hvbox-skeleton-forest))

;(define sunburst-schedule (read-schedule "benchmarks/sunburst/sunburst.sched"))

(define sunburst-grammar (read-grammar "benchmarks/sunburst/sunburst.ftl"))

(define sunburst-skeleton
  (sched-comp 'seq
              (sched-trav 'pre `((Root) (Radial) (Leaf)))
              (sched-trav 'pre `((Root) (Radial) (Leaf)))))

(define sunburst-forest
  ; NOTE: The below is literally the cached output from this procedure.
  ;(synthesize-forest sunburst-grammar 'Root)
  (map (curry annotate-tree sunburst-grammar) sunburst-skeleton-forest))

;(define treemap-schedule (read-schedule "benchmarks/treemap/treemap.sched"))

(define treemap-grammar (read-grammar "benchmarks/treemap/treemap.ftl"))

(define treemap-skeleton
  (let* ([visits '((Root)
                   (CountryContainer)
                   (Region)
                   (District)
                   (VSquare)
                   (HSquare)
                   (PollingPlace))]
         [pre (sched-trav 'pre visits)]
         [post (sched-trav 'post visits)])
    (sched-comp 'seq
                pre
                (sched-comp 'seq
                            post
                            pre))))

(define treemap-forest
  ; NOTE: The below is literally the cached output from this procedure.
  ;(synthesize-forest treemap-grammar 'Root)
  (map (curry annotate-tree treemap-grammar) treemap-skeleton-forest))

(define (synthesize-schedules grammar forest enumerator bound synthesizer)
  (for ([skeleton (enumerator grammar bound)])
    (printf "\n\n\n--------------------------------\n")
    (displayln skeleton)
    (synthesizer grammar skeleton forest)
    (clear-asserts!)))

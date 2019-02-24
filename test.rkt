#lang rosette

; Benchmark driver for schedule synthesis and verification.

(require "engine.rkt"
         (prefix-in tracing: "tracing/interpreter.rkt")
         (prefix-in tracing: "tracing/synthesizer.rkt")
         (prefix-in checking: "checking/interpreter.rkt")
         (prefix-in checking: "checking/synthesizer.rkt"))

(provide (all-defined-out))

(define (tracing:test-hvbox)
  (tracing:complete-sketch hvbox-grammar (make-hole-range hvbox-grammar)
                           hvbox-skeleton hvbox-examples))

(define (tracing:test-sunburst)
  (tracing:complete-sketch sunburst-grammar (make-hole-range sunburst-grammar)
                           sunburst-skeleton sunburst-examples))

(define (tracing:test-treemap)
  (tracing:complete-sketch treemap-grammar (make-hole-range treemap-grammar)
                           treemap-skeleton treemap-examples))

(define (checking:test-hvbox)
  (checking:complete-sketch hvbox-grammar (make-hole-range hvbox-grammar)
                            hvbox-skeleton hvbox-examples))

(define (checking:test-sunburst)
  (checking:complete-sketch sunburst-grammar (make-hole-range sunburst-grammar)
                            sunburst-skeleton sunburst-examples))

(define (checking:test-treemap)
  (checking:complete-sketch treemap-grammar (make-hole-range treemap-grammar)
                            treemap-skeleton treemap-examples))

(define hvbox-grammar (read-grammar "benchmarks/hvbox/hvbox.grammar" 'Top))

(define hvbox-examples (tree-examples hvbox-grammar))

(define hvbox-schedule (read-schedule "benchmarks/hvbox/hvbox.sched"))

(define hvbox-skeleton
  (sched-sequential (sched-traversal 'post
                                     `((Root ,(sched-hole))
                                       (HBox ,(sched-hole) ,(sched-hole))
                                       (VBox ,(sched-hole) ,(sched-hole))
                                       (Leaf ,(sched-hole))))
                    (sched-traversal 'pre
                                     `((Root ,(sched-hole))
                                       (HBox ,(sched-hole))
                                       (VBox ,(sched-hole))
                                       (Leaf ,(sched-hole))))))

(define hvbox-skeleton-parallel
  (sched-parallel hvbox-skeleton hvbox-skeleton))

(define sunburst-grammar (read-grammar "benchmarks/sunburst/sunburst.grammar" 'IRoot))

(define sunburst-examples (tree-examples sunburst-grammar))

(define sunburst-skeleton
  (sched-sequential (sched-traversal 'pre
                                     `((Root ,(sched-hole))
                                       ;(Radial ,(sched-hole) ,(sched-hole) ,(sched-hole))
                                       (Radial ,(sched-hole) ,(sched-hole))
                                       (Leaf ,(sched-hole))))
                    (sched-traversal 'pre
                                     `((Root ,(sched-hole))
                                       ;(Radial ,(sched-hole) ,(sched-hole) ,(sched-hole))
                                       (Radial ,(sched-hole) ,(sched-hole))
                                       (Leaf ,(sched-hole))))))

(define treemap-grammar (read-grammar "benchmarks/treemap/treemap.grammar" 'IRoot))

(define treemap-examples (tree-examples treemap-grammar))

(define treemap-schedule (read-schedule "benchmarks/treemap/treemap.sched"))

(define treemap-skeleton
  (let* ([visitors `((Root ,(sched-hole))
                     (CountryContainer ,(sched-hole) ,(sched-hole))
                     (Region ,(sched-hole) ,(sched-hole))
                     (District ,(sched-hole) ,(sched-hole))
                     (VSquare  ,(sched-hole) ,(sched-hole))
                     (HSquare  ,(sched-hole) ,(sched-hole))
                     (PollingPlace  ,(sched-hole)))]
         [pre (sched-traversal 'pre visitors)]
         [post (sched-traversal 'post visitors)])
    (sched-sequential pre (sched-sequential post pre))))

;; (define minicss-grammar (read-grammar "benchmarks/css/minicss.grammar"))

;; (define minicss-skeleton
;;   (let* ([visitors `((Root ,(sched-hole))
;;                      (CountryContainer ,(sched-hole) ,(sched-hole))
;;                      (Region ,(sched-hole) ,(sched-hole))
;;                      (District ,(sched-hole) ,(sched-hole))
;;                      (VSquare  ,(sched-hole) ,(sched-hole))
;;                      (HSquare  ,(sched-hole) ,(sched-hole))
;;                      (PollingPlace  ,(sched-hole)))]
;;          [zip (sched-traversal 'zip visitors)]
;;          ;[pre (sched-traversal 'pre visitors)]
;;          [post (sched-traversal 'post visitors)])
;;     ; SKETCH 1: pre; post; pre; post; post_zip; pre
;;     ; where post_zip traverses the children of BlockImg, NormalBlock, FlowBlock, and Root
;;     ; in sequemntial order, interleaving local actions
;;     ; SKETCH 2: pre; pre; pre; post; pre; pre; post; pre_zip; pre
;;     ; where pre_zip interleaves local actions with the traversal of children for every class,
;;     ; I guess?
;;     (sched-sequential zip post)))




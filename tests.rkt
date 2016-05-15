#lang rosette

(require rosette/lib/synthax
         racket/pretty
         "core/runtime.rkt"
         "core/tree.rkt"
         "compile/parse.rkt"
         "compile/generate.rkt"
         "schedule/syntax.rkt"
         "schedule/parse.rkt"
         "schedule/evaluate.rkt"
         "angelic/constrain.rkt")

(define runtime ftl-base-runtime)

(define (read-grammar filename)
  (let* ([port (open-input-file filename #:mode 'text)]
         [grammar (ftl-ir-generate runtime (ftl-ast-parse port))])
    (close-input-port port)
    grammar))

(define (read-tree grammar sentence filename)
  (xml->ftl-tree runtime
                 grammar
                 sentence
                 (port->string (open-input-file filename #:mode 'text))))

(define hvbox-grammar
  (read-grammar "examples/hvbox.ftl"))

(define hvbox-tree
  (read-tree hvbox-grammar 'Top "examples/hvbox.xml"))

(define hvbox-schedule
  (ftl-sched-parse (open-input-file "examples/hvbox.sched"
                                    #:mode 'text)))
(define points-grammar
  (read-grammar "examples/points.ftl"))

(define points-tree
  (read-tree points-grammar 'Root "examples/points.xml"))

(define points-schedule
  (ftl-sched-parse (open-input-file "examples/points.sched")))

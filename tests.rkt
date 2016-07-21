#lang rosette

(require rosette/lib/synthax
         racket/pretty
         "grammar/runtime.rkt"
         "grammar/tree.rkt"
         "grammar/parse.rkt"
         "grammar/generate.rkt"
         "schedule/syntax.rkt"
         "schedule/parse.rkt"
         "schedule/evaluate.rkt"
         "schedule/constrain.rkt"
         "angelic/constrain.rkt"
         "examples/hvbox-sketch.rkt"
         "examples/treemap-sketch.rkt")

(current-bitwidth #f)

(define runtime ftl-base-runtime)

(define (read-grammar filename)
  (let* ([port (open-input-file filename #:mode 'text)]
         [grammar (ftl-ir-generate (ftl-ast-parse port))])
    (close-input-port port)
    grammar))

(define (read-tree grammar sentence filename)
  (xml->ftl-tree grammar
                 sentence
                 (port->string (open-input-file filename #:mode 'text))
                 #:runtime runtime))

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

(define treemap-grammar
  (read-grammar "examples/treemap.ftl"))

(define treemap-tree
  (read-tree treemap-grammar 'IRoot "examples/treemap.xml"))

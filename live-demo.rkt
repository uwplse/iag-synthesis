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
         "angelic/constrain.rkt"
         "draw.rkt")

; parse the HVBox layout spec from our attribute grammar DSL
(define grammar
  (ftl-ir-generate ftl-base-runtime
                   (ftl-ast-parse (open-input-file "examples/hvbox.ftl"
                                                   #:mode 'text))))

; parse a small tree that matches the HVBox attribute grammar
(define tree
  (xml->ftl-tree ftl-base-runtime
                 grammar
                 'Top
                 (port->string (open-input-file "examples/hvbox-small.xml"
                                                #:mode 'text))))

; show input tree
;(pretty-print tree)

; solve output tree
;(define output-tree
;  (ftl-angelic-evaluate ftl-base-runtime grammar tree))

; show symbolically annotated output tree
;(pretty-print tree)

; show evaluation constraints
;(pretty-print (asserts))

; show concretely annotated output tree
;(pretty-print output-tree)

; parse a schedule for HVBox from our schedule DSL
(define schedule
  (ftl-sched-parse (open-input-file "examples/hvbox.sched"
                                    #:mode 'text)))

; evaluate the same tree with a static schedule
;(ftl-schedule-evaluate! grammar tree schedule)
; show output tree
;(pretty-print tree)

; assert the validity of a schedule sketch on our example tree
;(ftl-schedule-validate ftl-base-runtime grammar sketch tree)
; take a peek at the constraints
;(asserts)
; synthesize a concrete schedule from the sketch
;(display (ftl-sched-serialize (evaluate sketch (solve #t))))

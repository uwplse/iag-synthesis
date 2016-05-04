#lang rosette

(require rosette/lib/synthax
         "core/runtime.rkt"
         "core/tree.rkt"
         "compile/parse.rkt"
         "compile/generate.rkt"
         "schedule/evaluate.rkt"
         "schedule/syntax.rkt"
         "angelic/constrain.rkt")

(define hvbox-grammar
  (let* ([port (open-input-file "examples/hvbox.ftl" #:mode 'text)]
         [grammar (ftl-ir-generate ftl-base-runtime
                                   (ftl-ast-parse port))])
    (close-input-port port)
    grammar))

(define hvbox-tree
  (let* ([port (open-input-file "examples/hvbox.xml" #:mode 'text)]
         [tree (xml->ftl-tree ftl-base-runtime
                              hvbox-grammar
                              'Top
                              (port->string port))])
    (close-input-port port)
    tree))

; QUESTION: do we want to parse ftl + xml files interactively?
; show input tree
; > hvbox-tree
; solve output tree
; > (define output-hvbox-tree
; >   (ftl-angelic-evaluate ftl-base-runtime hvbox-grammar hvbox-tree))
; show symbolically annotated output tree
; > hvbox-tree
; show evaluation constraints
; > (asserts)
; show concretely annotated output tree
; > output-hvbox-tree

; TODO: parse the below schedule from pretty sched file
; evaluate the same tree with a static schedule
; > (define output-hvbox-tree
; >   (ftl-schedule-evaluate hvbox-grammar hvbox-tree hvbox-sched))

(define hvbox-sched
  (ftl-sched-seq
   (ftl-sched-trav 'post
                   `(((Top . Root) . ())
                     ((HVBox . HBox) . (,(ftl-visit-iter 'childs
                                                         '((self . childsWidth)
                                                           (self . childsHeight)))
                                        ,(ftl-visit-eval '((self . width)
                                                           (self . height)))))
                     ((HVBox . VBox) . (,(ftl-visit-iter 'childs
                                                         '((self . childsWidth)
                                                           (self . childsHeight)))
                                        ,(ftl-visit-eval '((self . width)
                                                           (self . height)))))
                     ((HVBox . Leaf) . (,(ftl-visit-eval '((self . width)
                                                           (self . height)))))))
   (ftl-sched-trav 'pre
                   `(((Top . Root) . (,(ftl-visit-eval '((root . right)
                                                         (root . bottom)))))
                     ((HVBox . HBox) . (,(ftl-visit-iter 'childs
                                                         '((childs . right)
                                                           (childs . bottom)))))
                     ((HVBox . VBox) . (,(ftl-visit-iter 'childs
                                                         '((childs . right)
                                                           (childs . bottom)))))
                     ((HVBox . Leaf) . ())))))

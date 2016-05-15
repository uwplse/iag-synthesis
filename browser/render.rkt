#lang s-exp rosette

; model CSS layout engine, to experiment with incrementalization

(require rosette/lib/meta/meta
         rosette/lib/tools/render
         xml
         2htdp/image
         "utility.rkt"
         "grammar.rkt"
         "schedule.rkt")

(provide string->image)

(define example-html-string
"
<html>
  <body color=\"blue\">
     <div color=\"yellow\" width=\"100px\">
        <p><em>Hello there</em>, friend</p>
     </div>
  </body>
</html>
")

; -------------------------
; Data structures and types
; -------------------------

(define style '(left
                right
                top
                bottom
                width
                min-width
                max-width
                height
                min-height
                max-height
                line-height
                vertical-align
                float
                position
                color))

(define layout '(left
                 right
                 top
                 bottom
                 width
                 height
                 line-height
                 vertical-align
                 float
                 position
                 color))

; html --> node | string
(define (html? v)
  (or (node? v)
      (string? v)))

; ------------------
; Layout computation
; ------------------

(define (synthesize-width tree)
  (void))

; ---------------------------
; Data structure construction
; ---------------------------

(struct layout-attributes)

; canonicalize style attributes to layout attributes
(define (style->layout attributes)
  (void))

(define (xexpr->html xexpr)
  (if (list? xexpr)
      (let* ([attributed (dict? (cadr xexpr))]
             [attributes (if attributed
                             (cadr xexpr)
                             null)]
             [children (if attributed
                           (cddr xexpr)
                           (rest xexpr))])
        (node (car xexpr)
              attributes
              (map xexpr->html children)))
      (xexpr->string xexpr)))

; --------------------------
; Data structure computation
; --------------------------

(define (html-schedule tree)
  (solve (begin (assert #t)
                (assert #t))))

(define (html-evaluate tree)
  (void))

(define (html-render tree)
  (void))

; solve CSS attributes?
; angelically evaluate attributes of layout derivation tree
; render with pict or image libraries

(define xexpr->image
  (compose html-render html-evaluate html-schedule xexpr->html))

(define string->image
  (compose xexpr->image string->xexpr))

(define input->image
  (compose xexpr->image xml->xexpr read-xml/element))
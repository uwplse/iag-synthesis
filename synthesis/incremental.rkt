#lang s-exp rosette

; Functional Tree Language (FTL) intepreter
; Incrementality

(require "parse.rkt")

; Naively incrementalize the given AST
(define (naively-incrementalize ast-list)
  ; for each input alpha of the original AST,
  ;     generate input alpha_initial,
  ;              input alpha,
  ;              variable alpha_delta
  ; for each variable alpha of the original AST,
  ;     generate input alpha_initial,
  ;              variable alpha_dirty,
  ;              variable alpha_delta,
  ;              variable alpha
  (define (incrementalize-defn ast-decl)
    (let* ([input (ftl-ast-declare-input ast-decl)]
           [ident (ftl-ast-declare-name ast-decl)]
           [ident-string (symbol->string ident)]
           [ident_initial (string->symbol (string-append ident-string "_initial"))]
           [ident_dirty (string->symbol (string-append ident-string "_dirty"))]
           [ident_delta (string->symbol (string-append ident-string "_delta"))]
           [type (ftl-ast-declare-type ast-decl)])
      (if input
          (list (ftl-ast-declare #t ident_initial type)
                (ftl-ast-declare #t ident type)
                (ftl-ast-declare #f ident_delta boolean))
          (list (ftl-ast-declare #t ident_initial type)
                (ftl-ast-declare #f ident type)
                (ftl-ast-declare #f ident_dirty boolean)
                (ftl-ast-declare #f ident_delta boolean)))))
  ; for each definition alpha := f(beta,...) of the original AST,
  ;     generate alpha_dirty := and(beta_delta,...)
  ;              alpha := alpha_dirty ? f(beta,...) : alpha_initial
  ;              alpha_delta := alpha != alpha_initial
  (define (incrementalize-decl ast-defn)
    (void))
  ; Our incrementalized reduction is degenerate and still requires an iteration
  ; over the children.
  ;
  ; Propose definition of incremental FTL or Δ-FTL, that disposes of explicit loop
  ; constructs. Instead of loop childs { childs.a := f(b) }, childs.a := f(b), and
  ; instead of loop childs { self.a := fold e .. f(b) }, self.a := fold childs e .. f(b).

  (void))
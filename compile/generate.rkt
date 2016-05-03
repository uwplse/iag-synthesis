#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Intermediate Representation Generation

(require "../core/utility.rkt"
         "../core/syntax.rkt"
         "../core/grammar.rkt"
         "../core/runtime.rkt"
         "typecheck.rkt")

(provide ftl-ir-generate)

; TODO: generate Typed Racket syntax instead of lambdas

; Open questions:
; 1. Must a child sequence be non-empty? Thinking NO (cf. of fold semantics)
; 2. Can FTL grammars define an attribute outside of a loop in terms of first or
;    last children in a child sequence? Thinking NO (due to above)

; -------------------------------------
; Intermediate Representation Generator
; -------------------------------------

; translate parsed abstract syntax tree into intermediate representation
(define (ftl-ir-generate runtime ast-list)
  (if (void? (ftl-ast-conflicts? ast-list))
    (ftl-ast-compile (ftl-ast-typecheck (ftl-ast-validate (ftl-ast-inline ast-list))
                                        runtime))
    (error "top-level namespace conflict")))

; -------------------------------------------------
; Closure Compilation (Syntax-Directed Translation)
; -------------------------------------------------

; turn an AST attribute reference into an IR attribute dependency
(define (ftl-ast-refer->ftl-ir-dependency ref)
  (ftl-ir-dependency (ftl-ast-refer-object ref)
                     (ftl-ast-refer-index ref)
                     (ftl-ast-refer-label ref)))

; compile ("lambdify") a symbol-resolved expression AST to a closed lambda term
(define (ftl-ast-expr-compile depv expr)
  (let ([recurse (curry ftl-ast-expr-compile depv)])
    (match expr
      [(ftl-ast-expr-call fun params)
       (let ([arg-funs (map recurse params)])
         (λ (argv)
           (apply fun
                  (map (λ (f) (f argv)) arg-funs))))]
      [(ftl-ast-expr-cond cond then else)
       (let ([cond-fun (recurse cond)]
             [then-fun (recurse then)]
             [else-fun (recurse else)])
         (λ (argv)
           (if (cond-fun argv)
               (then-fun argv)
               (else-fun argv))))]
      [(? ftl-ast-refer?)
       (λ (argv)
         (vector-ref argv (vector-member expr depv)))]
      [else ; literal value
       (λ (_) expr)])))

; Convert an action from the AST to an object-label pair paired with an IR
; definition (('object . 'label) . ftl-ir-definition)
(define (ftl-ast-action-compile action)
  (define (make-evaluation rhs)
    (let ([depv (apply vector-immutable (ftl-ast-expr-depends rhs))])
      (ftl-ir-evaluation (ftl-ast-expr-compile depv rhs)
                         (vector-map ftl-ast-refer->ftl-ir-dependency depv))))

  (match action
    [(ftl-ast-loop child (ftl-ast-define lhs rhs))
     (cons (ftl-ast-refer->pair lhs)
           (ftl-ir-definition child
                              (match rhs
                                [(ftl-ast-expr-fold init step)
                                 (ftl-ir-reduction (make-evaluation init)
                                                   (make-evaluation step))]
                                [else
                                 (make-evaluation rhs)])))]
    [(ftl-ast-define lhs rhs)
     (cons (ftl-ast-refer->pair lhs)
           (ftl-ir-definition (void)
                              (make-evaluation rhs)))]))

(define (ftl-ast-body-compile ast-body)
  (match-let*
      ([(ftl-ast-body children attributes actions) ast-body]
       [inputs (filter-map (λ (decl)
                             (and (ftl-ast-declare-input decl)
                                  (ftl-ast-declare-name decl)))
                           attributes)]
       [labels (map (λ (decl)
                      (cons (ftl-ast-declare-name decl)
                            (ftl-ast-declare-type decl)))
                    attributes)]
       [singletons (filter-map (λ (child)
                                 (and (not (ftl-ast-child-sequence child))
                                      (cons (ftl-ast-child-name child)
                                            (ftl-ast-child-interface child))))
                               children)]
       [sequences (filter-map (λ (child)
                                (and (ftl-ast-child-sequence child)
                                     (cons (ftl-ast-child-name child)
                                           (ftl-ast-child-interface child))))
                              children)]
       [definitions (map ftl-ast-action-compile actions)])
    (ftl-ir-production inputs
                       labels
                       definitions
                       singletons
                       sequences)))

(define (ftl-ast-compile ast-map)
  (map (λ (mapping)
         (match-let* ([(cons symbol production) mapping])
           (cons symbol
                 (cdrmap ftl-ast-body-compile
                         (rest production)))))
       ast-map))

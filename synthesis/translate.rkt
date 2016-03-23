#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Translation (AST to IR with closed lambdas)

(require racket/dict
         "syntax.rkt"
         "parse.rkt"
         "typecheck.rkt"
         "runtime.rkt"
         "../utility.rkt")

(provide (struct-out ftl-ir-dependency)
         (struct-out ftl-ir-evaluation)
         (struct-out ftl-ir-reduction)
         (struct-out ftl-ir-definition)
         (struct-out ftl-ir-production)
         (struct-out ftl-ir-grammar)
         ftl-ir-translate ; AST -> IR
         example-ir)

; Open questions:
; 1. Must a child sequence be non-empty? Thinking NO (cf. of fold semantics)
; 2. Can FTL grammars define an attribute outside of a loop in terms of first or
;    last children in a child sequence? Thinking NO (due to above)

; ---------------------------
; Intermediate Representation
; ---------------------------

; FIXME: attributes defined by a fold should be treated as pseudo-vectors, such
; that both 'first, 'previous, 'current, and 'last are all allowed.

; Attribute dependencies with 'previous indices (e.g., object$-.alpha) valid if
; and only if the referenced attribute is defined by a fold in some loop context
; and the reference occurs in the context of a loop over the same child
; sequence. However, this condition is not currently currently checked along with
; most other index reference checks and must be delt with at interpretation time.
; The only check performed is whether an indexed attribute dependency happens in
; a proper loop context and is not recursive (except for 'previous, of course).

; attribute definition dependency
(struct ftl-ir-dependency
  (; singleton child name, sequence child name, or 'self
   object
   ; 'first, 'previous, 'current, 'last, or 'none
   index
   ; name of attribute on object
   label
   ) #:transparent)

; So the interpreters need a way to get ahold of the collection of all definitions that
; may need to iterate in lockstep, i.e., all definitions in identical loop contexts.
; Inherited definitions of attributes on the child sequence in question are easy to
; identify, but what about synthesized definitions on singleton objects? This is tricky...

; attribute evaluation
(struct ftl-ir-evaluation
  (; accepts dependency values packed in an argument vector
   function
   ; vector of IR dependencies
   dependencies ; TODO: maybe rename parameters? or inputs?
   ) #:transparent)

; attribute definition for a reduction (fold)
(struct ftl-ir-reduction
  (; IR evaluation for initial accumulator value
   init
   ; IR evaluation for next accumulator value
   step
   ) #:transparent)

; attribute definition
(struct ftl-ir-definition
  (; name of sequence child to iterate/reduce over or (void) if not applicable
   iterate
   ; IR evaluation or reduction to compute attribute
   evaluate
   ) #:transparent)

; production descriptor
(struct ftl-ir-production
  (; list of labels that must be supplied by the input derivation
   inputs
   ; list associating each label to its type (as a symbol)
   labels
   ; list associating each object-label pair to its IR definition; an individual
   ; association (pair) is sometimes referred to as an action
   definitions
   ; list associating each singleton child name with its symbol [in the implicit
   ; alphabet]
   singletons
   ; list associating each sequence child name with its symbol [in the implicit
   ; alphabet]
   sequences
   ) #:transparent)

; grammar descriptor
(struct ftl-ir-grammar
  (; list associating each symbol [of the implicit alphabet] to a list
   ; associating each option name to its production
   vocabulary
   ; starting symbol from the vocabulary
   sentence
   ) #:transparent)

; -------------------------------
; Grammar translation (AST -> IR)
; -------------------------------
(define (id x) x)
; Driver: translate parsed abstract syntax tree into intermediate representation
(define (ftl-ir-translate ast-list root runtime)
  (if (void? (ftl-ast-conflicts? ast-list))
    (ftl-ast-compile (ftl-ast-typecheck (ftl-ast-validate (ftl-ast-inline ast-list))
                                        runtime)
                     root)
    (error "top-level namespace conflict")))

; -------------------
; Closure compilation
; -------------------

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

(define (ftl-ast-compile ast-map root)
  (let* ([vocabulary (map (λ (mapping)
                            (match-let* ([(cons symbol production) mapping])
                              (cons symbol
                                    (cdrmap ftl-ast-body-compile
                                            (rest production)))))
                          ast-map)])
    (ftl-ir-grammar vocabulary root)))

; --------------------------
; Example Grammar in IR Form
; --------------------------

(define example-ir (ftl-ir-translate example-ast 'Root ftl-base-runtime))

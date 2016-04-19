#lang rosette

(require "../utility.rkt"
         "parse.rkt"
         "syntax.rkt"
         "runtime.rkt")

(provide ftl-ast-typecheck)

; TODO: can we disentangle typechecking, index-normalization, and
; symbol-resolution? and if so, is it worth doing? On the other hand, how can
; these functions be named so as to more effectively convey their generalized
; type-driven functionality? "ftl-ast-type-magic"?
; Note that symbol resolution also involves the substitution of function callss
; for unary and binary operators.

; Create an error thunk to say that the given name of the given kind was
; undefined. The name argument is assumed to be either a symbol or a pair of
; symbols. Useful for association list lookups.
(define (undefined kind name)
  (thunk (let ([occ (if (pair? name)
                        (string-append (symbol->string (car name))
                                       "."
                                       (symbol->string (cdr name)))
                        (symbol->string name))])
           (raise-arguments-error 'typecheck (string-append "undefined " kind)
                                  "occurrence" occ))))

; Resolve a unary operator to the proper presumably monomorphic definition
(define (resolve-unary runtime operator typename)
  (let* ([types (ftl-runtime-types runtime)]
         [type (assoc-lookup types
                             typename
                             (undefined "type" typename))]
         [unary (ftl-type-unary type)])
    (assoc-lookup unary
                  operator
                  (undefined "operator" operator))))

; Resolve a binary operator to the proper presumably monomorphic definition
(define (resolve-binary runtime operator typename)
  (let* ([types (ftl-runtime-types runtime)]
         [type (assoc-lookup types
                             typename
                             (undefined "type" typename))]
         [binary (ftl-type-binary type)]
         [eq (ftl-type-equal? type)]
         [lt (ftl-type-less? type)])
    ; desugar certain operators
    (match operator
      ['== eq]
      ['!= (compose not eq)]
      ['< lt]
      ['> (λ (x y)
            (and (not (lt x y))
                 (not (eq x y))))]
      ['<= (λ (x y)
             (or (lt x y)
                 (eq x y)))]
      ['>= (compose not lt)]
      [else (assoc-lookup binary
                          operator
                          (undefined "operator" operator))])))

; Produce an symbol-resolved, index-normalized form of a definition if it is
; well-typed; otherwise, raise an appropriate error. Together, symbol-resolution
; and index-normalization translate (resolve) all operators into function calls
; to the appropriate target symbol or definition, resolve all function symbols
; to the appropriate target symbol or definition, and inserts implicit current
; indices (e.g., children.attr => children$i.attr) in the right-hand side of the
; definition.
(define (ftl-ast-define-typecheck runtime typeof iterable? loop definition)
  (match-let* ([bool-type (ftl-runtime-boolean runtime)]
               [library (ftl-runtime-library runtime)]
               [(ftl-ast-define lhs rhs) definition]
               [(ftl-ast-refer lhs-object lhs-index lhs-label) lhs])
    ; resolve symbols and infer the type of an expression
    (define (recurse fold expr)
      (match expr
        ; unary operation
        [(ftl-ast-expr-unary oper subexpr)
         (match-let* ([(cons subexpr type) (recurse fold subexpr)]
                      [fun (resolve-unary runtime oper type)]
                      [args (list subexpr)])
           (cons (ftl-ast-expr-call fun args) type))]
        ; binary operation
        [(ftl-ast-expr-binary left-expr oper right-expr)
         (match-let* ([(cons left-expr left-type) (recurse fold left-expr)]
                      [(cons right-expr right-type) (recurse fold right-expr)]
                      [fun (resolve-binary runtime oper left-type)]
                      [args (list left-expr right-expr)]
                      [type (if (memq oper '(== != < > <= >=))
                                ; oper : type*type -> bool
                                bool-type
                                ; oper : type*type -> type
                                left-type)])
           (if (eq? left-type right-type)
               (cons (ftl-ast-expr-call fun args) type)
               (raise-arguments-error 'typecheck
                                      "binary operation on two different types:"
                                      "left" (symbol->string left-type)
                                      "right" (symbol->string right-type))))]
        ; conditional expression
        [(ftl-ast-expr-cond cond-expr then-expr else-expr)
         (match-let ([(cons cond-expr cond-type) (recurse fold cond-expr)]
                     [(cons then-expr then-type) (recurse fold then-expr)]
                     [(cons else-expr else-type) (recurse fold else-expr)])
           (if (eq? cond-type bool-type)
               (if (eq? then-type else-type)
                   ; do not translate conditionals into function calls, because
                   ; FTL assumes definitions have call-by-value semantics
                   (cons (ftl-ast-expr-cond cond-expr then-expr else-expr)
                         then-type) ; ite : bool*type*type -> type
                   (raise-arguments-error 'typecheck
                                          "cannot unify branches of conditional expression"
                                          "then branch" (symbol->string then-type)
                                          "else branch" (symbol->string else-type)))
               (raise-arguments-error 'typecheck
                                      "non-boolean condition expression"
                                      "type" (symbol->string cond-type))))]
        ; function invocation
        [(ftl-ast-expr-call symbol arg-exprs)
         (match-let* ([(cons sig fun) (assoc-lookup library
                                                    symbol
                                                    (undefined "function" symbol))]
                      [ar (- (length sig) 1)] ; arity
                      [par (map (curry recurse fold) arg-exprs)] ; parameters
                      [par-exprs (map car par)]
                      [par-types (map cdr par)]
                      [dom (take sig ar)] ; domain
                      [cod (last sig)]) ; codomain
           (if (eq? ar (length arg-exprs))
               (if (foldl && #t (map eq? dom par-types))
                   (cons (ftl-ast-expr-call fun par-exprs)
                         cod) ; fun : dom[*] -> cod
                   (raise-arguments-error 'typecheck
                                          "ill-typed function application"
                                          "function" (symbol->string symbol)))
               (raise-arguments-error 'typecheck
                                      "arity mismatch"
                                      "function" (symbol->string symbol)
                                      "arity" ar
                                      "arguments" (length arg-exprs))))]
        ; reduction
        [(ftl-ast-expr-fold init-expr step-expr)
         (match-let ([(cons init-expr init-type) (recurse 'init init-expr)]
                     [(cons step-expr step-type) (recurse 'step step-expr)])
           (if (or (eq? fold 'init)
                   (eq? fold 'step))
               (raise-arguments-error 'typecheck
                                      "nested fold")
               (if (void? loop)
                   (raise-arguments-error 'typecheck
                                          "fold outside of loop")
                   (if (eq? init-type step-type)
                       (cons (ftl-ast-expr-fold init-expr step-expr)
                             init-type) ; fold (i:type) .. (e:type) => v:type
                       (raise-arguments-error 'typecheck
                                              "initial and stepping expressions of fold with different types:"
                                              "init" (symbol->string init-type)
                                              "step" (symbol->string step-type))))))]
        ; attribute reference
        [(ftl-ast-refer object index label)
         (let* ([recursive (and (eq? object lhs-object)
                                (eq? label lhs-label))]
                [regular (and (not recursive)
                              (eq? index 'none)
                              (not (iterable? object)))]
                [extrema (and (not recursive)
                              (or (eq? index 'first) ; are these allowed outside a loop context over object?
                                  (eq? index 'last))
                              (iterable? object))]
                [looping (and (not recursive) ; normally indexed attribute references relative to loop context
                              (eq? loop object)
                              (or (eq? fold 'none)
                                  (eq? fold 'step))
                              (or (eq? index 'none)
                                  (eq? index 'first)
                                  (eq? index 'current)
                                  (eq? index 'last)))]
                [folding (and (eq? fold 'step)
                              (eq? index 'previous))])

           ; either return index-normalized attribute reference or raise an
           ; error, dumping relevant info
           (if (or regular extrema looping folding)
               (cons (ftl-ast-refer object
                                    (if (and (iterable? object)
                                             (eq? index 'none))
                                        'current
                                        index)
                                    label)
                     (typeof object label))
               (raise-arguments-error 'typecheck "bad attribute reference"
                                      "attribute" (ftl-ast-refer-serialize expr))))]
        ; value literal
        [else
         ; search for matching type predicate
         (let* ([types (ftl-runtime-types runtime)]
                [type (findf (λ (type)
                               ((ftl-type-predicate (cdr type)) expr))
                             types)])
           (if type
               (cons expr (car type))
               (raise-arguments-error 'typecheck "unknown literal value"
                                      "given" expr)))]))

      ; Check that the assignee reference is unindexed and to a sequence child iff
      ; this action is in a loop context over that sequence child
    (unless (and (eq? lhs-index 'none)
                 (eq? (eq? loop lhs-object) (iterable? lhs-object)))
      (raise-arguments-error 'typecheck
                             "invalid attribute definition (bad assignee reference)"))
    ; Initiate the traversal and check that the expression really is well-typed
    (match-let* ([expected (typeof lhs-object lhs-label)]
                 [(cons resolved inferred) (recurse 'none rhs)])
      (if (eq? expected inferred)
          (ftl-ast-define lhs resolved)
          (raise-arguments-error 'typecheck
                                 "ill-typed attribute definition"
                                 "expected" (symbol->string expected)
                                 "actual" (symbol->string inferred))))))

; Compute the type context (list of label-type pairs) of a list of declarations
(define (ftl-ast-declare-context decl-asts)
  (make-immutable-hasheq
   (map (λ (decl-ast)
          (cons (ftl-ast-declare-name decl-ast)
                (ftl-ast-declare-type decl-ast)))
        decl-asts)))

; Compute the type environment for a body AST
(define (ftl-ast-body-environ body-ast iface-contexts)
  (match-let* ([(ftl-ast-body children attributes actions) body-ast]
               [self-context (ftl-ast-declare-context attributes)])
    (make-immutable-hasheq
     (list* (cons 'self (cons #f self-context))
            (map (λ (child)
                   (match-let* ([(ftl-ast-child name sequence interface) child]
                                [child-context (hash-ref iface-contexts
                                                         interface
                                                         (undefined "interface"
                                                                    interface))])
                    (cons name (cons sequence child-context))))
                 children)))))

; Typecheck, index-normalize, and symbol-resolve a body AST
(define (ftl-ast-body-typecheck body-ast iface-contexts runtime)
  (match-let* ([(ftl-ast-body children attributes actions) body-ast]
               [environ (ftl-ast-body-environ body-ast iface-contexts)]
               [iterable? (λ (object)
                            (car (hash-ref environ
                                           object
                                           (undefined "child" object))))]
               [typeof (λ (object label)
                         (let ([context (cdr (hash-ref environ
                                                       object
                                                       (undefined "child" object)))])
                           (hash-ref context
                                     label
                                     (undefined "attribute" (cons object label)))))]
               [check-def (curry ftl-ast-define-typecheck
                                 runtime
                                 typeof
                                 iterable?)]
               [check-act (λ (action)
                            (match action
                              [(? ftl-ast-define?)
                               (list (check-def (void) action))]
                              [(ftl-ast-loop child actions)
                               (if (iterable? child)
                                   (map (λ (def)
                                          (ftl-ast-loop child
                                                        (check-def child def)))
                                        actions)
                                   (raise-arguments-error 'typecheck
                                                          "loop over non-sequence child"))]))])
    (ftl-ast-body children
                  attributes
                  (apply append (map check-act actions)))))

; Check for type and loop errors in each inlined class, returning the
; index-normalized, symbol-resolved form of the AST if it is well-typed
(define (ftl-ast-typecheck ast-map runtime)
  (let* ([iface-contexts (make-immutable-hasheq
                          (map (λ (mapping)
                                 (match-let* ([(list-rest symbol interface _) mapping]
                                              [attributes (ftl-ast-body-attributes interface)]
                                              [context (ftl-ast-declare-context attributes)])
                                   (cons symbol context)))
                               ast-map))])
    (ftl-ast-map-class (λ (body-ast)
                         (ftl-ast-body-typecheck body-ast iface-contexts runtime))
                       ast-map)))

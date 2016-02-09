#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Translation (AST to IR with closed lambdas)

(require racket/dict
         "parse.rkt"
         "runtime.rkt")

(provide translate ; AST -> IR
         (struct-out ftl-ir-dependency)
         (struct-out ftl-ir-reduction)
         (struct-out ftl-ir-definition)
         (struct-out ftl-ir-alternative)
         (struct-out ftl-ir-grammar)
         example-ir)

; ---------------------------
; Intermediate Representation
; ---------------------------

; FIXME: describe data structures in greater detail, with invariants

; names are symbols and dictionaries are immutable hash{eq,} tables

; dependency descriptor (same as ftl-ast-refer)
(struct ftl-ir-dependency
  (object ; singleton child name, sequence child name, or 'self
   index ; 'first, 'index, or 'last if object is a sequence; 'previous if accumulator within fold; otherwise ignored
   label ; name of attribute on object
   ) #:transparent)

; attribute descriptor
(struct ftl-ir-definition ; note that if this defines an attribute of a sequence child, it is implicitly looped
  (evaluate ; accepts dependency values packed in an argument vector
   dependencies ; vector of IR dependencies
   ) #:transparent)

(struct ftl-ir-reduction
  (init-define ; IR definition
   step-define ; IR definition
   iter-child ; name of sequence child being reduced
   ) #:transparent)

; alternative (element of production) descriptor (implicitly terminal or nonterminal)
(struct ftl-ir-alternative ; instances of a production
  (inputs ; list of labels that must be supplied by the input derivation
   labels ; dictionary of types keyed by labels
   definitions ; dictionary of IR definitions or reductions keyed by pairs of names of the object (including self) and its attribute
   singletons ; dictionary of of symbols keyed by child names, for singleton children ('self is reserved)
   sequences ; dictionary of symbols keyed by child names, for child sequences ('self is reserved)
   ) #:transparent)

; grammar descriptor (IR of an FTL program)
(struct ftl-ir-grammar
  (vocabulary ; dictionary of productions (dictionary of IR alternatives keyed by option names) keyed by symbols of the implicit alphabet
   sentence ; starting symbol (key) from vocabulary (for schedule synthesis and verification)
   ) #:transparent)

; -------------------------------
; Grammar Translation (AST -> IR)
; -------------------------------

; Driver: translate parsed abstract syntax tree into intermediate representation
(define (translate ftl-ast-list root runtime)
  (compile (check-types (validate (organize ftl-ast-list)) runtime) root runtime))

(define fail (thunk (error "Error: could not translate AST to IR")))

; TODO: refactor everything here on down

; 1. ORGANIZE - transform AST list into vocabulary, while
;    * inlining all inherited traits and implemented interfaces
;    * and ensuring that all namespaces are unambiguous
; 2. VALIDATE - check for duplicate declarations and/or definitions
; 3. TYPECHECK - check for type and loop errors (e.g., loop child1 { child2$i.attr := ... })
; 4. COMPILE CLOSURES - produce a lambda closure for each definition and an associated dependency list
; 5. EMIT - produce the final IR grammar

(define (ftl-ast-refer->pair ref)
  (cons (ftl-ast-refer-object ref)
        (ftl-ast-refer-label ref)))

(define (undefined-error-thunk kind name)
  (thunk (error (string-append "undefined "
                               kind
                               ": '"
                               (if (pair? name)
                                   (string-append (symbol->string (car name))
                                                  "."
                                                  (symbol->string (cdr name)))
                                   (symbol->string name))
                               "'"))))

(define (for-each-class function vocabulary)
  (hash-for-each vocabulary
                 (λ (symbol production)
                   (let ([classes (rest production)])
                     (for-each (compose function cdr) classes)))))

(define (map-class function vocabulary)
  (make-immutable-hasheq
   (hash-map vocabulary
             (λ (symbol production)
               (let ([iface (first production)]
                     [classes (rest production)])
                 (cons symbol
                       (cons iface
                             (map (λ (class)
                                    (cons (car class)
                                          (function (cdr class))))
                                  classes))))))))

(define (inline iface-list class-list trait-list)
  (let* ([trait-hash (make-immutable-hasheq
                      (map (λ (trait)
                             (cons (ftl-ast-trait-name trait)
                                   (ftl-ast-trait-body trait)))
                           trait-list))]
         [iface-hash (make-immutable-hasheq
                      (map (λ (iface)
                             (cons (ftl-ast-interface-name iface)
                                   (ftl-ast-body null
                                             (ftl-ast-interface-fields iface)
                                             null)))
                           iface-list))]
         [inline (λ (class)
                   (let* ([iface-name (ftl-ast-class-interface class)]
                          [iface-body (hash-ref iface-hash
                                                iface-name
                                                (undefined-error-thunk "interface" iface-name))]
                          [class-name (ftl-ast-class-name class)]
                          [class-body (ftl-ast-class-body class)]
                          [trait-names (ftl-ast-class-traits class)]
                          [trait-bodies (map (λ (trait-name)
                                               (hash-ref trait-hash
                                                         trait-name
                                                         (undefined-error-thunk "trait" trait-name)))
                                             trait-names)]
                          [bodies (list* class-body iface-body trait-bodies)])
                     (cons iface-name (cons class-name (apply ftl-ast-body-merge bodies)))))]
         [inlined (map inline class-list)]
         [grouped (group-by car inlined eq?)] ; requires Racket v6.3+
         [assoced (map (λ (group)
                         (list* (caar group) ; pull out one copy of the interface name
                                (hash-ref iface-hash (caar group)) ; hold onto a copy of the interface body
                                (map cdr group))) ; and leave the list of distinct class bodies
                       grouped)])
    (make-immutable-hasheq assoced)))


; Phase 1: return an inlined AST vocabulary from a list of AST top-level nodes while checking for
;  * namespace collisions between traits, interfaces, or classes
;  * the existence of all implemented interfaces
;  * the existence of an implementation for all interfaces
;  * the existence of traits inherited by a class
(define (organize ftl-ast-list)
  (let*-values ([(trait-list other-list) (partition ftl-ast-trait? ftl-ast-list)]
                [(class-list iface-list) (partition ftl-ast-class? other-list)])
    ; error if the trait, class, or interface namespaces are ambiguous (requires Racket v6.3+)
    (unless (not (check-duplicates trait-list eq? #:key ftl-ast-trait-name))
      (error "trait namespace ambiguous"))
    (unless (not (check-duplicates class-list eq? #:key ftl-ast-class-name))
      (error "class namespace ambiguous"))
    (unless (not (check-duplicates iface-list eq? #:key ftl-ast-interface-name))
      (error "interface namespace ambiguous"))

    (let* ([vocab (inline iface-list class-list trait-list)]
           [vocab-names (hash-keys vocab)]
           [iface-names (map ftl-ast-interface-name iface-list)])
      (unless (equal? (apply seteq vocab-names)
                      (apply seteq iface-names))
        (error "one or more interfaces lacks an implementation"))
      ; all done, return the AST vocabulary
      vocab)))


; Phase 2: check for certain semantic errors in the abstract syntax trees
;  * duplicate declarations within a class, its traits, and its interface
;  * duplicate definitions within a class and its traits
;  * definitions of undeclared traits between a class, its traits, and its interface
;  * the existence of traits inherited by a class
(define (validate vocabulary)
  (let* ([deloopify (λ (act)
                      (if (ftl-ast-loop? act)
                          (ftl-ast-loop-actions act)
                          act))]
         [check-duplicate-fields (λ (access identify equal has-duplicate)
                                   (for-each-class
                                    (λ (class)
                                      (let ([duplicate (check-duplicates (access class) ; requires Racket v6.3+
                                                                         equal
                                                                         #:key identify)])
                                        (when duplicate
                                          (has-duplicate class duplicate))))
                                    vocabulary))]
         [has-duplicate-define (λ (class def)
                                 (error (string-append
                                        "multiple definitions of attribute '"
                                        (symbol->string (ftl-ast-refer-object (ftl-ast-define-lhs def)))
                                        "."
                                        (symbol->string (ftl-ast-refer-label (ftl-ast-define-lhs def)))
                                        "' as a component of class '"
                                        (symbol->string (ftl-ast-class-name class))
                                        "'")))]
         [has-duplicate-declare (λ (class decl)
                                  (error (string-append
                                        "multiple declarations of attribute '"
                                        (symbol->string (ftl-ast-declare-name decl))
                                        "' as a component of class '"
                                        (symbol->string (ftl-ast-class-name class))
                                        "'")))]
         [has-duplicate-child (λ (class child)
                                (error (string-append
                                        "multiple declarations of child '"
                                        (symbol->string (ftl-ast-child-name child))
                                        "' as a component of class '"
                                        (symbol->string (ftl-ast-class-name class))
                                        "'")))])
    ; check that no attribute is defined more than once
    (check-duplicate-fields (compose flatten
                                     (curry map deloopify)
                                     ftl-ast-body-actions)
                            (compose ftl-ast-refer->pair
                                     ftl-ast-define-lhs)
                            equal?
                            has-duplicate-define)
    ; check that the same label was not used for multiple attributes
    (check-duplicate-fields ftl-ast-body-attributes
                            ftl-ast-declare-name
                            eq?
                            has-duplicate-declare)
    ; check that no child name was declared more than once
    (check-duplicate-fields ftl-ast-body-children
                            ftl-ast-child-name
                            eq?
                            has-duplicate-child)
    vocabulary))


(define (environment vocabulary)
  (let* ([context (λ (decls)
                    (make-immutable-hasheq
                     (map (λ (decl)
                            (cons (ftl-ast-declare-name decl)
                                  (ftl-ast-declare-type decl)))
                          decls)))]
         [iface-context (λ (symbol production)
                          (let ([iface (first production)])
                            (cons symbol
                                  (context (ftl-ast-body-attributes iface)))))]
         [iface-contexts (make-immutable-hasheq
                          (hash-map vocabulary iface-context))])
    (λ (body) ; given an ftl-ast-body, produce a pair of functions, iterable? and typeof
      (let* ([attributes (ftl-ast-body-attributes body)]
             [children (ftl-ast-body-children body)]
             [environ (make-immutable-hasheq
                       (list* (cons 'self (cons #f (context attributes)))
                              (map (λ (child)
                                     (cons (ftl-ast-child-name child)
                                           (cons (ftl-ast-child-sequence child)
                                                 (hash-ref iface-contexts
                                                           (ftl-ast-child-interface child)
                                                           (undefined-error-thunk "interface"
                                                                                  (ftl-ast-child-interface child))))))
                                   children)))])
        (cons (λ (object)
                (car (hash-ref environ object (undefined-error-thunk "child" object))))
              (λ (object label)
                (let ([context (cdr (hash-ref environ object (undefined-error-thunk "child" object)))])
                  (hash-ref context
                            label
                            (undefined-error-thunk "attribute" (cons object label))))))))))


(define (typecheck typeof signature value? iterable? bool-type loop definition) ; produce an index-normalized form of expr if well-typed
  (define lhs (ftl-ast-define-lhs definition))
  (define rhs (ftl-ast-define-rhs definition))
  (define (typeerror thingof type)
    (error (string-append "expected "
                          thingof
                          " type "
                          (symbol->string type))))
  (define (recurse fold expr type)
    (match expr
      [(ftl-ast-expr-call fun args) (let* ([sig (signature fun)]
                                       [res (last sig)]
                                       [arity (- (length sig) 1)])
                                  (unless (eq? arity (length args))
                                    (error (string-append "function arity mismatch: "
                                                          (symbol->string fun))))
                                  (unless (eq? res type)
                                    (typeerror "function returning" type))
                                  (ftl-ast-expr-call fun
                                                 (map (curry recurse fold) args (take sig arity))))]
      [(ftl-ast-expr-fold init step) (if fold
                                     (error "nested fold")
                                     (ftl-ast-expr-fold (recurse #f init type) ; FIXME: this must be an unindexed attribute/value
                                                    (recurse #t step type)))]
      [(ftl-ast-expr-cond cond then else) (ftl-ast-expr-cond (recurse fold cond bool-type)
                                                     (recurse fold then type)
                                                     (recurse fold else type))]
      [(? ftl-ast-refer?) (let* ([object (ftl-ast-refer-object expr)]
                             [label (ftl-ast-refer-label expr)]
                             [index (ftl-ast-refer-index expr)]
                             [recursive (and (eq? object (ftl-ast-refer-object lhs))
                                             (eq? label (ftl-ast-refer-label lhs)))]
                             [regular (and (not recursive)
                                           (or (eq? index 'none)
                                               (eq? index 'first) ; are sequence children always non-empty?
                                               (eq? index 'last))
                                           (not (iterable? object)))]
                             [looping (and (not recursive)
                                           (eq? loop object)
                                           (or (eq? index 'first)
                                               (eq? index 'index)
                                               (eq? index 'none)
                                               (eq? index 'last)))]
                             [folding (and fold recursive (eq? index 'previous))]
                             [welltyped (eq? type (typeof object label))])
                        (if (and welltyped (or regular looping folding))
                            (ftl-ast-refer object
                                       (if (and looping (eq? loop object) (eq? index 'none))
                                           'index ; normalize child.attr to child$i.attr in loops over child
                                           index)
                                       label)
                            (begin ; dump error data
                              (display "well-typed: ")
                              (displayln welltyped)
                              (display "regular: ")
                              (displayln regular)
                              (display "looping: ")
                              (displayln looping)
                              (display "folding: ")
                              (displayln folding)
                              (error (string-append (ftl-ast-refer-serialize expr)
                                                    " (in definition of "
                                                    (ftl-ast-refer-serialize lhs)
                                                    ") is ill-typed")))))]
      [else (if (value? expr type)
                expr
                (typeerror "value of" type))]))
  (let* ([object (ftl-ast-refer-object lhs)]
         [index (ftl-ast-refer-index lhs)]
         [label (ftl-ast-refer-label lhs)]
         [iter (iterable? object)])
    (if (or (and (eq? index 'none)
                     (or (eq? object loop)
                         (not iter)))
            (and (or (eq? index 'index)
                     (eq? index 'none))
                 (eq? object loop)
                 iter))
        (ftl-ast-define (ftl-ast-refer object
                               (if (and (eq? index 'none) (eq? object loop) iter)
                                   'index
                                   index)
                               label)
                    (recurse #f
                             rhs
                             (typeof object label)))
        (error "definition of invalidly indexed attribute on child or self"))))

; Phase 3: check for type and loop errors
(define (check-types vocabulary runtime)
  (letrec ([library (ftl-runtime-library runtime)]
           [types (ftl-runtime-types runtime)]
           [bool (ftl-runtime-boolean runtime)]
           [signature (λ (function)
                        (car (hash-ref library
                                       function
                                       (undefined-error-thunk "function" function))))]
           [value? (λ (v t) ; value * type -> boolean
                     ((car (hash-ref types t (undefined-error-thunk "type" t))) v))]
           [environ-body (environment vocabulary)])
    (map-class (λ (class) 
                 (let* ([children (ftl-ast-body-children class)]
                        [attributes (ftl-ast-body-attributes class)]
                        [actions (ftl-ast-body-actions class)]
                        [environ (environ-body class)]
                        [iterable? (car environ)]
                        [typeof (cdr environ)]
                        [check (curry typecheck typeof signature value? iterable? bool)])
                   (ftl-ast-body children
                             attributes
                             (apply append
                                    (map (λ (action)
                                           (if (ftl-ast-define? action)
                                               (list (check (void) action))
                                               (if (iterable? (ftl-ast-loop-iterate action))
                                                   (map (λ (def)
                                                          (ftl-ast-loop (ftl-ast-loop-iterate action)
                                                                    (check (ftl-ast-loop-iterate action) def)))
                                                        (ftl-ast-loop-actions action))
                                                   (error "cannot loop over non-sequence child"))))
                                         actions)))))
               vocabulary)))

; Note that child sequences are really semantically equivalent to recursive intermediate productions.
; This fact is fairly trivial once you see it but was not obvious to me at first.
; This:
;   self -> ... [child] ...
;     self.a := fold 0 .. child$-.b + child$i.b
;     self.c := fold 1 .. self$-.a * self$i.a
; has the following semantic meaning:
;   self -> u1 self' u2
;     self.a := self'.a
;     self.c := self'.c
;   self' -> child self'_r | e
;     self'.a := child.b + self'_r.a | 0
;     self'.c := self'.a * self'_r.a | 1

; How to compute a fold (reduction):
; 1. set attribute to the initial value
; 2. for each iterated child,
;    a. destructively set the attribute to the output of step

(define (lambdify runtime action)
  (letrec ([library (ftl-runtime-library runtime)]
           [function (λ (fun)
                       (cdr (hash-ref library fun)))]
           [deps (λ (expr base) ; construct list of unique occurrences of attributes
                   (match expr
                     [(ftl-ast-expr-call fun args) (foldl deps base args)]
                     [(ftl-ast-expr-cond cond then else) (foldl deps base (list cond then else))]
                     [(ftl-ast-expr-fold init step) (deps step (deps init base))]
                     [(? ftl-ast-refer?) (if (not (member expr base))
                                         (cons expr base)
                                         base)]
                     [else base]))]
           [cbv (λ (depv fun args) ; call by value
                  (let ([fun-fun (function fun)] ; so much fun
                        [arg-funs (map (curry close depv) args)])
                    (λ (argv)
                      (apply fun-fun (map (λ (f) (f argv)) arg-funs)))))]
           [close (λ (depv expr)
                    (match expr
                      [(ftl-ast-expr-call fun args) (cbv depv fun args)]
                      [(ftl-ast-expr-cond cond then else) (let ([cond-fun (close depv cond)]
                                                            [then-fun (close depv then)]
                                                            [else-fun (close depv else)])
                                                        (λ (argv)
                                                          (if (cond-fun argv)
                                                              (then-fun argv)
                                                              (else-fun argv))))]
                      [(? ftl-ast-refer?) (λ (argv)
                                        (vector-ref argv (vector-member expr depv)))]
                      [else (λ (_) expr)]))]
           [refer->dependency (λ (ref)
                                (ftl-ir-dependency (ftl-ast-refer-object ref)
                                               (ftl-ast-refer-index ref)
                                               (ftl-ast-refer-label ref)))]
           [ftl-ir-define (λ (rhs)
                        (let* ([depv (apply vector-immutable (deps rhs null))]
                               [lam (close depv rhs)])
                          (ftl-ir-definition lam (vector-map refer->dependency depv))))])
    
    (match action
      [(ftl-ast-loop iter (ftl-ast-define lhs (ftl-ast-expr-fold init step)))
       (cons (ftl-ast-refer->pair lhs)
             (ftl-ir-reduction (ftl-ir-define init)
                           (ftl-ir-define step)
                           iter))]
      [(ftl-ast-loop _ (ftl-ast-define lhs rhs))
       (cons (ftl-ast-refer->pair lhs)
             (ftl-ir-define rhs))]
      [(ftl-ast-define lhs rhs)
       (cons (ftl-ast-refer->pair lhs)
             (ftl-ir-define rhs))])))


(define (compile ftl-ast-vocab root runtime)
  (let* ([cdrmap (λ (f xs)
                   (map (λ (p)
                          (cons (car p)
                                (f (cdr p))))
                        xs))]
         [inputs (λ (body)
                   (filter-map (λ (decl)
                                 (and (ftl-ast-declare-input decl)
                                      (ftl-ast-declare-name decl)))
                               (ftl-ast-body-attributes body)))]
         [labels (λ (body)
                   (make-immutable-hasheq
                    (map (λ (decl)
                           (cons (ftl-ast-declare-name decl)
                                 (ftl-ast-declare-type decl)))
                         (ftl-ast-body-attributes body))))]
         [singletons (λ (body)
                       (make-immutable-hasheq
                        (filter-map (λ (child)
                                      (and (not (ftl-ast-child-sequence child))
                                           (cons (ftl-ast-child-name child)
                                                 (ftl-ast-child-interface child))))
                                    (ftl-ast-body-children body))))]
         [sequences (λ (body)
                      (make-immutable-hasheq
                       (filter-map (λ (child)
                                     (and (ftl-ast-child-sequence child)
                                          (cons (ftl-ast-child-name child)
                                                (ftl-ast-child-interface child))))
                                   (ftl-ast-body-children body))))]
         [definitions (λ (body)
                        (make-immutable-hash
                         (map (curry lambdify runtime)
                              (ftl-ast-body-actions body))))])
    (ftl-ir-grammar (make-immutable-hasheq
                 (hash-map ftl-ast-vocab
                           (λ (symbol production)
                             (cons symbol
                                   (make-immutable-hasheq
                                    (cdrmap (λ (body)
                                              (ftl-ir-alternative (inputs body)
                                                              (labels body)
                                                              (definitions body)
                                                              (singletons body)
                                                              (sequences body)))
                                            (rest production)))))))
                root)))

; --------------------------
; Example Grammar in IR Form
; --------------------------

(define example-ir (translate example-ast 'Root ftl-base-runtime))
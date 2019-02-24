#lang rosette

; Parser and Serializer for Language of Attribute Grammars

(require "syntax.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/match)

(provide ag-parse
         ag-serialize)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns (INTERFACE
                             RBRACE
                             LBRACE
                             INPUT
                             OUTPUT
                             TRAIT
                             CLASS
                             CHILDREN
                             ATTRIBUTES
                             EVALUATION
                             LOOP
                             SELF
                             DEFINE
                             FOLD
                             FIRST
                             PREVIOUS
                             LAST
                             DOTDOT
                             SEMICOLON
                             CONDITION
                             COLON
                             PLUS
                             MINUS
                             AND
                             OR
                             DIVIDE
                             MULTIPLY
                             LPAREN
                             RPAREN
                             LBRACKET
                             RBRACKET
                             COMMA
                             DOT
                             GT
                             LT
                             GE
                             LE
                             EQ
                             NE
                             NOT
                             EOF))

(define-tokens tkns (IDENT
                     LITERAL))

(define-lex-trans integer
  (syntax-rules ()
    ((_)
     (:: (:? (:or "-" "+"))
         (:+ (char-range "0" "9"))))))

(define-lex-trans exponent
  (syntax-rules ()
    ((_)
     (:: (:or "e" "E") (:? (:or "+" "-")) (:+ (char-range "0" "9"))))))

(define-lex-trans float
  (syntax-rules ()
    ((_)
     (:: (:? (:or "-" "+"))
         (:or (:: (:+ (char-range "0" "9")) "." (:* (char-range "0" "9")) (:? (exponent)) (:? "f"))
              (:: "." (:+ (char-range "0" "9")) (:? (exponent)))
              (:: (:+ (char-range "0" "9")) (exponent)))))))

(define-lex-trans number
  (syntax-rules ()
    ((_)
     (:or (integer) (float)))))

(define-lex-trans ident
  (syntax-rules ()
    ((_)
     (:: (:or (char-range "a" "z") (char-range "A" "Z") "_" "&") (:* (:or (char-range "a" "z") (char-range "A" "Z") (char-range "0" "9") "_" "-"))))))

(define-lex-trans comment
  (syntax-rules ()
    ((_)
     (:or (:: "//" (:* (char-complement (:or "\r" "\n"))) (:? "\r") "\n")
          (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))))

(define-lex-trans string
  (syntax-rules ()
    ((_)
     (:: "\""
         (:* (:? (:: "\\" (:or "\\" "\"")))
             (char-complement (:or "\\" "\"")))
         "\""))))

(define ag-lex
  (lexer
   [(comment) (ag-lex input-port)]
   ["true" (token-LITERAL #t)]
   ["false" (token-LITERAL #f)]
   ["interface" (token-INTERFACE)]
   ["}" (token-RBRACE)]
   ["{" (token-LBRACE)]
   ["input" (token-INPUT)]
   ["output" (token-OUTPUT)]
   ["trait" (token-TRAIT)]
   ["class" (token-CLASS)]
   ["children" (token-CHILDREN)]
   ["attributes" (token-ATTRIBUTES)]
   ["evaluation" (token-EVALUATION)]
   ["loop" (token-LOOP)]
   ["self" (token-SELF)]
   [":=" (token-DEFINE)]
   ["fold" (token-FOLD)]
   [".." (token-DOTDOT)]
   [";" (token-SEMICOLON)]
   ["?" (token-CONDITION)]
   [":" (token-COLON)]
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["&&" (token-AND)]
   ["||" (token-OR)]
   ["/" (token-DIVIDE)]
   ["*" (token-MULTIPLY)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   [">" (token-GT)]
   ["<" (token-LT)]
   [">=" (token-GE)]
   ["<=" (token-LE)]
   ["==" (token-EQ)]
   ["!=" (token-NE)]
   ["!" (token-NOT)]
   ["$0" (token-FIRST)]
   ["$-" (token-PREVIOUS)]
   ["$$" (token-LAST)]
   [(number) (token-LITERAL (string->number lexeme))]
   [(ident) (token-IDENT (string->symbol lexeme))]
   [(string) (token-LITERAL (read lexeme))]
   [whitespace (ag-lex input-port)]
   [(eof) (token-EOF)]))

; -----------------
; Parser Definition
; -----------------

(define ag-parse-lexed
  (parser
   (start decl-list)
   (tokens tkns e-tkns)
   (end EOF)
   (error (thunk (display "Error: could not parse attribute grammar source")))
   (grammar
    (decl-list
     ((decl decl-list) (cons $1 $2))
     (() null))

    (decl
     ((iface-decl) $1)
     ((trait-decl) $1)
     ((class-decl) $1))

    (class-decl
     ((CLASS IDENT COLON IDENT
             LBRACE class-body RBRACE) (ag-class $2 null $4 $6))
     ((CLASS IDENT LPAREN trait-list RPAREN COLON IDENT
             LBRACE class-body RBRACE) (ag-class $2 $4 $7 $9)))

    (trait-list
     ((IDENT COMMA trait-list) (cons $1 $3))
     ((IDENT) (list $1)))

    (iface-decl
     ((INTERFACE IDENT
                 LBRACE label-list RBRACE) (ag-interface $2 $4)))

    (trait-decl
     ((TRAIT IDENT LBRACE class-body RBRACE) (ag-trait $2 $4)))

    (class-body
     ((class-body-block class-body) (ag-body-merge $1 $2))
     (() (ag-body null null null)))

    (class-body-block
     ((CHILDREN LBRACE child-list RBRACE) (ag-body $3 null null))
     ((ATTRIBUTES LBRACE label-list RBRACE) (ag-body null $3 null))
     ((EVALUATION LBRACE rule-or-loop-list RBRACE) (ag-body null null $3)))

    (child-list
     ((IDENT COLON child-type SEMICOLON child-list) (cons (ag-child $1
                                                                         (car $3)
                                                                         (cdr $3))
                                                          $5))
     (() null))

    (child-type
     ((IDENT) (cons #f $1))
     ((LBRACKET IDENT RBRACKET) (cons #t $2)))

    (label-list
     ((label-io IDENT COLON IDENT SEMICOLON
               label-list) (cons (ag-label $1 $2 $4) $6))
     (() null))

    (label-io
     ((INPUT) #t)
     ((OUTPUT) #f))

    (rule-or-loop-list
     ((rule-or-loop rule-or-loop-list) (cons $1 $2))
     (() null))

    (rule-or-loop
     ((rule) $1)
     ((loop) $1))

    (rule
      ((IDENT DEFINE expr SEMICOLON) (ag-rule (cons 'self $1) $3))
      ((SELF DOT IDENT DEFINE expr SEMICOLON) (ag-rule (cons 'self $3) $5))
      ((IDENT DOT IDENT DEFINE expr SEMICOLON) (ag-rule (cons $1 $3) $5)))

    (loop
     ((LOOP IDENT LBRACE loop-rule-list RBRACE) (ag-loop $2 $4)))

    (loop-rule-list
     ((loop-rule loop-rule-list) (cons $1 $2))
     (() null))

    (loop-rule
     ((IDENT DEFINE fold SEMICOLON) (ag-rule (cons 'self $1) $3))
     ((SELF DOT IDENT DEFINE fold SEMICOLON) (ag-rule (cons 'self $3) $5))
     ((IDENT DOT IDENT DEFINE fold SEMICOLON) (ag-rule (cons $1 $3) $5)))

    (fold
     ((expr) $1)
     ((FOLD expr DOTDOT expr) (ag-fold $2 $4)))

    (expr
     ((cond-expr) $1))

    (cond-expr
     ((and-expr) $1)
     ((and-expr CONDITION and-expr COLON and-expr) (ag-expr-condition $1 $3 $5)))

    (and-expr
     ((or-expr) $1)
     ((or-expr AND and-expr) (ag-expr-binary $1 '&& $3)))

    (or-expr
     ((comp-expr) $1)
     ((comp-expr OR or-expr) (ag-expr-binary $1 '|| $3)))

    (comp-expr
     ((term) $1)
     ((NOT term) (ag-expr-unary '! $2))
     ((term GT term) (ag-expr-binary $1 '> $3))
     ((term LT term) (ag-expr-binary $1 '< $3))
     ((term GE term) (ag-expr-binary $1 '>= $3))
     ((term LE term) (ag-expr-binary $1 '<= $3))
     ((term EQ term) (ag-expr-binary $1 '== $3))
     ((term NE term) (ag-expr-binary $1 '!= $3)))

    (term
     ((factor) $1)
     ((factor PLUS term) (ag-expr-binary $1 '+ $3))
     ((factor MINUS term) (ag-expr-binary $1 '- $3)))

    (factor
     ((prim-expr) $1)
     ((prim-expr MULTIPLY factor) (ag-expr-binary $1 '* $3))
     ((prim-expr DIVIDE factor) (ag-expr-binary $1 '/ $3)))

    (prim-expr
     ((MINUS prim-expr) (ag-expr-unary '- $2))
     ((attr-expr) $1)
     ((LITERAL) $1)
     ((IDENT LPAREN arg-list RPAREN) (ag-expr-call $1 $3))
     ((IDENT LPAREN RPAREN) (ag-expr-call $1 null))
     ((LPAREN expr RPAREN) $2))

    (attr-expr
     ((IDENT) (ag-expr-reference 'self #f $1))
     ((SELF DOT IDENT) (ag-expr-reference 'self #f $3))
     ((IDENT DOT IDENT) (ag-expr-reference $1 #f $3))
     ((IDENT FIRST DOT IDENT) (ag-expr-reference $1 'first $4))
     ((IDENT PREVIOUS DOT IDENT) (ag-expr-reference $1 'previous $4))
     ((IDENT LAST DOT IDENT) (ag-expr-reference $1 'last $4)))

    (arg-list
     ((expr COMMA arg-list) (cons $1 $3))
     ((expr) (list $1))))))

; Take an input port and return an abstract syntax tree for the attribute grammar.
(define (ag-parse input)
  (ag-parse-lexed (thunk (ag-lex input))))

; -----------------
; AST Serialization
; -----------------

(define (ag-serialize ast-list)
  (string-join (for/list ([decl ast-list])
                 (cond
                   [(ag-interface? decl)
                    (ag-interface-serialize decl)]
                   [(ag-trait? decl)
                    (ag-trait-serialize decl)]
                   [(ag-class? decl)
                    (ag-class-serialize decl)]))
               "\n"))

(define (ag-interface-serialize interface-ast)
  (match-let ([(ag-interface name labels) interface-ast])
    (string-append "interface "
                   (symbol->string name)
                   " {\n"
                   (string-join (map ag-label-serialize
                                     labels)
                                "\n")
                   "\n}\n")))

(define (ag-label-serialize label-ast)
  (match-let ([(ag-label input name type) label-ast])
    (string-append "    "
                   (if input "input" "var")
                   " "
                   (symbol->string name)
                   " : "
                   (symbol->string type) ";")))

(define (ag-trait-serialize trait-ast)
  (match-let ([(ag-trait name body) trait-ast])
    (string-append "trait "
                   (symbol->string name)
                   " {\n"
                   (ag-body-serialize body)
                   "\n}\n")))

(define (ag-class-serialize class-ast)
  (match-let ([(ag-class name traits interface body) class-ast])
    (define trait-list
      (if (null? traits)
          ""
          (string-append "(" (string-join (map symbol->string traits) ",") ")")))

    (string-append "class "
                   (symbol->string name)
                   trait-list
                   " : "
                   (symbol->string interface)
                   " {\n"
                   (ag-body-serialize body)
                   "}\n")))

(define (ag-body-serialize body-ast)
  (match-let ([(ag-body children labels rules) body-ast])
    (string-append "    children {\n"
                   (serialize-children children)
                   "\n    }\n"
                   "    attributes {\n"
                   (serialize-labels labels)
                   "\n    }\n"
                   "    evaluation {\n"
                   (serialize-rules rules 8)
                   "\n    }\n")))

(define (serialize-labels label-ast-list)
  (string-join (for/list ([label-ast label-ast-list])
                 (string-append (spaces 4)
                                (ag-label-serialize label-ast)))
               "\n"))

(define (ag-child-serialize child-ast)
  (match-let ([(ag-child name sequence interface) child-ast])
    (let* ([ident (symbol->string name)]
           [iface (symbol->string interface)]
           [type (if sequence
                     (string-append "[" iface "]")
                     iface)])
      (string-append "        " ident " : " type ";"))))

(define (serialize-children children-ast-list)
  (string-join (map ag-child-serialize children-ast-list) "\n"))

(define (spaces depth)
  (if (equal? depth 0)
      ""
      (string-append (spaces (- depth 1)) " ")))

; This will accept folds, regardless of whether the evaluation rule was actually
; in a loop context or not.
(define (ag-rule-serialize rule-ast depth)
  (match-let ([(ag-rule (cons object label) value) rule-ast])
    (string-append (spaces depth)
                   (symbol->string object)
                   "."
                   (symbol->string label)
                   " := "
                   (ag-fold-serialize value)
                   ";")))

(define (ag-loop-serialize loop-ast depth)
  (match-let ([(ag-loop object rules) loop-ast])
    (string-append (spaces depth)
                   "loop "
                   (symbol->string object)
                   " {\n"
                   (serialize-rules rules (+ depth 4))
                   ;(spaces depth) ; FIXME: delete?
                   "\n        }")))

(define (serialize-rules rule-or-loop-ast-list depth)
  (string-join (for/list ([rule-or-loop-ast rule-or-loop-ast-list])
                 (if (ag-rule? rule-or-loop-ast)
                     (ag-rule-serialize rule-or-loop-ast depth)
                     (ag-loop-serialize rule-or-loop-ast depth)))
               "\n"))

(define/match (ag-fold-serialize fold-ast)
  [((ag-fold seed step))
   (string-append "fold (" (ag-expr-serialize seed) ") .. "
                  "(" (ag-expr-serialize step) ")")]
  [(expr)
   (ag-expr-serialize expr)])

(define/match (ag-expr-serialize expr-ast)
  [((ag-expr-condition if then else))
   (string-append "(" (ag-expr-serialize if) ") ? "
                  "(" (ag-expr-serialize then) ") : "
                  "(" (ag-expr-serialize else) ")")]
  [((ag-expr-unary op e))
   (string-append (symbol->string op)
                  "(" (ag-expr-serialize e) ")")]
  [((ag-expr-binary e1 op e2))
   (string-append "(" (ag-expr-serialize e1) ") "
                  (symbol->string op)
                  " (" (ag-expr-serialize e2) ")")]
  [((ag-expr-call name args))
   (string-append (symbol->string name)
                  "("
                  (string-join (map ag-expr-serialize args) ",")
                  ")")]
  [((? ag-expr-reference?))
   (ag-expr-reference->string expr-ast)]
  [((? number?))
   (number->string expr-ast)]
  [((? boolean?))
   (if expr-ast "true" "false")])

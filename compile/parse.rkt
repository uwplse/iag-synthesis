#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Grammar DSL Parser and Serializer

(require "../core/syntax.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/match)

(provide parse-ftl
         ftl-ast-parse
         ftl-ast-serialize
         ftl-ast-refer-serialize)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns (INTERFACE
                             RBRACE
                             LBRACE
                             VAR
                             INPUT
                             TRAIT
                             CLASS
                             CHILDREN
                             ATTRIBUTES
                             PHANTOM
                             ACTIONS
                             LOOP
                             DEFINE
                             FOLD
                             FIRST
                             PREVIOUS
                             INDEX
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

(define ftl-lex
  (lexer
   [(comment) (ftl-lex input-port)]
   ["true" (token-LITERAL #t)]
   ["false" (token-LITERAL #f)]
   ["interface" (token-INTERFACE)]
   ["}" (token-RBRACE)]
   ["{" (token-LBRACE)]
   ["var" (token-VAR)]
   ["input" (token-INPUT)]
   ["trait" (token-TRAIT)]
   ["class" (token-CLASS)]
   ["children" (token-CHILDREN)]
   ["attributes" (token-ATTRIBUTES)]
   ["phantom" (token-PHANTOM)]
   ["actions" (token-ACTIONS)]
   ["loop" (token-LOOP)]
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
   ["$i" (token-INDEX)]
   ["$$" (token-LAST)]
   [(number) (token-LITERAL (string->number lexeme))]
   [(ident) (token-IDENT lexeme)]
   [(string) (token-LITERAL (read lexeme))]
   [whitespace (ftl-lex input-port)]
   [(eof) (token-EOF)]))

; -----------------
; Parser Definition
; -----------------

; TODO: phantom types and the 'fold <ident> by <expr> { <defn>* }' construct
; (which may be deprecated?)

; TODO: default/given input values of the form 'input <ident> : <type> =
; <literal>;'

(define ftl-parse
  (parser
   (start decl-list)
   (tokens tkns e-tkns)
   (end EOF)
   (error (thunk (display "Error: could not parse FTL source")))
   (grammar
    (decl-list
     ((decl decl-list) (cons $1 $2))
     (() null))

    (decl
     ((iface-decl) $1)
     ((trait-decl) $1)
     ((class-decl) $1))

    (class-decl
     ((CLASS IDENT COLON IDENT LBRACE class-body RBRACE) (ftl-ast-class (string->symbol $2)
                                                                    null
                                                                    (string->symbol $4)
                                                                    $6))
     ((CLASS IDENT LPAREN trait-list RPAREN COLON IDENT LBRACE class-body RBRACE) (ftl-ast-class (string->symbol $2)
                                                                                             $4
                                                                                             (string->symbol $7)
                                                                                             $9)))

    (trait-list
     ((IDENT COMMA trait-list) (cons (string->symbol $1) $3))
     ((IDENT) (list (string->symbol $1))))

    (iface-decl
     ((INTERFACE IDENT LBRACE attr-decl-list RBRACE) (ftl-ast-interface (string->symbol $2) $4)))

    (trait-decl
     ((TRAIT IDENT LBRACE class-body RBRACE) (ftl-ast-trait (string->symbol $2) $4)))

    (class-body
     ((class-body-block class-body) (ftl-ast-body-merge $1 $2))
     (() (ftl-ast-body null null null)))

    (class-body-block
     ((ATTRIBUTES LBRACE attr-decl-list RBRACE) (ftl-ast-body null $3 null))
;     ((PHANTOM LBRACE attr-decl-list RBRACE) (ftl-ast-body null $3 null))
     ((ACTIONS LBRACE attr-def-list RBRACE) (ftl-ast-body null null $3))
     ((CHILDREN LBRACE child-list RBRACE) (ftl-ast-body $3 null null)))

    (attr-decl-list ((attr-decl-input IDENT COLON IDENT SEMICOLON attr-decl-list) (cons (ftl-ast-declare $1
                                                                                                     (string->symbol $2)
                                                                                                     (string->symbol $4))
                                                                                        $6))
                    (() null))

    (attr-decl-input ((VAR) #f)
                     ((INPUT) #t))

    (attr-def-list
     ((attr-def attr-def-list) (cons $1 $2))
     (() null))

    (attr-def
     ((LOOP IDENT LBRACE attr-def-loop-list RBRACE) (ftl-ast-loop (string->symbol $2) $4))
     ((attr-def-ref DEFINE cond-expr SEMICOLON) (ftl-ast-define $1 $3)))

    (attr-def-ref
     ((IDENT) (ftl-ast-refer 'self 'none (string->symbol $1)))
     ((IDENT DOT IDENT) (ftl-ast-refer (string->symbol $1) 'none (string->symbol $3)))
     ((IDENT FIRST DOT IDENT) (ftl-ast-refer (string->symbol $1) 'first (string->symbol $4)))
     ((IDENT LAST DOT IDENT) (ftl-ast-refer (string->symbol $1) 'last (string->symbol $4))))

    (attr-def-loop-list
     ((attr-def-loop attr-def-loop-list) (cons $1 $2))
     (() null))

    (attr-def-loop
     ((attr-def-loop-ref DEFINE fold-expr SEMICOLON) (ftl-ast-define $1 $3)))

    (attr-def-loop-ref
     ((IDENT) (ftl-ast-refer 'self 'none (string->symbol $1)))
     ((IDENT DOT IDENT) (ftl-ast-refer (string->symbol $1) 'none (string->symbol $3)))
     ((PREVIOUS DOT IDENT) (ftl-ast-refer 'self 'previous (string->symbol $3))) ; $0, $i, and $$ have no semantic meaning for self
     ((IDENT FIRST DOT IDENT) (ftl-ast-refer (string->symbol $1) 'first (string->symbol $4)))
     ((IDENT PREVIOUS DOT IDENT) (ftl-ast-refer (string->symbol $1) 'previous (string->symbol $4)))
     ((IDENT INDEX DOT IDENT) (ftl-ast-refer (string->symbol $1) 'current (string->symbol $4)))
     ((IDENT LAST DOT IDENT) (ftl-ast-refer (string->symbol $1) 'last (string->symbol $4))))

    (child-list
     ((IDENT COLON child-type SEMICOLON child-list) (cons (ftl-ast-child (string->symbol $1)
                                                                     (car $3)
                                                                     (cdr $3))
                                                          $5))
     (() null))

    (child-type
     ((IDENT) (cons #f (string->symbol $1)))
     ((LBRACKET IDENT RBRACKET) (cons #t (string->symbol $2))))

    (fold-expr
     ((cond-expr) $1)
     ((FOLD cond-expr DOTDOT cond-expr) (ftl-ast-expr-fold $2 $4)))

    (cond-expr
     ((and-expr) $1)
     ((and-expr CONDITION and-expr COLON and-expr) (ftl-ast-expr-cond $1 $3 $5)))

    (and-expr
     ((or-expr) $1)
     ((or-expr AND and-expr) (ftl-ast-expr-binary $1 '&& $3)))

    (or-expr
     ((comp-expr) $1)
     ((comp-expr OR or-expr) (ftl-ast-expr-binary $1 '|| $3)))

    (comp-expr
     ((term) $1)
     ((NOT term) (ftl-ast-expr-unary '! $2))
     ((term GT term) (ftl-ast-expr-binary $1 '> $3))
     ((term LT term) (ftl-ast-expr-binary $1 '< $3))
     ((term GE term) (ftl-ast-expr-binary $1 '>= $3))
     ((term LE term) (ftl-ast-expr-binary $1 '<= $3))
     ((term EQ term) (ftl-ast-expr-binary $1 '== $3))
     ((term NE term) (ftl-ast-expr-binary $1 '!= $3)))

    (term
     ((factor) $1)
     ((factor PLUS term) (ftl-ast-expr-binary $1 '+ $3))
     ((factor MINUS term) (ftl-ast-expr-binary $1 '- $3)))

    (factor
     ((prim-expr) $1)
     ((prim-expr MULTIPLY factor) (ftl-ast-expr-binary $1 '* $3))
     ((prim-expr DIVIDE factor) (ftl-ast-expr-binary $1 '/ $3)))

    (prim-expr
     ((MINUS prim-expr) (ftl-ast-expr-unary '- $2))
     ((attr-def-loop-ref) $1) ; would be a lot of duplication to distinguish between attr-def-ref and attr-def-loop-ref in expressions
     ((LITERAL) $1)
     ((IDENT LPAREN arg-list RPAREN) (ftl-ast-expr-call (string->symbol $1) $3))
     ((IDENT LPAREN RPAREN) (ftl-ast-expr-call (string->symbol $1) null))
     ((LPAREN cond-expr RPAREN) $2))

    (arg-list
     ((cond-expr) (list $1))
     ((cond-expr COMMA arg-list) (cons $1 $3))))))

(define (ftl-ast-parse input)
  (ftl-parse (λ () (ftl-lex input))))

(define parse-ftl ftl-ast-parse)

; -----------------
; AST Serialization
; -----------------

(define (ftl-ast-serialize ftl-ast-list)
  (string-join (for/list ([decl ftl-ast-list])
                 (cond
                   [(ftl-ast-interface? decl)
                    (ftl-ast-interface-serialize decl)]
                   [(ftl-ast-trait? decl)
                    (ftl-ast-trait-serialize decl)]
                   [(ftl-ast-class? decl)
                    (ftl-ast-class-serialize decl)]))
               "\n"))

(define serialize-ftl ftl-ast-serialize)

(define (ftl-ast-interface-serialize iface)
  (string-append "interface "
                 (symbol->string (ftl-ast-interface-name iface))
                 " {\n"
                 (string-join (map ftl-ast-declare-serialize
                                   (ftl-ast-interface-fields iface))
                              "\n")
                 "\n}\n"))

(define (ftl-ast-declare-serialize declare)
  (string-append "    "
                 (if (ftl-ast-declare-input declare) "input" "var")
                 " "
                 (symbol->string (ftl-ast-declare-name declare))
                 " : "
                 (symbol->string (ftl-ast-declare-type declare)) ";"))

(define (ftl-ast-trait-serialize trait)
  (string-append "trait "
                 (symbol->string (ftl-ast-trait-name trait))
                 " {\n"
                 (ftl-ast-body-serialize (ftl-ast-trait-body trait))
                 "\n}\n"))

(define (ftl-ast-class-serialize class)
  (define trait-list
    (if (null? (ftl-ast-class-traits class))
        ""
        (string-append "("
                       (string-join (map symbol->string
                                         (ftl-ast-class-traits class))
                                    ",")
                       ")")))

  (string-append "class "
                   (symbol->string (ftl-ast-class-name class))
                   trait-list
                   " : "
                   (symbol->string (ftl-ast-class-interface class))
                   " {\n"
                   (ftl-ast-body-serialize (ftl-ast-class-body class))
                   "}\n"))

(define (ftl-ast-body-serialize body)
  (string-append "    children {\n"
                 (serialize-children (ftl-ast-body-children body))
                 "\n    }\n"
                 "    attributes {\n"
                 (serialize-attributes (ftl-ast-body-attributes body))
                 "\n    }\n"
                 "    actions {\n"
                 (serialize-actions (ftl-ast-body-actions body) 8)
                 "\n    }\n"))

(define (serialize-attributes attributes)
  (string-join (for/list ([attribute attributes])
                 (string-append (spaces 4)
                                (ftl-ast-declare-serialize attribute)))
               "\n"))

(define (ftl-ast-child-serialize child)
  (let* ([ident (symbol->string (ftl-ast-child-name child))]
         [iface (symbol->string (ftl-ast-child-interface child))]
         [type (if (ftl-ast-child-sequence child)
                   (string-append "[" iface "]")
                   iface)])
    (string-append "        " ident " : " type ";")))

(define (serialize-children children)
  (string-join (map ftl-ast-child-serialize children) "\n"))

(define (spaces depth)
  (if (equal? depth 0)
      ""
      (string-append (spaces (- depth 1)) " ")))

(define (ftl-ast-define-serialize def depth)
  (string-append (spaces depth)
                 (ftl-ast-refer-serialize (ftl-ast-define-lhs def))
                 " := "
                 (ftl-ast-expr-serialize (ftl-ast-define-rhs def))
                 ";"))

(define (ftl-ast-loop-serialize loop depth)
  (string-append (spaces depth)
                 "loop "
                 (symbol->string (ftl-ast-loop-iterate loop))
                 " {\n"
                 (serialize-actions (ftl-ast-loop-actions loop)
                                    (+ depth 4))
                 (spaces depth)
                 "\n        }"))

(define (serialize-actions defs depth)
  (string-join (for/list ([def defs])
                 (if (ftl-ast-define? def)
                     (ftl-ast-define-serialize def depth)
                     (ftl-ast-loop-serialize def depth)))
               "\n"))

(define (ftl-ast-refer-serialize ref)
  (let ([index (match (ftl-ast-refer-index ref)
                 ['none ""]
                 ['first "$0"]
                 ['previous "$-"]
                 ['current "$i"]
                 ['last "$$"])])
    (string-append (symbol->string (ftl-ast-refer-object ref))
                   index
                   "."
                   (symbol->string (ftl-ast-refer-label ref)))))

(define (ftl-ast-expr-serialize expr)
  (match expr
    [(ftl-ast-expr-fold init step)
     (string-append "fold (" (ftl-ast-expr-serialize init) ") .. "
                    "(" (ftl-ast-expr-serialize step) ")")]
    [(ftl-ast-expr-cond if then else)
     (string-append "(" (ftl-ast-expr-serialize if) ") ? "
                    "(" (ftl-ast-expr-serialize then) ") : "
                    "(" (ftl-ast-expr-serialize else) ")")]
    [(ftl-ast-expr-unary op e)
     (string-append (symbol->string op)
                    "(" (ftl-ast-expr-serialize e) ")")]
    [(ftl-ast-expr-binary e1 op e2)
     (string-append "(" (ftl-ast-expr-serialize e1) ") "
                    (symbol->string op)
                    " (" (ftl-ast-expr-serialize e2) ")")]
    [(ftl-ast-expr-call name args)
     (string-append (symbol->string name)
                    "("
                    (string-join (map ftl-ast-expr-serialize args) ",")
                    ")")]
    [(? ftl-ast-refer?)
     (ftl-ast-refer-serialize expr)]
    [(? number?)
     (number->string expr)]
    [(? boolean?)
     (if expr "true" "false")]))

#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Parser

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         racket/match)

(provide parse-ftl 
         ftl-ast-serialize
         ftl-ast-refer-serialize
         ftl-ast-body-merge
         example-ftl
         example-ast
         (struct-out ftl-ast-interface)
         (struct-out ftl-ast-trait)
         (struct-out ftl-ast-class)
         (struct-out ftl-ast-body)
         (struct-out ftl-ast-child)
         (struct-out ftl-ast-declare)
         (struct-out ftl-ast-define)
         (struct-out ftl-ast-refer)
         (struct-out ftl-ast-loop)
         (struct-out ftl-ast-expr-fold)
         (struct-out ftl-ast-expr-call)
         (struct-out ftl-ast-expr-unary)
         (struct-out ftl-ast-expr-binary)
         (struct-out ftl-ast-expr-cond))

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

; TODO: phantom types and the fold child by <expr> { ... } construct, which may be deprecated
; TODO: default/given input values of the form input <ident> : <type> = <literal>;

; Note that identifiers are symbols

(struct ftl-ast-interface ; interface definition
  (name ; identifier corresponding to nonterminal in the attribute grammar
   fields ; list of attribute declarations
   ) #:transparent)
(struct ftl-ast-trait ; trait (mixin class) definition
  (name ; identifier
   body ; class body
   ) #:transparent)
(struct ftl-ast-class
  (name ; identifier corresponding to a particular production for the nonterminal
   traits ; list of trait identifiers
   interface ; the nonterminal symbol
   body ; a class body
   ) #:transparent)
(struct ftl-ast-body ; body of a class or trait
  (children ; list of child declarations
   attributes ; list of attribute declarations
   actions ; list of attribute definitions, possibly inside of loops
   ) #:transparent)
(struct ftl-ast-child ; child declaration
  (name ; child (more generally, object) identifier (cannot be 'self)
   sequence ; whether this is a child sequence
   interface ; the nonterminal that this child's subtree(s) generates
   ) #:transparent)
(struct ftl-ast-declare ; attribute declaration
  (input ; whether this attribute's value is given (#t) or computed (#f)
   name ; attribute identifier, also known as a label
   type ; the identifier for the attribute value's type, interpreted by the runtime/backend
   ) #:transparent)
(struct ftl-ast-define ; attribute definition
  (lhs ; attribute reference (index will always be 'none)
   rhs ; expression, made up of function calls, conditionals, unary/binary operations, and
       ; zero-to-one top-level fold
   ) #:transparent)
(struct ftl-ast-refer ; attribute reference
  (object ; object identifier, i.e., a child identifier or 'self 
   index ; 'previous, 'current, 'last, or 'none
   label ; attribute identifier
   ) #:transparent)
(struct ftl-ast-loop ; loop body
  (iterate ; identifier of a sequence child
   actions ; list of attribute definitions
   ) #:transparent)

(struct ftl-ast-expr-fold (init step) #:transparent)
(struct ftl-ast-expr-call (fun args) #:transparent)
(struct ftl-ast-expr-unary (operator operand) #:transparent)
(struct ftl-ast-expr-binary (left operator right) #:transparent)
(struct ftl-ast-expr-cond (if then else) #:transparent)

(define (ftl-ast-body-merge . xs)
  (ftl-ast-body (apply append (map ftl-ast-body-children xs))
            (apply append (map ftl-ast-body-attributes xs))
            (apply append (map ftl-ast-body-actions xs))))

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

(define (parse-ftl input)
  (ftl-parse (λ () (ftl-lex input))))

; -----------------
; AST Serialization
; -----------------

(define (ftl-ast-serialize ftl-ast-list)
  (define (m decl)
    (cond
      [(ftl-ast-interface? decl) (ftl-ast-interface-serialize decl)]
      [(ftl-ast-trait? decl) (ftl-ast-trait-serialize decl)]
      [(ftl-ast-class? decl) (ftl-ast-class-serialize decl)]))
  (string-join (map m ftl-ast-list) "\n"))

(define serialize-ftl ftl-ast-serialize)

(define (ftl-ast-interface-serialize iface)
  (define header (string-append "interface " (symbol->string (ftl-ast-interface-name iface)) " {\n"))
  (define body (string-join (map ftl-ast-declare-serialize (ftl-ast-interface-fields iface)) "\n"))
  (define footer "\n}\n")
  (string-append header body footer))

(define (ftl-ast-declare-serialize declare)
  (string-append "    "
                 (if (ftl-ast-declare-input declare) "input" "var")
                 " "
                 (symbol->string (ftl-ast-declare-name declare))
                 " : "
                 (symbol->string (ftl-ast-declare-type declare)) ";"))

(define (ftl-ast-trait-serialize trait)
  (define header (string-append "trait " (symbol->string (ftl-ast-trait-name trait)) " {\n"))
  (define body (ftl-ast-body-serialize (ftl-ast-trait-body trait)))
  (define footer "\n}\n")
  (string-append header body footer))

(define (ftl-ast-class-serialize class)
  (define header (string-append 
    "class " 
    (symbol->string (ftl-ast-class-name class))
    (if (null? (ftl-ast-class-traits class))
        "" 
        (string-append "(" (string-join (map symbol->string (ftl-ast-class-traits class)) ",") ")"))
    " : "
    (symbol->string (ftl-ast-class-interface class))
    " {\n"))
  (define body (ftl-ast-body-serialize (ftl-ast-class-body class)))
  (define footer "}\n")
  (string-append header body footer))

(define (ftl-ast-body-serialize body)
  (define children (string-append "    children {\n" (serialize-children (ftl-ast-body-children body)) "\n    }\n"))
  (define attributes (string-append "    attributes {\n" (serialize-attributes (ftl-ast-body-attributes body)) "\n    }\n"))
  (define actions (string-append "    actions {\n" (serialize-actions (ftl-ast-body-actions body) 8) "\n    }\n"))
  (string-append children attributes actions))

(define (serialize-attributes attributes)
  (string-join (map (λ (x) (string-append (spaces 4) (ftl-ast-declare-serialize x))) attributes) "\n"))

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
  (string-append (spaces depth) (ftl-ast-refer-serialize (ftl-ast-define-lhs def))
                 " := " (ftl-ast-expr-serialize (ftl-ast-define-rhs def)) ";"))

(define (ftl-ast-loop-serialize loop depth)
  (string-append (spaces depth) "loop " (symbol->string (ftl-ast-loop-iterate loop)) " {\n"
                 (serialize-actions (ftl-ast-loop-actions loop) (+ depth 4)) (spaces depth)
                 "\n        }"))

(define (serialize-actions defs depth)
  (string-join (map (λ (x) (if (ftl-ast-define? x)
                               (ftl-ast-define-serialize x depth)
                               (ftl-ast-loop-serialize x depth))) defs) "\n"))

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
    [(ftl-ast-expr-fold init step) (string-append "fold (" (ftl-ast-expr-serialize init) ") .. "
                                                  "(" (ftl-ast-expr-serialize step) ")")]
    [(ftl-ast-expr-cond if then else) (string-append "(" (ftl-ast-expr-serialize if) ") ? "
                                                     "(" (ftl-ast-expr-serialize then) ") : "
                                                     "(" (ftl-ast-expr-serialize else) ")")]
    [(ftl-ast-expr-unary op e) (string-append (symbol->string op) "(" (ftl-ast-expr-serialize e) ")")]
    [(ftl-ast-expr-binary e1 op e2) (string-append "(" (ftl-ast-expr-serialize e1) ") "
                                                   (symbol->string op)
                                                   " (" (ftl-ast-expr-serialize e2) ")")]
    [(ftl-ast-expr-call name args) (string-append (symbol->string name) "(" (string-join (map ftl-ast-expr-serialize args) ",") ")")]
    [(? ftl-ast-refer?) (ftl-ast-refer-serialize expr)]
    [(? number?) (number->string expr)]
    [(? boolean?) (if expr "true" "false")]))

; ------------------
; Example FTL source
; ------------------

(define example-ftl "
interface Root { }

interface Point {
    var bx : int;
    var by : int;
    var x : int;
    var y : int;
}

trait InheritBase {
    children {
        p : [Point];
    }
    actions {
        loop p {
            p.bx := x;
            p.by := y;
        }
    }
}

class Origin(InheritBase) : Root {
    attributes {
        input x : int;
        input y : int;
    }
}

class Relative(InheritBase) : Point {
    attributes {
        input dx : int;
        input dy : int;
    }
    actions {
        x := bx + dx;
        y := by + dy;
    }
}

class Fixed(InheritBase) : Point {
    attributes {
        var dx : int;
        var dy : int;
        input fx : int;
        input fy : int;
    }
    actions {
        dx := x - bx;
        dy := y - by;
        x := fx;
        y := fy;
    }
}

class Endpoint : Point {
    actions {
        x := bx;
        y := by;
    }
}
")

(define example-ast (parse-ftl (open-input-string example-ftl)))

(define example-str (serialize-ftl example-ast))

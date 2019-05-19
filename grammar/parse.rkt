#lang racket

; Parser and serializer for language of attribute grammars

(require "../utility.rkt"
         "syntax.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide parse-grammar
         file->grammar
         grammar->string)

; -----
; Lexer
; -----

(define-empty-tokens e-tkns
  (RBRACE LBRACE LPAREN RPAREN LBRACKET RBRACKET
   COLON SEMICOLON COMMA DOT
   TRAVERSAL CASE VISIT RECUR ITERATE
   INTERFACE CLASS
   CHILDREN ATTRIBUTES METHODS RULES
   INPUT OUTPUT
   DEFINE FOLDL FOLDR DOTDOT
   BANG
   PLUS MINUS STAR SLASH
   LT LE EQ NE GE GT
   AND OR
   IF THEN ELSE
   SELF FIRST PRED CURR SUCC LAST
   EOF))

(define-tokens tkns
  (IDENT
   INT FLOAT))

(define-lex-trans comment
  (syntax-rules ()
    ((_)
     (:or (:: "//" (:* (char-complement (:or "\r" "\n"))) (:? "\r") "\n")
          (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))))

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

(define-lex-trans identifier
  (syntax-rules ()
    ((_)
     (:: (:or (char-range "a" "z") (char-range "A" "Z") "_")
         (:* (:or (char-range "a" "z") (char-range "A" "Z") "_" (char-range "0" "9")))))))

(define lex
  (lexer
   [(comment) (lex input-port)]
   ["}" (token-RBRACE)]
   ["{" (token-LBRACE)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   [":" (token-COLON)]
   [";" (token-SEMICOLON)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   ["traversal" (token-TRAVERSAL)]
   ["case" (token-CASE)]
   ["iterate" (token-ITERATE)]
   ["recur" (token-RECUR)]
   ["visit" (token-VISIT)]
   ["interface" (token-INTERFACE)]
   ["children" (token-CHILDREN)]
   ["attributes" (token-ATTRIBUTES)]
   ["class" (token-CLASS)]
   ["methods" (token-METHODS)]
   ["rules" (token-RULES)]
   ["input" (token-INPUT)]
   ["output" (token-OUTPUT)]
   [":=" (token-DEFINE)]
   ["foldl" (token-FOLDL)]
   ["foldr" (token-FOLDR)]
   [".." (token-DOTDOT)]
   ["!" (token-BANG)]
   ["+" (token-PLUS)]
   ["-" (token-MINUS)]
   ["*" (token-STAR)]
   ["/" (token-SLASH)]
   ["<" (token-LT)]
   ["<=" (token-LE)]
   ["==" (token-EQ)]
   ["!=" (token-NE)]
   [">=" (token-GE)]
   [">" (token-GT)]
   ["&&" (token-AND)]
   ["||" (token-OR)]
   ["if" (token-IF)]
   ["then" (token-THEN)]
   ["else" (token-ELSE)]
   ["self" (token-SELF)]
   ["$0" (token-FIRST)]
   ["$-" (token-PRED)]
   ["$i" (token-CURR)]
   ["$+" (token-SUCC)]
   ["$$" (token-LAST)]
   [(integer) (token-INT (string->number lexeme))]
   [(float) (token-FLOAT (string->number lexeme))]
   [(identifier) (token-IDENT (string->symbol lexeme))]
   [whitespace (lex input-port)]
   [(eof) (token-EOF)]))

; ------
; Parser
; ------

(define parse
  (parser
   (start program)
   (tokens tkns e-tkns)
   (end EOF)
   (error
    (λ (_ token lexeme)
      (printf "Unexpected token: ~a(~a)~n" token lexeme)))
   (precs
    (nonassoc LPAREN RPAREN)
    (nonassoc BANG)
    (left STAR SLASH)
    (left PLUS MINUS)
    (nonassoc LT LE EQ NE GE GT)
    (left OR)
    (left AND)
    (nonassoc IF THEN ELSE))
   (grammar
    (program
     ((traversal-list interface-list class-list) (ag-grammar $2 $3 $1)))

    (traversal-list
     ((traversal traversal-list) (cons $1 $2))
     (() null))

    (interface-list
     ((interface interface-list) (cons $1 $2))
     (() null))

    (class-list
     ((class class-list) (cons $1 $2))
     (() null))

    (traversal
     ((TRAVERSAL name LBRACE traversal-case-list RBRACE) (cons $2 $4)))

    (traversal-case-list
     ((traversal-case traversal-case-list) (cons $1 $2))
     (() null))

    (traversal-case
     ((CASE name LBRACE traversal-step-list RBRACE) (cons $2 $4)))

    (traversal-step-list
     ((traversal-step traversal-step-list) (cons $1 $2))
     (() null))

    (traversal-step
     ((VISIT node SEMICOLON) `(visit ,$2))
     ((RECUR node SEMICOLON) `(recur ,$2))
     ((ITERATE node LBRACE traversal-step-list RBRACE) `(iterate ,$2 ,$4)))

    (interface
     ((INTERFACE name LBRACE attributes RBRACE)
      (cons $2 (ag-interface $4))))

    (class
     ((CLASS name COLON name LBRACE children attributes methods rules RBRACE)
      (cons $2 (ag-class $4 $6 $7 $8 $9))))

    (children
     ((CHILDREN LBRACE child-list RBRACE) $3))

    (child-list
     ((child SEMICOLON child-list) (cons $1 $3))
     (() null))

    (child
     ((name COLON name) `(,$1 . (unit ,$3)))
     ((name COLON name STAR) `(,$1 . (star ,$3)))
     ((name COLON name PLUS) `(,$1 . (plus ,$3))))

    (attributes
     ((ATTRIBUTES LBRACE attribute-list RBRACE) $3))

    (attribute-list
     ((attribute SEMICOLON attribute-list) (cons $1 $3))
     (() null))

    (attribute
     ((INPUT name COLON name) `(,$2 . (in ,$4)))
     ;((GHOST IDENT COLON IDENT) `(,$2 . (tmp ,$4)))
     ((OUTPUT name COLON name) `(,$2 . (out ,$4))))

    (methods
     ((METHODS LBRACE method-list RBRACE) $3))

    (method-list
     ((method SEMICOLON method-list) (cons $1 $3))
     (() null))

    (method
     ((LBRACE variable-list RBRACE name LPAREN RPAREN LBRACE variable-list RBRACE)
      `(,$4 . (,$2 . ,$8))))

    (rules
     ((RULES LBRACE rule-list RBRACE) $3))

    (rule-list
     ((rule SEMICOLON rule-list) (cons $1 $3))
     (() null))

    (rule
     ((reference DEFINE definition) (cons $1 $3)))

    (definition
     ((FOLDL LBRACKET node RBRACKET expression DOTDOT expression) `(foldl ,$3 ,$5 ,$7))
     ((FOLDR LBRACKET node RBRACKET expression DOTDOT expression) `(foldr ,$3 ,$5 ,$7))
     ((expression) $1))
    
    (expression
     ((reference) $1)
     ((BANG expression) `(! ,$2))
     ((expression AND expression) `(&& ,$1 ,$3))
     ((expression OR expression) `(\|\| ,$1 ,$3))
     ((expression PLUS expression) `(+ ,$1 ,$3))
     ((expression MINUS expression) `(- ,$1 ,$3))
     ((expression STAR expression) `(* ,$1 ,$3))
     ((expression SLASH expression) `(/ ,$1 ,$3))
     ((expression LT expression) `(< ,$1 ,$3))
     ((expression LE expression) `(<= ,$1 ,$3))
     ((expression EQ expression) `(== ,$1 ,$3))
     ((expression NE expression) `(!= ,$1 ,$3))
     ((expression GE expression) `(>= ,$1 ,$3))
     ((expression GT expression) `(> ,$1 ,$3))
     ((name LPAREN variable-list RPAREN) `(call ,$1 ,$3))
     ((IF expression THEN expression ELSE expression) `(ite ,$2 ,$4 ,$6))
     ((LPAREN expression RPAREN) $2))

    (variable-list
     ((reference COMMA variable-list) (cons $1 $3))
     ((reference) (list $1)))

    (reference
     ((name) `((unit self) . ,$1))
     ((node DOT name) `((unit ,$1) . ,$3))
     ((node FIRST DOT name) `((first ,$1) . ,$4))
     ((node PRED DOT name) `((pred ,$1) . ,$4))
     ((node CURR DOT name) `((curr ,$1) . ,$4))
     ((node SUCC DOT name) `((succ ,$1) . ,$4))
     ((node LAST DOT name) `((last ,$1) . ,$4)))

;    (field
;     ((SELF selector) (cons 'self $2)))
;
;    (selector
;     ((DOT name selector) (cons $2 $3))
;     ((DOT name) (list $2)))

    (node
     ((SELF) 'self)
     ((name) $1))

    (name
     ((IDENT) $1)
     ((TRAVERSAL) 'traversal)
     ((CASE) 'case)
     ((ITERATE) 'iterate)
     ((RECUR) 'recur)
     ((VISIT) 'visit)
     ((INTERFACE) 'interface)
     ((CLASS) 'class)
     ((CHILDREN) 'children)
     ((ATTRIBUTES) 'attributes)
     ((METHODS) 'methods)
     ((RULES) 'rules)
     ((INPUT) 'input)
     ((OUTPUT) 'output)))))

(define (parse-grammar port) ; input->grammar
  (parse (thunk (lex port))))

(define (file->grammar path)
  (call-with-input-file path parse-grammar #:mode 'text))

; ----------
; Serializer
; ----------

(define/match (grammar->string grammar)
  [((ag-grammar iface-list class-list trav-list))
   (string-join
    (list (string-join (map traversal->string trav-list) "\n\n")
          (string-join (map interface->string iface-list) "\n\n")
          (string-join (map class->string class-list) "\n\n"))
    "\n\n")])

(define/match (traversal->string trav)
  [((cons name case-list))
   (format "traversal ~a {\n~a\n}"
           name
           (string-join (map traversal-case->string case-list) "\n"))])

(define/match (traversal-case->string case)
  [((cons class-name step-list))
    (format "  case ~a {\n~a\n  }"
            class-name
            (string-join (map traversal-step->string step-list) "\n"))])

(define/match (traversal-step->string step)
  [(`(visit ,node))
   (format "    visit ~a;" node)]
  [(`(recur ,node))
   (format "    recur ~a;" node)]
  [(`(iterate ,node ,steps))
   (format "    iterate ~a {\n~a\n    }"
           node
           (string-join (map traversal-step->string steps) "\n"))])

(define/match (interface->string iface)
  [((cons name (ag-interface attributes)))
   (format "interface ~a {\n  attributes {~a\n  }\n}"
           name
           (string-join (map attribute->string attributes) "\n" #:before-first "\n"))])

(define/match (class->string class-ast)
  [((cons name (ag-class iface-name children attributes methods rules)))
   (define (inner-block header item->string items)
     (define body
       (if (null? items)
           " "
           (string-join (map item->string items) "\n" #:before-first "\n")))
     (format "  ~a {~a\n  }" header body))
   (format "class ~a : ~a {~a\n}"
           name
           iface-name
           (string-join
            (list (inner-block "children" child->string children)
                  (inner-block "attributes" attribute->string attributes)
                  (inner-block "methods" method->string methods)
                  (inner-block "rules" rule->string rules))
            "\n" #:before-first "\n"))])

(define/match (attribute->string attribute)
  [((input name type))
   (format "    input ~a : ~a;" name type)]
  [((output name type))
   (format "    output ~a : ~a;" name type)])

(define/match (child->string child)
  [((child1 name iface-name))
   (format "    ~a : ~a;" name iface-name)]
  [((child* name iface-name))
   (format "    ~a : ~a*;" name iface-name)]
  [((child+ name iface-name))
   (format "    ~a : ~a+;" name iface-name)])

(define/match (method->string method-ast)
  [(`(,name . (,pre-refs . ,post-refs)))
   (format "    { ~a } ~a() { ~a };"
           (string-join (map reference->string pre-refs) ", ")
           name
           (string-join (map reference->string post-refs) ", "))])

(define/match (rule->string rule)
  [(`(,ref . ,def))
   (format "    ~a := ~a;"
           (reference->string ref)
           (definition->string def))])

(define/match (definition->string def)
  [(`(foldl ,child-name ,default-expr ,next-expr))
   (format "foldl[~a] ~a .. ~a"
           child-name
           (expression->string default-expr)
           (expression->string next-expr))]
  [(`(foldr ,child-name ,default-expr ,previous-expr))
   (format "foldr[~a] ~a .. ~a"
           child-name
           (expression->string default-expr)
           (expression->string previous-expr))]
  [(expr)
   (expression->string expr)])

(define/match (expression->string expr)
  [(`(! ,expr))
   (format "!(~a)"
           (expression->string expr))]
  [(`(,(and op (or '+ '- '* '/ '< '<= '== '!= '>= '> '&& '\|\|)) ,left-expr ,right-expr))
   (format "(~a) ~a (~a)"
           (expression->string left-expr)
           op
           (expression->string right-expr))]
  [(`(call ,(symbol fun) ,arg-exprs))
   (format "~a(~a)"
           fun
           (string-join (map expression->string arg-exprs) ", "))]
  [(`(ite ,cond-expr ,then-expr ,else-expr))
   (format "if ~a then ~a else ~a"
           (expression->string cond-expr)
           (expression->string then-expr)
           (expression->string else-expr))]
  [(ref)
   (reference->string ref)])

(define/match (reference->string variable)
  [(`((unit ,(symbol node)) . ,(symbol label)))
   (format "~a.~a" node label)]
  [(`((first ,(symbol node)) . ,(symbol label)))
   (format "~a$0.~a" node label)]
  [(`((pred ,(symbol node)) . ,(symbol label)))
   (format "~a$-.~a" node label)]
  [(`((curr ,(symbol node)) . ,(symbol label)))
   (format "~a$i.~a" node label)]
  [(`((succ ,(symbol node)) . ,(symbol label)))
   (format "~a$+.~a" node label)]
  [(`((last ,(symbol node)) . ,(symbol label)))
   (format "~a$$.~a" node label)])
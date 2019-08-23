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
   OPTION COLON SEMICOLON COMMA DOT
   TRAVERSAL CASE ITERATE REVERSE RECUR CALL EVAL SKIP HOLE
   INTERFACE CLASS TRAIT
   CHILDREN ATTRIBUTES STATEMENTS RULES
   INPUT OUTPUT LOCAL
   DEFINE FOLDL FOLDR SCANL SCANR DOTDOT
   INDEXED
   ACCUMULATOR SUPREMUM
   PREDECESSOR SUCCESSOR
   FIRST LAST
   LENGTH
   PHASE
   BANG
   PLUS MINUS STAR SLASH
   LT LE EQ NE GE GT
   AND OR
   QUESTION
   IF THEN ELSE
   TRUE FALSE
   SELF
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
         (:* (:or (char-range "a" "z") (char-range "A" "Z") "_"
                  (char-range "0" "9")
                  "<" ">" ":"))))))

(define lex
  (lexer-src-pos
   [(comment) (return-without-pos (lex input-port))]
   ["}" (token-RBRACE)]
   ["{" (token-LBRACE)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["?" (token-OPTION)]
   [":" (token-COLON)]
   [";" (token-SEMICOLON)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   ["traversal" (token-TRAVERSAL)]
   ["case" (token-CASE)]
   ["iterate" (token-ITERATE)]
   ["reverse" (token-REVERSE)]
   ["recur" (token-RECUR)]
   ["eval" (token-EVAL)]
   ["skip" (token-SKIP)]
   ["??" (token-HOLE)]
   ["interface" (token-INTERFACE)]
   ["class" (token-CLASS)]
   ["trait" (token-TRAIT)]
   ["children" (token-CHILDREN)]
   ["attributes" (token-ATTRIBUTES)]
   ["statements" (token-STATEMENTS)]
   ["rules" (token-RULES)]
   ["input" (token-INPUT)]
   ["output" (token-OUTPUT)]
   ["local" (token-LOCAL)]
   [":=" (token-DEFINE)]
   ["foldl" (token-FOLDL)]
   ["foldr" (token-FOLDR)]
   ["scanl" (token-SCANL)]
   ["scanr" (token-SCANR)]
   [".." (token-DOTDOT)]
   ["[i]" (token-INDEXED)] ; children[i].attribute // indexed node
   ["[@]" (token-ACCUMULATOR)] ; children[@].attribute, self[@].attribute // current accumulator
   ["[$]" (token-SUPREMUM)] ; children[$].attribute, self[$].attribute // final accumulator
   ["[i-1]" (token-PREDECESSOR)] ; children[i-1].attribute : default // backward peeked node
   ["[i+1]" (token-SUCCESSOR)] ; children[i+1].attribute : default // forward peeked node
   ["[0]" (token-FIRST)] ; children[0].attribute : default // first node
   ["[-1]" (token-LAST)] ; children[-1].attribute : default // last node
   ["[#]" (token-LENGTH)]
   ["%" (token-PHASE)]
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
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["self" (token-SELF)]
   [(integer) (token-INT (string->number lexeme))]
   [(float) (token-FLOAT (string->number lexeme))]
   [(identifier) (token-IDENT (string->symbol lexeme))]
   [whitespace (return-without-pos (lex input-port))]
   [(eof) (token-EOF)]))

; ------
; Parser
; ------

(define parse
  (parser
   (start program)
   (src-pos)
   (tokens tkns e-tkns)
   (end EOF)
   (error
    (λ (_ token lexeme start stop)
      (printf "Unexpected token ~a~a on line ~a at column ~a~n"
              token (if lexeme (format "(~a)" lexeme) "")
              (position-line start) (position-col start))))
   (precs
    (nonassoc LPAREN RPAREN)
    (nonassoc IF THEN ELSE)
    (left COLON)
    (left OR)
    (left AND)
    (nonassoc LT LE EQ NE GE GT)
    (left STAR SLASH)
    (left PLUS MINUS)
    (nonassoc BANG)
    (left DOT))
   (grammar
    (program
     ((entity-list) (ag:make-grammar $1)))

    (entity-list
     ((entity entity-list) (cons $1 $2))
     (() null))

    (entity
     ((traversal) $1)
     ((interface) $1)
     ((class) $1)
     ((trait) $1))

    (traversal
     ((TRAVERSAL name LBRACE visitor-list RBRACE) (ag:traversal $2 $4)))

    (visitor-list
     ((visitor visitor-list) (cons $1 $2))
     (() null))

    (visitor
     ((CASE name LBRACE command-list RBRACE) (ag:visitor $2 $4)))

    (command-list
     ((command command-list) (cons $1 $2))
     (() null))

    (command
     ((ITERATE name LBRACE command-list RBRACE) (ag:iter/left $2 $4))
     ((REVERSE name LBRACE command-list RBRACE) (ag:iter/right $2 $4))
     ((RECUR name SEMICOLON) (ag:recur $2))
     ((EVAL reference SEMICOLON) (ag:eval $2))
     ((SKIP SEMICOLON) (ag:skip))
     ((HOLE SEMICOLON) (ag:hole)))

    (interface
     ((INTERFACE name LBRACE attribute-list RBRACE) (ag:interface $2 $4)))

    (class
      ((CLASS name COLON name LBRACE body RBRACE)
       (ag:class $2 $4 null $6))
      ((CLASS name LPAREN trait-list RPAREN COLON name LBRACE body RBRACE)
       (ag:class $2 $7 $4 $9)))

    (trait-list
     ((name COMMA trait-list) (cons $1 $3))
     ((name) (list $1)))

    (trait
     ((TRAIT name LBRACE body RBRACE)
      (ag:trait $2 $4)))

    (body
     ((block body) (ag:body-union $1 $2))
     (() (ag:body null null null)))

    (block
     ((CHILDREN LBRACE child-list RBRACE) (ag:body $3 null null))
     ((ATTRIBUTES LBRACE attribute-list RBRACE) (ag:body null $3 null))
     ((STATEMENTS LBRACE rule-list RBRACE) (ag:body null null $3))
     ((RULES LBRACE rule-list RBRACE) (ag:body null null $3)))

    (child-list
     ((child SEMICOLON child-list) (cons $1 $3))
     (() null))

    (child
     ((name COLON name) (ag:child/one $1 $3))
     ((name COLON LBRACKET name RBRACKET) (ag:child/seq $1 $4)))

    (attribute-list
     ((attribute SEMICOLON attribute-list) (cons $1 $3))
     (() null))

    (attribute
     ((INPUT field COLON name) (ag:label/in $2 $4))
     ((OUTPUT field COLON name) (ag:label/out $2 $4))
     ((LOCAL attribute-path COLON name) (ag:label/var $2 $4)))

    ; XXX: The option modifier should apply to attribute path components.
    ; Ex.: margin?.{left?, right?, top, bottom}
    (attribute-path
     ((IDENT DOT LBRACE attribute-path-list RBRACE) (list $1 $4))
     ((IDENT) $1))

    (attribute-path-list
     ((attribute-path COMMA attribute-path-list) (cons $1 $3))
     ((attribute-path) (list $1)))

    (rule-list
     ((rule SEMICOLON rule-list) (cons $1 $3))
     (() null))

    (rule
     ((node DOT field DEFINE scalar) (ag:rule/scalar (cons $1 $3) $5 #f))
     ((node INDEXED DOT field DEFINE vector) (ag:rule/vector (cons $1 $4) $6 $1)))

    (scalar
     ((FOLDL term DOTDOT term) (ag:fold/left $2 $4))
     ((FOLDR term DOTDOT term) (ag:fold/right $2 $4))
     ((term) $1))

    (vector
     ((SCANL term DOTDOT term) (ag:scan/left $2 $4))
     ((SCANR term DOTDOT term) (ag:scan/right $2 $4))
     ((term) $1))

    (term
     ((constant) (ag:const $1))
     ((node DOT field) (ag:field/get (cons $1 $3)))
     ((node INDEXED DOT field) (ag:field/cur (cons $1 $4)))
     ((node ACCUMULATOR DOT field) (ag:field/acc (cons $1 $4)))
     ((node SUPREMUM DOT field) (ag:field/sup (cons $1 $4)))
     ((node PREDECESSOR DOT field COLON term) (ag:field/pred (cons $1 $4) $6))
     ((node SUCCESSOR DOT field COLON term) (ag:field/succ (cons $1 $4) $6))
     ((node FIRST DOT field COLON term) (ag:field/first (cons $1 $4) $6))
     ((node LAST DOT field COLON term) (ag:field/last (cons $1 $4) $6))
     ((node LENGTH) (ag:length $1))
     ((BANG term) (ag:expr '! (list $2)))
     ((term AND term) (ag:expr '&& (list $1 $3)))
     ((term OR term) (ag:expr '\|\| (list $1 $3)))
     ((term PLUS term) (ag:expr '+ (list $1 $3)))
     ((term MINUS term) (ag:expr '- (list $1 $3)))
     ((term STAR term) (ag:expr '* (list $1 $3)))
     ((term SLASH term) (ag:expr '/ (list $1 $3)))
     ((term LT term) (ag:expr '< (list $1 $3)))
     ((term LE term) (ag:expr '<= (list $1 $3)))
     ((term EQ term) (ag:expr '== (list $1 $3)))
     ((term NE term) (ag:expr '!= (list $1 $3)))
     ((term GE term) (ag:expr '>= (list $1 $3)))
     ((term GT term) (ag:expr '> (list $1 $3)))
     ((name LPAREN RPAREN) (ag:call $1 null))
     ((name LPAREN term-list RPAREN) (ag:call $1 $3))
     ((term DOT name LPAREN RPAREN) (ag:invoke $1 $3 null))
     ((term DOT name LPAREN term-list RPAREN) (ag:invoke $1 $3 $5))
     ((IF term THEN term ELSE term) (ag:branch $2 $4 $6))
     ((LPAREN term RPAREN) $2))

    (term-list
     ((term COMMA term-list) (cons $1 $3))
     ((term) (list $1)))

    (constant
     ((TRUE) #t)
     ((FALSE) #f)
     ((INT) $1)
     ((FLOAT) $1))

    (reference
     ((node DOT field) (cons $1 $3)))

    (node
     ((SELF) 'self)
     ((name) $1))

    (field
     ((path) (symbol-join $1 ".")))

    (path
     ((name DOT path) (cons $1 $3))
     ((name) (list $1)))

    (name
     ((IDENT) $1)
     ((TRAVERSAL) 'traversal)
     ((CASE) 'case)
     ((ITERATE) 'iterate)
     ((REVERSE) 'reverse)
     ((RECUR) 'recur)
     ((CALL) 'call)
     ((EVAL) 'eval)
     ((SKIP) 'skip)
     ((INTERFACE) 'interface)
     ((CLASS) 'class)
     ((TRAIT) 'trait)
     ((CHILDREN) 'children)
     ((ATTRIBUTES) 'attributes)
     ((STATEMENTS) 'statements)
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

(define (grammar->string G)
  (string-append "/* Automatically formatted attribute grammar */"
                 (list->string traversal->string (ag:grammar-traversals G))
                 (list->string interface->string (ag:grammar-interfaces G))
                 (list->string class->string (ag:grammar-classes G))
                 (list->string trait->string (ag:grammar-traits G))
                 "\n"))

(define/match (traversal->string traversal)
  [((ag:traversal name visitors))
   (format "\n\ntraversal ~a {~a\n}"
           name
           (list->string visitor->string visitors))])

(define/match (visitor->string visitor)
  [((ag:visitor (ag:class class-name _ _ _ _) commands))
   (format "\n  case ~a {~a\n  }"
           class-name
           (list->string command->string commands))])

(define/match (command->string command)
  [((ag:iter/left child commands))
   (format "\n    iterate ~a left {~a\n    }"
           child
           (list->string iterated-command->string commands))]
  [((ag:iter/right child commands))
   (format "\n    iterate ~a right {~a\n    }"
           child
           (list->string iterated-command->string commands))]
  [((ag:recur child))
   (format "\n    recur ~a;" child)]
  [((ag:eval (cons object label)))
   (format "\n    eval ~a.~a;" object label)]
  [((ag:hole)) "\n    ??;"]
  [((ag:skip)) "\n    skip;"])

(define/match (iterated-command->string command)
  [((ag:recur child))
   (format "\n      recur ~a;" child)]
  [((ag:eval (cons object label)))
   (format "\n      eval ~a.~a;" object label)]
  [((ag:hole)) "\n      ??;"]
  [((ag:skip)) "\n      skip;"])

(define/match (identify-interface interface)
  [((? symbol? name)) name]
  [((ag:interface name _ _)) name])

(define/match (identify-trait trait)
  [((? symbol? name)) name]
  [((ag:trait name _)) name])

(define/match (interface->string interface)
  [((ag:interface name labels _))
   (format "\n\ninterface ~a {~a\n}"
           name
           (if (null? labels)
               " "
               (list->string label->string labels)))])

(define/match (class->string class)
  [((ag:class name interface traits body _))
   (format "\n\nclass ~a~a : ~a ~a"
           name
           (if (null? traits)
               ""
               (list->string (compose symbol->string identify-trait)
                             traits ", " "(" ")"))
           (identify-interface interface)
           (body->string body))])

(define/match (trait->string trait)
  [((ag:trait name body))
   (format "\n\ntrait ~a ~a"
           name
           (body->string body))])

(define/match (body->string body)
  [((ag:body children labels rules))
   (format "{~a~a~a\n}"
           (block->string "children" child->string children)
           (block->string "attributes" label->string labels)
           (block->string "statements" rule->string rules))])

(define (block->string header entry->string entries)
  (format "\n  ~a {~a\n  }"
          header
          (list->string entry->string entries)))

(define/match (label->string attribute)
  [((ag:label/in name type))
   (format "\n    input ~a : ~a;" name type)]
  [((ag:label/out name type))
   (format "\n    output ~a : ~a;" name type)])

(define/match (child->string child)
  [((ag:child/one name interface))
   (format "\n    ~a : ~a;" name (identify-interface interface))]
  [((ag:child/seq name interface))
   (format "\n    ~a : [~a];" name (identify-interface interface))])

(define/match (rule->string rule)
  [((ag:rule/scalar (cons object field) formula _))
   (format "\n    ~a.~a := ~a;"
           object
           field
           (formula->string formula))]
  [((ag:rule/vector (cons object field) formula _))
   (format "\n    ~a[i].~a := ~a;"
           object
           field
           (formula->string formula))])

(define/match (formula->string formula)
  [((ag:fold/left init next))
   (format "foldl ~a .. ~a"
           (term->string init)
           (term->string next))]
  [((ag:fold/right init next))
   (format "foldr ~a .. ~a"
           (term->string init)
           (term->string next))]
  [((ag:scan/left init next))
   (format "scanl ~a .. ~a"
           (term->string init)
           (term->string next))]
  [((ag:scan/right init next))
   (format "scanr ~a .. ~a"
           (term->string init)
           (term->string next))]
  [(term)
   (term->string term)])

(define/match (term->string term)
  [((ag:const #t)) "true"]
  [((ag:const #f)) "false"]
  [((ag:const (? number? n))) (number->string n)]
  [((ag:field/get (cons object label)))
   (format "~a.~a"
           object
           label)]
  [((ag:field/cur (cons object label)))
   (format "~a[i].~a"
           object
           label)]
  [((ag:field/acc (cons object label)))
   (format "~a[@].~a"
           object
           label)]
  [((ag:field/sup (cons object label)))
   (format "~a[$].~a"
           object
           label)]
  [((ag:field/pred (cons object label) default))
   (format "(~a[i-1].~a : ~a)"
           object
           label
           (term->string default))]
  [((ag:field/succ (cons object label) default))
   (format "(~a[i+1].~a : ~a)"
           object
           label
           (term->string default))]
  [((ag:field/first (cons object label) default))
   (format "(~a[0].~a : ~a)"
           object
           label
           (term->string default))]
  [((ag:field/last (cons object label) default))
   (format "(~a[-1].~a : ~a)"
           object
           label
           (term->string default))]
  [((ag:length child))
   (format "~a[#]"
           child)]
  [((ag:expr unop (list only)))
   (format "~a~a"
           unop
           (term->string only))]
  [((ag:expr binop (list left right)))
   (format "(~a ~a ~a)"
           (term->string left)
           binop
           (term->string right))]
  [((ag:call fun args))
   (format "~a(~a)"
           fun
           (list->string term->string args ", "))]
  [((ag:invoke recv meth args))
   (format "~a.~a(~a)"
           (term->string recv)
           meth
           (list->string term->string args ", "))]
  [((ag:branch if then else))
   (format "(if ~a then ~a else ~a)"
           (term->string if)
           (term->string then)
           (term->string else))])

(define/match (attribute->string attribute)
  [(`(,object . ,label)) (format "~a.~a" object label)])

(define (list->string element->string elements [separator ""]
                      [left-delimiter ""] [right-delimiter ""])
  (string-join (map element->string elements)
               separator
               #:before-first left-delimiter
               #:after-last right-delimiter))

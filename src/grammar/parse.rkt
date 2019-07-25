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
   TRAVERSAL CASE ITERATE REVERSE RECUR CALL EVAL SKIP HOLE
   INTERFACE CLASS TRAIT
   CHILDREN ATTRIBUTES STATEMENTS RULES
   INPUT OUTPUT
   DEFINE FOLDL FOLDR DOTDOT AT FIRST LAST
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
   ["@" (token-AT)]
   ["[0]" (token-FIRST)]
   ["[#]" (token-LAST)]
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
   ["?" (token-QUESTION)]
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
    (nonassoc QUESTION COLON)
    (left OR)
    (left AND)
    (nonassoc LT LE EQ NE GE GT)
    (left STAR SLASH)
    (left PLUS MINUS)
    (nonassoc BANG))
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
     ((OUTPUT field COLON name) (ag:label/out $2 $4)))

    (rule-list
     ((rule SEMICOLON rule-list) (cons $1 $3))
     (() null))

    (rule
     ((reference DEFINE formula) (ag:rule $1 $3)))

    (formula
     ((FOLDL term DOTDOT term) (ag:fold/left $2 $4))
     ((FOLDR term DOTDOT term) (ag:fold/right $2 $4))
     ((term) $1))

    (term
     ((constant) (ag:const $1))
     ((reference) (ag:field $1))
     ((AT LBRACE reference RBRACE) (ag:accum $3))
     ((FIRST LBRACE reference COLON term RBRACE) (ag:index/first $3 $5))
     ((LAST LBRACE reference COLON term RBRACE) (ag:index/last $3 $5))
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
     ((name LPAREN RPAREN) (ag:expr $1 null))
     ((name LPAREN term-list RPAREN) (ag:expr $1 $3))
     ((IF term THEN term ELSE term) (ag:ite $2 $4 $6))
     ((term QUESTION term COLON term) (ag:ite $1 $3 $5))
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
  [((ag:rule attr formula _))
   (format "\n    ~a := ~a;"
           (attribute->string attr)
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
  [(term)
   (term->string term)])

(define/match (term->string term)
  [((ag:const #t)) "true"]
  [((ag:const #f)) "false"]
  [((ag:const (? number? n))) (number->string n)]
  [((ag:field attr))
   (attribute->string attr)]
  [((ag:accum attr))
   (format "@{~a}"
           (attribute->string attr))]
  [((ag:index/first attr default))
   (format "[0]{~a : ~a}"
           (attribute->string attr)
           (term->string default))]
  [((ag:index/last attr default))
   (format "[#]{~a : ~a}"
           (attribute->string attr)
           (term->string default))]
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
  [((ag:ite if then else))
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

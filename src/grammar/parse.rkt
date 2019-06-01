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
   TRAVERSAL CASE ITERATE LEFT RIGHT RECUR CALL EVAL SKIP HOLE
   INTERFACE CLASS TRAIT
   CHILDREN ATTRIBUTES STATEMENTS RULES METHODS
   INPUT OUTPUT
   DEFINE FOLDL FOLDR DOTDOT
   BANG
   PLUS MINUS STAR SLASH
   LT LE EQ NE GE GT
   AND OR
   IF THEN ELSE
   TRUE FALSE
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
  (lexer-src-pos
   [(comment) (return-without-pos (lex input-port))]
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
   ["left" (token-LEFT)]
   ["right" (token-RIGHT)]
   ["recur" (token-RECUR)]
   ["call" (token-CALL)]
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
   ["methods" (token-METHODS)]
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
   ["true" (token-TRUE)]
   ["false" (token-FALSE)]
   ["self" (token-SELF)]
   ["$0" (token-FIRST)]
   ["$-" (token-PRED)]
   ["$i" (token-CURR)]
   ["$+" (token-SUCC)]
   ["$$" (token-LAST)]
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
    (left OR)
    (left AND)
    (nonassoc LT LE EQ NE GE GT)
    (left STAR SLASH)
    (left PLUS MINUS)
    (nonassoc BANG))
   (grammar
    (program
     ((traversal program) (ag-grammar-add-traversal $2 $1))
     ((interface program) (ag-grammar-add-interface $2 $1))
     ((class program) (ag-grammar-add-class $2 $1))
     ((trait program) (ag-grammar-add-trait $2 $1))
     (() (ag-grammar null null null null)))

    (traversal
     ((TRAVERSAL name LBRACE visitor-list RBRACE) (cons $2 $4)))

    (visitor-list
     ((visitor visitor-list) (cons $1 $2))
     (() null))

    (visitor
     ((CASE name LBRACE command-list RBRACE) (cons $2 $4)))

    (command-list
     ((command command-list) (cons $1 $2))
     (() null))

    (command
     ((ITERATE LBRACKET LEFT RBRACKET name LBRACE command-list RBRACE) `(iter-left ,$5 ,$7))
     ((ITERATE LBRACKET RIGHT RBRACKET name LBRACE command-list RBRACE) `(iter-right ,$5 ,$7))
     ((RECUR name SEMICOLON) `(recur ,$2))
     ((EVAL node DOT name SEMICOLON) `(eval ,$2 ,$4))
     ((CALL name SEMICOLON) `(call ,$2))
     ((SKIP SEMICOLON) `(skip))
     ((HOLE SEMICOLON) `(hole)))

    (interface
     ((INTERFACE name LBRACE attribute-list RBRACE)
      (cons $2 $4)))

    (class
      ((CLASS name COLON name LBRACE body RBRACE)
       (cons $2 (ag-class $4 null $6)))
      ((CLASS name LPAREN trait-list RPAREN COLON name LBRACE body RBRACE)
       (cons $2 (ag-class $7 $4 $9))))

    (trait-list
     ((trait COMMA trait-list) (cons $1 $3))
     ((trait) (list $1)))

    (trait
     ((TRAIT name LBRACE body RBRACE)
      (cons $2 $4)))

    (body
     ((block body) (ag-body-union $1 $2))
     (() (ag-body null null null null)))

    (block
     ((CHILDREN LBRACE child-list RBRACE) (ag-body $3 null null null))
     ((ATTRIBUTES LBRACE attribute-list RBRACE) (ag-body null $3 null null))
     ((STATEMENTS LBRACE statement-list RBRACE) (ag-body null null $3 null))
     ((RULES LBRACE statement-list RBRACE) (ag-body null null $3 null))
     ((METHODS LBRACE method-list RBRACE) (ag-body null null null $3)))

    (child-list
     ((child SEMICOLON child-list) (cons $1 $3))
     (() null))

    (child
     ((name COLON name) `(,$1 . (unit ,$3)))
     ((name COLON name STAR) `(,$1 . (star ,$3)))
     ((name COLON name PLUS) `(,$1 . (plus ,$3))))

    (attribute-list
     ((attribute SEMICOLON attribute-list) (cons $1 $3))
     (() null))

    (attribute
     ((INPUT name COLON name) `(,$2 . (in ,$4)))
     ;((GHOST IDENT COLON IDENT) `(,$2 . (tmp ,$4)))
     ((OUTPUT name COLON name) `(,$2 . (out ,$4))))

    (statement-list
     ((statement SEMICOLON statement-list) (cons $1 $3))
     (() null))

    (statement
     ((reference DEFINE definition) (cons $1 $3)))

    (definition
     ((FOLDL LBRACKET node RBRACKET expression DOTDOT expression) `(foldl ,$3 ,$5 ,$7))
     ((FOLDR LBRACKET node RBRACKET expression DOTDOT expression) `(foldr ,$3 ,$5 ,$7))
     ((expression) $1))

    (expression
     ((TRUE) #t)
     ((FALSE) #f)
     ((INT) $1)
     ((FLOAT) $1)
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
     ((name LPAREN RPAREN) `(,$1))
     ((name LPAREN expression-list RPAREN) `(,$1 . ,$3))
     ((IF expression THEN expression ELSE expression) `(ite ,$2 ,$4 ,$6))
     ((LPAREN expression RPAREN) $2))

    (expression-list
     ((expression COMMA expression-list) (cons $1 $3))
     ((expression) (list $1)))

    (reference
     ((name) `((unit self) . ,$1))
     ((node DOT name) `((unit ,$1) . ,$3))
     ((node FIRST DOT name) `((first ,$1) . ,$4))
     ((node PRED DOT name) `((pred ,$1) . ,$4))
     ((node CURR DOT name) `((curr ,$1) . ,$4))
     ((node SUCC DOT name) `((succ ,$1) . ,$4))
     ((node LAST DOT name) `((last ,$1) . ,$4)))

    (method-list
     ((method SEMICOLON method-list) (cons $1 $3))
     (() null))

    (method
     ((LBRACE variable-list RBRACE name LPAREN RPAREN LBRACE variable-list RBRACE)
      `(,$4 . (,$2 . ,$8))))

    (variable-list
     ((reference COMMA variable-list) (cons $1 $3))
     ((reference) (list $1)))

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
     ((LEFT) 'left)
     ((RIGHT) 'right)
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
     ((METHODS) 'methods)
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
  [((ag-grammar iface-list class-list trait-list trav-list))
   (string-append "/* Automatically formatted attribute grammar */"
                  (list->string traversal->string trav-list)
                  (list->string interface->string iface-list)
                  (list->string class->string class-list)
                  (list->string trait->string trait-list)
                  "\n")])

(define/match (traversal->string trav)
  [((cons trav-name visitors))
   (format "\n\ntraversal ~a {~a\n}"
           trav-name
           (list->string visitor->string visitors))])

(define/match (visitor->string visitor)
  [((cons class-name body))
   (format "\n  case ~a {~a\n  }"
           class-name
           (list->string command->string body))])

(define/match (command->string command)
  [(`(iter-left ,child ,commands))
   (format "\n    iterate[left] ~a {~a\n    }"
           child
           (list->string iterated-command->string commands))]
  [(`(iter-right ,child ,commands))
   (format "\n    iterate[right] ~a {~a\n    }"
           child
           (list->string iterated-command->string commands))]
  [(`(recur ,child))
   (format "\n    recur ~a;" child)]
  [(`(call ,method-name))
   (format "\n    call ~a;" method-name)]
  [(`(eval ,node ,label))
   (format "\n    eval ~a.~a;" node label)]
  [(`(hole)) "\n    ??;"]
  [(`(skip)) "\n    skip;"])

(define/match (iterated-command->string command)
  [(`(recur ,child))
   (format "\n      recur ~a;" child)]
  [(`(call ,method-name))
   (format "\n      call ~a;" method-name)]
  [(`(eval ,node ,label))
   (format "\n      eval ~a.~a;" node label)]
  [(`(hole)) "\n      ??;"]
  [(`(skip)) "\n      skip;"])

(define/match (interface->string iface)
  [((cons name attributes))
   (format "\n\ninterface ~a {~a\n}"
           name
           (if (null? attributes)
               " "
               (list->string attribute->string attributes)))])

(define/match (class->string class)
  [((cons name (ag-class interface traits body)))
   (format "\n\nclass ~a~a : ~a ~a"
           name
           (if (null? traits)
               ""
               (list->string symbol->string traits ", " "(" ")"))
           interface
           (body->string body))])

(define/match (trait->string trait)
  [((cons name body))
   (format "\n\ntrait ~a ~a"
           name
           (body->string body))])

(define/match (body->string body)
  [((ag-body children attributes statements methods))
   (format "{~a~a~a~a\n}"
           (block->string "children" child->string children)
           (block->string "attributes" attribute->string attributes)
           (block->string "statements" statement->string statements)
           (block->string "methods" method->string methods))])

(define (block->string header entry->string entries)
  (format "\n  ~a {~a\n  }"
          header
          (list->string entry->string entries)))

(define/match (attribute->string attribute)
  [((cons label (ag:input type)))
   (format "\n    input ~a : ~a;" label type)]
  [((cons label (ag:output type)))
   (format "\n    output ~a : ~a;" label type)])

(define/match (child->string child)
  [((child1 name iface-name))
   (format "\n    ~a : ~a;" name iface-name)]
  [((child* name iface-name))
   (format "\n    ~a : ~a*;" name iface-name)]
  [((child+ name iface-name))
   (format "\n    ~a : ~a+;" name iface-name)])

(define/match (statement->string statement)
  [(`(,ref . ,def))
   (format "\n    ~a := ~a;"
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
  [(#t) "true"]
  [(#f) "false"]
  [((? number?)) (number->string expr)]
  [((ag:reference _ _))
   (reference->string expr)]
  [(`(! ,expr))
   (format "!~a"
           (expression->string expr))]
  [(`(,(and op (or '+ '- '* '/ '< '<= '== '!= '>= '> '&& '\|\|)) ,left-expr ,right-expr))
   (format "(~a ~a ~a)"
           (expression->string left-expr)
           op
           (expression->string right-expr))]
  [(`(ite ,cond-expr ,then-expr ,else-expr))
   (format "(if ~a then ~a else ~a)"
           (expression->string cond-expr)
           (expression->string then-expr)
           (expression->string else-expr))]
  [((list (symbol fun) arg-exprs ...))
   (format "~a(~a)"
           fun
           (list->string expression->string arg-exprs ", "))])

(define/match (method->string method)
  [(`(,name . (,precondition . ,postcondition)))
   (format "\n    { ~a } ~a() { ~a };"
           (list->string reference->string precondition ", ")
           name
           (list->string reference->string postcondition ", "))])

(define/match (reference->string reference)
  [((ag:reference object label))
   (format "~a.~a" (object->string object) label)])

  (define/match (object->string object)
    [((ag:object1 node)) (format "~a" node)]
    [((ag:object$0 node)) (format "~a$0" node)]
    [((ag:object$- node)) (format "~a$-" node)]
    [((ag:object$i node)) (format "~a$i" node)]
    [((ag:object$+ node)) (format "~a$+" node)]
    [((ag:object$$ node)) (format "~a$$" node)])

(define (list->string element->string elements [separator ""]
                      [left-delimiter ""] [right-delimiter ""])
  (string-join (map element->string elements)
               separator
               #:before-first left-delimiter
               #:after-last right-delimiter))

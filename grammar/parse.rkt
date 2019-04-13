#lang racket

; Parser and serializer for language of attribute grammars

(require "../utility.rkt"
         "syntax.rkt"
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(provide parse-grammar
         file->grammar
         serialize-grammar)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns (TRAVERSAL
                             CASE
                             ITERATE
                             RECUR
                             VISIT
                             INTERFACE
                             CLASS
                             RBRACE
                             LBRACE
                             CHILD
                             CHILDREN
                             INPUT
                             OUTPUT
                             METHOD
                             READ
                             WRITE
                             SELF
                             SEMICOLON
                             COLON
                             LPAREN
                             RPAREN
                             LBRACKET
                             RBRACKET
                             COMMA
                             DOT
                             EOF))

(define-tokens tkns (IDENT))

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

(define-lex-trans identifier
  (syntax-rules ()
    ((_)
     (:: (:or (char-range "a" "z") (char-range "A" "Z") "_")
         (:* (:or (char-range "a" "z") (char-range "A" "Z") "_" (char-range "0" "9")))))))

(define lex
  (lexer
   [(comment) (lex input-port)]
   ["traversal" (token-TRAVERSAL)]
   ["case" (token-CASE)]
   ["iterate" (token-ITERATE)]
   ["recur" (token-RECUR)]
   ["visit" (token-VISIT)]
   ["interface" (token-INTERFACE)]
   ["}" (token-RBRACE)]
   ["{" (token-LBRACE)]
   ["child" (token-CHILD)]
   ["children" (token-CHILDREN)]
   ["input" (token-INPUT)]
   ["output" (token-OUTPUT)]
   ["class" (token-CLASS)]
   ["method" (token-METHOD)]
   ["read" (token-READ)]
   ["write" (token-WRITE)]
   ["self" (token-SELF)]
   [";" (token-SEMICOLON)]
   [":" (token-COLON)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["." (token-DOT)]
   ["," (token-COMMA)]
   [(identifier) (token-IDENT (string->symbol lexeme))]
   [whitespace (lex input-port)]
   [(eof) (token-EOF)]))

; -----------------
; Parser Definition
; -----------------

(define parse
  (parser
   (start program)
   (tokens tkns e-tkns)
   (end EOF)
   (error (λ (_ token lexeme) (printf "Unexpected token: ~a(~a)~n" token lexeme)))
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
     ((TRAVERSAL IDENT LBRACE traversal-case-list RBRACE) (cons $2 $4)))

    (traversal-case-list
     ((traversal-case traversal-case-list) (cons $1 $2))
     (() null))

    (traversal-case
     ((CASE IDENT LBRACE traversal-step-list RBRACE) (cons $2 $4)))

    (traversal-step-list
     ((traversal-step traversal-step-list) (cons $1 $2))
     (() null))

    (traversal-step
     ((VISIT SELF SEMICOLON) (ag-visit)) ; TODO: Support visits of other nodes?
     ((RECUR field SEMICOLON) (ag-recur $2))
     ((ITERATE field LBRACE traversal-step-list RBRACE) (ag-iterate $2 $4)))

    (interface
     ((INTERFACE IDENT LBRACE input-list output-list RBRACE)
      (cons $2 (ag-interface $4 $5))))

    (class
     ((CLASS IDENT COLON IDENT
             LBRACE child-list children-list input-list output-list method-list RBRACE)
      (cons $2 (ag-class $4 $6 $7 $8 $9 $10))))

    (child-list
     ((child child-list) (cons $1 $2))
     (() null))

    (children-list
     ((children children-list) (cons $1 $2))
     (() null))

    (input-list
     ((input input-list) (cons $1 $2))
     (() null))

    (output-list
     ((output output-list) (cons $1 $2))
     (() null))

    (method-list
     ((method method-list) (cons $1 $2))
     (() null))

    (child
     ((CHILD field COLON IDENT SEMICOLON) (cons $2 $4)))

    (children
     ((CHILDREN field COLON IDENT SEMICOLON) (cons $2 $4)))

    (input
     ((INPUT field SEMICOLON) $2))

    (output
     ((OUTPUT field SEMICOLON) $2))

    (method
     ((METHOD SELF DOT IDENT parameter LBRACE read-list write-list RBRACE)
      (cons (list 'self $4) (ag-method $5 $7 $8))))

    (parameter
     ((LPAREN RPAREN) null)
     ((LPAREN IDENT RPAREN) (list $2)))

    (read-list
     ((read read-list) (cons $1 $2))
     (() null))

    (read
     ((READ field SEMICOLON) $2))

    (write-list
     ((write write-list) (cons $1 $2))
     (() null))

    (write
     ((WRITE field SEMICOLON) $2))

    (field
     ((SELF selector) (cons 'self $2)))

    (selector
     ((DOT label selector) (cons $2 $3))
     ((DOT label) (list $2)))

    (label
     ((IDENT) $1)
     ((TRAVERSAL) 'traversal)
     ((CASE) 'case)
     ((ITERATE) 'iterate)
     ((RECUR) 'recur)
     ((VISIT) 'visit)
     ((INTERFACE) 'interface)
     ((CLASS) 'class)
     ((CHILD) 'child)
     ((CHILDREN) 'children)
     ((INPUT) 'input)
     ((OUTPUT) 'output)
     ((READ) 'read)
     ((WRITE) 'write)
     ((METHOD) 'method)))))

; Take an input port and return an abstract syntax tree for the attribute grammar.
(define (parse-grammar port)
  (parse (thunk (lex port))))

(define file->grammar (curry with-input-file parse-grammar))

; -----------------
; AST Serialization
; -----------------

(define/match (serialize-grammar grammar)
  [((ag-grammar iface-list class-list trav-list))
   (string-append
    (string-append* (map serialize-traversal trav-list))
    (string-append* (map serialize-interface iface-list))
    (string-append* (map serialize-class class-list)))])

(define/match (serialize-traversal trav)
  [((cons name case-list))
   (format "\ntraversal ~a {\n~a\n}\n"
           name
           (string-join (map serialize-traversal-case case-list) "\n"))])

(define/match (serialize-traversal-case case)
  [((cons class-name step-list))
    (format "  case ~a {\n~a\n  }"
            class-name
            (string-join (map serialize-traversal-step step-list) "\n"))])

(define/match (serialize-traversal-step step)
  [((ag-visit))
   (format "    visit self;")]
  [((ag-recur node))
   (format "    recur ~a;"
           (path->string node))]
  [((ag-iterate node step-list))
   (format "    iterate ~a {\n~a\n    }"
           (path->string node)
           (string-join (map serialize-traversal-step step-list) "\n"))])

(define/match (serialize-interface iface)
  [((cons name (ag-interface input-list output-list)))
   (format "\ninterface ~a {\n~a\n~a\n}\n"
           name
           (string-join (map serialize-input input-list) "\n")
           (string-join (map serialize-output output-list) "\n"))])

(define (serialize-input field)
  (format "  input ~a;" (path->string field)))

(define (serialize-output field)
  (format "  output ~a;" (path->string field)))

(define/match (serialize-class class-ast)
  [((cons name (ag-class iface child-list children-list input-list output-list method-list)))
   (format "\nclass ~a : ~a {\n~a\n~a\n~a\n~a\n~a\n}\n"
           name
           iface
           (string-join (map serialize-child child-list) "\n")
           (string-join (map serialize-children children-list) "\n")
           (string-join (map serialize-input input-list) "\n")
           (string-join (map serialize-output output-list) "\n")
           (string-join (map serialize-method method-list) "\n"))])

(define/match (serialize-child child-ast)
  [((cons name interface))
   (format "  child ~a : ~a;" (path->string name) interface)])

(define/match (serialize-children child-ast)
  [((cons name interface))
   (format "  children ~a : ~a;" (path->string name) interface)])

(define/match (serialize-method method-ast)
  [((cons name (ag-method params reads writes)))
   (format "  method ~a(~a) {\n~a\n~a\n  }"
           (path->string name)
           (string-join (map symbol->string params) ", ")
           (string-join (map (curry format "    read ~a;") (map path->string reads)) "\n")
           (string-join (map (curry format "    write ~a;") (map path->string writes)) "\n"))])

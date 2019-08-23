#lang rosette

; Parser and Serializer for Language of Tree Traversal Schedules

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "../grammar/syntax.rkt")

(provide parse-schedule
         file->schedule
         schedule->string)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns
  (LBRACE RBRACE LPAREN RPAREN LBRACKET RBRACKET
   DOT SEMICOLON SEQ PAR
   TRAVERSAL CASE ITERATE REVERSE LEFT RIGHT
   RECUR CALL EVAL SKIP HOLE
   SELF
   EOF))

(define-tokens tkns (IDENT))

(define-lex-trans ident
  (syntax-rules ()
    ((_)
     (:: (:or (char-range "a" "z") (char-range "A" "Z") "_" "&") (:* (:or (char-range "a" "z") (char-range "A" "Z") (char-range "0" "9") "_" "-"))))))

(define-lex-trans comment
  (syntax-rules ()
    ((_)
     (:or (:: "//" (:* (char-complement (:or "\r" "\n"))) (:? "\r") "\n")
          (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))))

(define sched-lex
  (lexer
   [(comment) (sched-lex input-port)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["." (token-DOT)]
   [";" (token-SEMICOLON)]
   [";;" (token-SEQ)]
   ["||" (token-PAR)]
   ["traversal" (token-TRAVERSAL)]
   ["case" (token-CASE)]
   ["iterate" (token-ITERATE)]
   ["reverse" (token-REVERSE)]
   ["left" (token-LEFT)]
   ["right" (token-RIGHT)]
   ["recur" (token-RECUR)]
   ["call" (token-CALL)]
   ["eval" (token-EVAL)]
   ["skip" (token-SKIP)]
   ["??" (token-HOLE)]
   ["self" (token-SELF)]
   [(ident) (token-IDENT (string->symbol lexeme))]
   [whitespace (sched-lex input-port)]
   [(eof) (token-EOF)]))

(define sched-parse-lexed
  (parser
   (start composition)
   (tokens tkns e-tkns)
   (end EOF)
   (error (λ (_ token lexeme) (printf "Unexpected token: ~a(~a)~n" token lexeme)))
   (grammar
    (composition
     ((traversal SEQ composition) `(seq ,$1 ,$3))
     ((traversal PAR composition) `(par ,$1 ,$3))
     ((traversal) $1))

    (traversal
     ((TRAVERSAL name LBRACE visitor-list RBRACE) `(trav ,$2 ,$4))
     ((LPAREN composition RPAREN) $2))

    (visitor-list
     ((visitor visitor-list) (cons $1 $2))
     ((visitor) (list $1)))

    (visitor
     ((CASE name LBRACE command-list RBRACE) (cons $2 $4)))

    (command-list
     ((command command-list) (cons $1 $2))
     (() null))

    (command
     ((ITERATE LBRACKET LEFT RBRACKET name LBRACE command-list RBRACE) `(iter-left ,$5 ,$7))
     ((ITERATE LBRACKET RIGHT RBRACKET name LBRACE command-list RBRACE) `(iter-right ,$5 ,$7))
     ((RECUR name SEMICOLON) `(recur ,$2))
     ((HOLE SEMICOLON) `(hole))
     ((SKIP SEMICOLON) `(skip))
     ((EVAL node DOT name SEMICOLON) `(eval ,$2 ,$4))
     ((CALL name SEMICOLON) `(call ,$2)))

    (node
     ((SELF) 'self)
     ((name) $1))

    (name
     ((IDENT) $1)
     ((TRAVERSAL) 'traversal)
     ((CASE) 'case)
     ((ITERATE) 'iterate)
     ((REVERSE) 'reverse)
     ((LEFT) 'left)
     ((RIGHT) 'right)
     ((RECUR) 'recur)
     ((CALL) 'call)
     ((EVAL) 'eval)
     ((SKIP) 'skip)))))

(define (parse-schedule input)
  (sched-parse-lexed (thunk (sched-lex input))))

(define (file->schedule path)
  (call-with-input-file path parse-schedule #:mode 'text))

(define/match (schedule->string sched)
  [((ag:sequential left-sched right-sched))
   (format "~a ;; ~a"
           (schedule->string left-sched)
           (schedule->string right-sched))]
  [((ag:parallel left-sched right-sched))
   (format "(~a || ~a)"
           (schedule->string left-sched)
           (schedule->string right-sched))]
  [((ag:traversal order visitors))
   (format "traversal ~a {\n~a\n}"
           order
           (string-join (map visitor->string visitors) "\n"))])

(define indentation (make-parameter 0))

(define (indent)
  (build-string (* (indentation) 2) (thunk* #\ )))

(define/match (visitor->string visitor)
  [((ag:visitor class commands))
   (parameterize ([indentation (+ (indentation) 1)])
     (define content
       (parameterize ([indentation (+ (indentation) 1)])
         (string-join (map command->string commands) "\n")))
     (format "~acase ~a {\n~a\n~a}"
             (indent)
             (ag:class-name class)
             content
             (indent)))])

(define/match (command->string command)
  [((ag:iter/left child commands))
   (define content
     (parameterize ([indentation (+ (indentation) 1)])
       (string-join (map command->string commands) "\n")))
   (format "~aiterate ~a {\n~a\n~a}"
           (indent) child content (indent))]
  [((ag:iter/right child commands))
   (define content
     (parameterize ([indentation (+ (indentation) 1)])
       (string-join (map command->string commands) "\n")))
   (format "~areverse ~a {\n~a\n~a}"
           (indent) child content (indent))]
  [((ag:recur child))
   (format "~arecur ~a;" (indent) child)]
  [((ag:eval (cons node label)))
   (format "~aeval ~a.~a;" (indent) node label)]
  [((ag:hole)) (format "~a??;" (indent))]
  [((ag:skip)) (format "~askip;" (indent) (indent))]
  [((list commands ...))
   (string-join (map command->string commands) "\n")])

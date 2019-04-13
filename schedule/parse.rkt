#lang rosette

; Parser and Serializer for Language of Tree Traversal Schedules

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "../utility.rkt"
         "syntax.rkt")

(provide parse-schedule
         file->schedule
         serialize-schedule)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns (LPAREN
                             RPAREN
                             LBRACE
                             RBRACE
                             TRAVERSAL
                             CASE
                             ITERATE
                             RECUR
                             SKIP
                             SELF
                             DOT
                             SEMICOLON
                             HOLE
                             SEQ
                             PAR
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
   [";;" (token-SEQ)]
   ["||" (token-PAR)]
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["traversal" (token-TRAVERSAL)]
   ["case" (token-CASE)]
   ["iterate" (token-ITERATE)]
   ["recur" (token-RECUR)]
   ["skip" (token-SKIP)]
   ["self" (token-SELF)]
   ["." (token-DOT)]
   ["??" (token-HOLE)]
   [";" (token-SEMICOLON)]
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
     ((traversal SEQ composition) (sched-sequential $1 $3))
     ((traversal PAR composition) (sched-parallel $1 $3))
     ((traversal) $1))

    (traversal
     ((TRAVERSAL IDENT LBRACE visitor-list RBRACE) (sched-traversal $2 $4))
     ((LPAREN composition RPAREN) $2))

    (visitor-list
     ((visitor visitor-list) (cons $1 $2))
     ((visitor) (list $1)))

    (visitor
     ((CASE IDENT LBRACE slot-list RBRACE) (cons $2 $4)))

    (slot-list
     ((slot slot-list) (cons $1 $2))
     (() null))

    (slot
     ((ITERATE SELF DOT label LBRACE slot-list RBRACE) (sched-iterate (list 'self $4) $6))
     ((RECUR SELF DOT label SEMICOLON) (sched-recur (list 'self $4)))
     ((HOLE SEMICOLON) (sched-slot-hole))
     ((SKIP SEMICOLON) (sched-slot-skip))
     ((SELF DOT label parameter SEMICOLON) (sched-slot-call (list 'self $3) $4)))

    (parameter
     ((LPAREN RPAREN) null)
     ((LPAREN label RPAREN) (list $2)))

    (label
     ((IDENT) $1)
     ((TRAVERSAL) 'traversal)
     ((CASE) 'case)
     ((ITERATE) 'iterate)
     ((RECUR) 'recur)
     ((SKIP) 'skip)))))

(define (parse-schedule input)
  (sched-parse-lexed (thunk (sched-lex input))))

(define file->schedule (curry with-input-file parse-schedule))

(define (parenthesize s)
  (format "(~a)" s))

(define/match (serialize-schedule sched)
  [((sched-sequential left right))
   (format "~a ;; ~a"
           (serialize-schedule left)
           (serialize-schedule right))]
  [((sched-parallel left right))
   (parenthesize
    (format "~a || ~a"
            (serialize-schedule left)
            (serialize-schedule right)))]
  [((sched-traversal order visitors))
   (format "traversal ~a {\n~a\n}"
           order
           (string-join (map sched-serialize-visitor visitors) "\n"))])

(define/match (sched-serialize-visitor visitor)
  [((cons class-name body))
   (format "  case ~a { ~a }"
           class-name
           (string-join (map serialize-slot (flatten body)) " "))])

(define/match (serialize-slot slot)
  [((sched-iterate child slot-list))
   (format "iterate ~a { ~a }"
           (string-join (map symbol->string child) ".")
           (string-join (map serialize-slot slot-list) " "))]
  [((sched-recur child))
   (format "recur ~a;"
           (string-join (map symbol->string child) "."))]
  [((sched-slot-hole)) "??;"]
  [((sched-slot-skip)) "skip;"]
  [((sched-slot-call method param-list))
   (format "~a(~a);"
           (string-join (map symbol->string method) ".")
           (string-join (map symbol->string param-list) ", "))])

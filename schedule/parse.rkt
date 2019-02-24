#lang rosette

; Parser and Serializer for Language of Tree Traversal Schedules

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "syntax.rkt")

(provide sched-parse
         sched-serialize)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns (LPAREN
                             RPAREN
                             LBRACE
                             RBRACE
                             COMMA
                             SKIP
                             HOLE
                             DOT
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
   ["(" (token-LPAREN)]
   [")" (token-RPAREN)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   ["skip" (token-SKIP)]
   ["??" (token-HOLE)]
   [";;" (token-SEQ)]
   ["||" (token-PAR)]
   [(ident) (token-IDENT (string->symbol lexeme))]
   [whitespace (sched-lex input-port)]
   [(eof) (token-EOF)]))

(define sched-parse-lexed
  (parser
   (start comp)
   (tokens tkns e-tkns)
   (end EOF)
   (error (thunk (display "Error: could not parse schedule source")))
   (grammar
    (comp
     ((trav comp-type comp) ($2 $1 $3))
     ((trav) $1))

    (comp-type
     ((SEQ) sched-sequential)
     ((PAR) sched-parallel))

    (trav
     ((IDENT LBRACE visitor-list RBRACE) (sched-traversal $1 $3))
     ((LPAREN comp RPAREN) $2))

    (visitor-list
     ((visitor COMMA visitor-list) (cons $1 $3))
     ((visitor) (list $1)))

    (visitor
     ((IDENT block-list) (cons $1 $2)))

    (block-list
     ((block block-list) (cons $1 $2))
     ((block) (list $1)))

    (block
     ((HOLE) (sched-hole))
     ((LBRACE slot-list RBRACE) $2))

    (slot-list
     ((slot COMMA slot-list) (cons $1 $3))
     ((slot) (list $1))
     (() null))

    (slot
     ((HOLE) (sched-hole))
     ((SKIP) (sched-slot-skip))
     ((IDENT DOT IDENT) (sched-slot-eval $1 $3))))))

(define (sched-parse input)
  (sched-parse-lexed (thunk (sched-lex input))))

(define/match (sched-serialize sched [parenthesize #f])
  [((sched-sequential left right) _)
   (string-append (if parenthesize "(" "")
                  (sched-serialize left #t)
                  ";;"
                  (sched-serialize right)
                  (if parenthesize ")" "")
                  )]
  [((sched-parallel left right) _)
   (string-append (if parenthesize "(" "")
                  (sched-serialize left #t)
                  "||"
                  (sched-serialize right)
                  (if parenthesize ")" ""))]
  [((sched-traversal order visitors) _)
   (string-append (if parenthesize "(" "")
                  (symbol->string order)
                  " {\n"
                  (string-join (map sched-serialize-visitor visitors) ",\n")
                  "\n}"
                  (if parenthesize ")" ""))])

(define/match (sched-serialize-visitor visitor)
  [((cons classname blocks))
   (string-append "    "
                  (symbol->string classname)
                  (string-join (map sched-serialize-block blocks) ""))])

(define (sched-serialize-block block)
  (if (sched-hole? block)
      " ??"
      (string-append " { " (string-join (map sched-serialize-slot block) ", ") " }")))

(define/match (sched-serialize-slot slot)
  [((sched-hole)) "??"]
  [((sched-slot-skip)) "skip"]
  [((sched-slot-eval object label))
   (string-append (symbol->string object) "." (symbol->string label))])

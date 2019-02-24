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
                             HOLE
                             DOT
                             SEQ
                             PAR
                             LOOP
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
   ["??" (token-HOLE)]
   [";;" (token-SEQ)]
   ["||" (token-PAR)]
   ["loop" (token-LOOP)]
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
     ((trav comp-type trav) (sched-comp $2 $1 $3))
     ((trav) $1))

    (comp-type
     ((SEQ) 'seq)
     ((PAR) 'par))

    (trav
     ((trav-type LBRACE visit-list RBRACE) (sched-trav $1 $3))
     ((LPAREN comp RPAREN) $2))

    (trav-type
     ((IDENT) $1))

    (visit-list
     ((visit visit-list) (cons $1 $2))
     ((visit) (list $1)))

    (visit
     ((IDENT LBRACE RBRACE) (cons $1 null))
     ((IDENT LBRACE slot-list RBRACE) (cons $1 $3)))

    (slot-list
     ((slot COMMA slot-list) (cons $1 $3))
     ((slot) (list $1)))

    (slot
     ((LOOP IDENT LBRACE attr-list RBRACE) (cons $2 $4))
     ((attr) $1))

    (attr-list
     ((attr COMMA attr-list) (cons $1 $3))
     ((attr) (list $1)))

    (attr
     ((IDENT DOT IDENT) (cons $1 $3))
     ((HOLE) '??)))))

(define (sched-parse input)
  (sched-parse-lexed (thunk (sched-lex input))))

(define/match (sched-serialize sched [parenthesize #f])
  [((sched-comp 'seq left right) _)
   (string-append (sched-serialize left #t)
                  ";;"
                  (sched-serialize right))]
  [((sched-comp 'par left right) _)
   (string-append (sched-serialize left #t)
                  "||"
                  (sched-serialize right))]
  [((sched-trav order visits) _)
   (string-append (if parenthesize "(" "")
                  (symbol->string order)
                  " {\n"
                  (string-join (map sched-serialize-visit visits) "\n")
                  "\n}"
                  (if parenthesize ")" ""))])

(define/match (sched-serialize-visit visit)
  [((cons classname slots))
   (string-append "  "
                  (symbol->string classname)
                  " {\n    "
                  (string-join (map sched-serialize-slot slots) ",\n    ")
                  "\n  }")])

(define (sched-serialize-slot slot)
  (if (list? (cdr slot))
      (string-append "loop "
                     (symbol->string (car slot))
                     " { "
                     (string-join (map sched-serialize-attr (cdr slot)) ", ")
                     " }")
      (sched-serialize-attr slot)))

(define/match (sched-serialize-attr attr)
  [((cons object label))
   (string-append (symbol->string object)
                  "."
                  (symbol->string label))])

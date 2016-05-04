#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Schedule DSL Parser and Serializer

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc
         "syntax.rkt")

(provide ftl-sched-parse
         ftl-sched-serialize)

; ----------------
; Lexer Definition
; ----------------

(define-empty-tokens e-tkns (PRE
                             POST
                             REC
                             RECUR
                             LBRACKET
                             RBRACKET
                             LBRACE
                             RBRACE
                             COLON
                             COMMA
                             DOT
                             SEQ
                             PAR
                             EOF))

(define-tokens tkns (IDENT))

(define-lex-trans integer
  (syntax-rules ()
    ((_)
     (:: (:? (:or "-" "+"))
         (:+ (char-range "0" "9"))))))

(define-lex-trans ident
  (syntax-rules ()
    ((_)
     (:: (:or (char-range "a" "z") (char-range "A" "Z") "_" "&") (:* (:or (char-range "a" "z") (char-range "A" "Z") (char-range "0" "9") "_" "-"))))))

(define-lex-trans comment
  (syntax-rules ()
    ((_)
     (:or (:: "//" (:* (char-complement (:or "\r" "\n"))) (:? "\r") "\n")
          (:: "/*" (complement (:: any-string "*/" any-string)) "*/")))))

; TODO: use identifiers for keywords?
(define sched-lex
  (lexer
   [(comment) (sched-lex input-port)]
   ["pre" (token-PRE)]
   ["post" (token-POST)]
   ["rec" (token-REC)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["recur" (token-RECUR)]
   [":" (token-COLON)]
   ["," (token-COMMA)]
   ["." (token-DOT)]
   [";;" (token-SEQ)]
   ["||" (token-PAR)]
   [(ident) (token-IDENT (string->symbol lexeme))]
   [whitespace (sched-lex input-port)]
   [(eof) (token-EOF)]))

(define sched-parse
  (parser
   (start trav-comp)
   (tokens tkns e-tkns)
   (end EOF)
   (error (thunk (display "Error: could not parse schedule source")))
   (grammar
    (trav-comp
     ((trav comp trav-comp) ($2 $1 $3))
     ((trav) $1))

    (comp
     ((SEQ) ftl-sched-seq)
     ((PAR) ftl-sched-par))

    (trav
     ((trav-type LBRACE visit-list RBRACE) (ftl-sched-trav $1 $3)))

    (trav-type
     ((PRE) 'pre)
     ((POST) 'post)
     ((REC) 'rec))

    (visit-list
     ((visit visit-list) (cons $1 $2))
     ((visit) (list $1)))

    (visit
     ((IDENT COLON IDENT LBRACE RBRACE) (cons (cons $3 $1) null))
     ((IDENT COLON IDENT LBRACE step-list RBRACE) (cons (cons $3 $1) $5)))

    (step-list
     ((step COMMA step-list) (cons $1 $3))
     ((step) (list $1)))

    (step
     ((LBRACKET attr-list RBRACKET) (ftl-visit-iter $2))
     ((RECUR IDENT) (ftl-visit-recur $2))
     ((attr) (ftl-visit-eval $1)))

    (attr-list
     ((attr COMMA attr-list) (cons $1 $3))
     ((attr) (list $1)))

    (attr
     ((IDENT DOT IDENT) (cons $1 $3))))))

(define (ftl-sched-parse input)
  (sched-parse (λ () (sched-lex input))))

(define/match (ftl-sched-serialize sched)
  [((ftl-sched-seq left right))
   (string-append (ftl-sched-serialize left)
                  ";;"
                  (ftl-sched-serialize right))]
  [((ftl-sched-par left right))
   (string-append (ftl-sched-serialize left)
                  "||"
                  (ftl-sched-serialize right))]
  [((ftl-sched-trav order visits))
   (string-append (symbol->string order)
                  " {\n"
                  (string-join (map ftl-sched-serialize-visit visits) "\n")
                  "\n}")])

(define/match (ftl-sched-serialize-visit visit)
  [((cons (cons iface class) steps))
   (string-append "  "
                  (symbol->string class)
                  ":"
                  (symbol->string iface)
                  " {\n    "
                  (string-join (map ftl-sched-serialize-step steps) ",\n    ")
                  "\n  }")])

(define (ftl-sched-serialize-step step)
  (match step
    [(ftl-visit-iter attributes)
     (string-append "["
                    (string-join (map ftl-sched-serialize-attr
                                      attributes)
                                 ", ")
                    "]")]
    [(ftl-visit-eval attribute)
     (ftl-sched-serialize-attr attribute)]
    [(ftl-visit-recur step)
     (string-append "recur" (symbol->string step))]))

(define/match (ftl-sched-serialize-attr attr)
  [((cons object label))
   (string-append (symbol->string object)
                  "."
                  (symbol->string label))])

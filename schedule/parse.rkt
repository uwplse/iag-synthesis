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
                             ARROW
                             IDENT
                             PERIOD
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

(define sched-lex
  (lexer
   [(comment) (sched-lex input-port)]
   ["pre" (token-PRE)]
   ["post" (token-POST)]
   ["[" (token-LBRACKET)]
   ["]" (token-RBRACKET)]
   ["{" (token-LBRACE)]
   ["}" (token-RBRACE)]
   ["recur" (token-RECUR)]
   ["->" (token-ARROW)]
   ["." (token-PERIOD)]
   [";;" (token-SEQ)]
   ["||" (token-PAR)]
   [(ident) (token-IDENT lexeme)]
   [whitespace (ftl-lex input-port)]
   [(eof) (token-EOF)]))

(define sched-parse
  (parser
   (start decl-list)
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
     ((trav-type LBRACE visit-list RBRACE) (ftl-sched-trav $1
                                                           $3)))

    (trav-type
     ((PRE) 'pre)
     ((POST) 'post))

    (visit
     ((ident ARROW ident LBRACE attr-list RBRACE) (cons (cons $1 $3) $5)))

    (attr-list
     ((attr attr-list) (cons $1 $2))
     ((attr) $1))

    (attr
     ((ident PERIOD ident) (cons $1 $3))))))

(define (ftl-sched-parse input)
  (sched-parse (λ () (sched-lex input))))

(define example-sched "
pre {
HVBox -> HBox { node.attr }
}
")

#lang racket

(require brag/support)

(provide lex)

(module+ test
  (require rackunit))

(define-lex-abbrev numeral
  (:+ numeric))

(define-lex-abbrev decimal
  (:: (:+ numeric) "." (:* numeric)))

(define-lex-abbrev keyword
  (:or "traversal" "interface" "trait" "class"
       "case" "iterate" "reverse" "recur" "eval" "skip"
       "input" "output" "local"
       "foldl" "foldr" "scanl" "scanr"
       "if" "then" "else"
       "max?" "min?"
       "self" "children"))

(define-lex-abbrev delimiter
  (:or "{" "}" "(" ")" "[" "]"))

(define-lex-abbrev operator
  (:or ":" ";" "." "," ".." ":=" "->" "?" "??"
       "!" "&&" "||" "==>"
       "<" "<=" "!=" "==" ">=" ">"
       "+" "-" "*" "/"
       "[i]" "[@]" "[$]" "[i-1]" "[i+1]" "[0]" "[#]"
       "%"))

(define-lex-abbrev identifier
  (:: alphabetic (:* (:or alphabetic numeric "_"))))

(define lex
  (lexer-srcloc
   [whitespace (token 'WHITESPACE #:skip? #t)]
   [(from/to "//" "\n") (token 'COMMENT #:skip? #t)]
   [(from/to "/*" "*/") (token 'COMMENT #:skip? #t)]
   [delimiter (token lexeme lexeme)]
   [keyword (token lexeme lexeme)]
   [operator (token lexeme lexeme)]
   [numeral (token 'NUMERAL (string->number lexeme))]
   [decimal (token 'DECIMAL (string->number lexeme))]
   [(from/to "\"" "\"") (token 'STRING (trim-ends "\"" lexeme "\""))]
   ["true" (token 'BOOLEAN #t)]
   ["false" (token 'BOOLEAN #f)]
   [identifier (token 'IDENTIFIER (string->symbol lexeme))]))

(module+ test
  (define (string->tokens s)
    (map srcloc-token-token (apply-lexer lex s)))

  (check-equal? (string->tokens "")
                null)

  (check-equal? (string->tokens "\n")
                (list (token 'WHITESPACE #:skip? #t)))

  (check-equal? (string->tokens "// I am a comment.\n")
                (list (token 'COMMENT #:skip? #t)))

  (check-equal? (string->tokens "/* I am a comment. */")
                (list (token 'COMMENT #:skip? #t)))

  (check-equal? (string->tokens "1234567890")
                (list (token 'NUMERAL 1234567890)))

  (check-equal? (string->tokens "12345.67890")
                (list (token 'DECIMAL 12345.67890)))

  (check-equal? (string->tokens "\"I am a string.\"")
                (list (token 'STRING "I am a string.")))

  (check-equal? (string->tokens "true")
                (list (token 'BOOLEAN #t)))

  (check-equal? (string->tokens "false")
                (list (token 'BOOLEAN #f)))

  (check-equal? (string->tokens "ident")
                (list (token 'IDENTIFIER 'ident)))

  (check-equal? (string->tokens "traversal")
                (list (token "traversal" "traversal")))

  (check-equal? (string->tokens "interface")
                (list (token "interface" "interface")))

  (check-equal? (string->tokens "trait")
                (list (token "trait" "trait")))

  (check-equal? (string->tokens "class")
                (list (token "class" "class")))

  (check-equal? (string->tokens "input")
                (list (token "input" "input")))

  (check-equal? (string->tokens "output")
                (list (token "output" "output")))

  (check-equal? (string->tokens "local")
                (list (token "local" "local")))

  (check-equal? (string->tokens "foldl")
                (list (token "foldl" "foldl")))

  (check-equal? (string->tokens "foldr")
                (list (token "foldr" "foldr")))

  (check-equal? (string->tokens "scanl")
                (list (token "scanl" "scanl")))

  (check-equal? (string->tokens "scanr")
                (list (token "scanr" "scanr")))

  (check-equal? (string->tokens "self")
                (list (token "self" "self")))

  (check-equal? (string->tokens "children")
                (list (token "children" "children")))

  (check-equal? (string->tokens "if")
                (list (token "if" "if")))

  (check-equal? (string->tokens "then")
                (list (token "then" "then")))

  (check-equal? (string->tokens "else")
                (list (token "else" "else")))

  (check-equal? (string->tokens "min?")
                (list (token "min?" "min?")))

  (check-equal? (string->tokens "max?")
                (list (token "max?" "max?")))

  (check-equal? (string->tokens "{}()[]")
                (list (token "{" "{") (token "}" "}")
                      (token "(" "(") (token ")" ")")
                      (token "[" "[") (token "]" "]")))

  (check-equal? (string->tokens ":;.,")
                (list (token ":" ":") (token ";" ";")
                      (token "." ".") (token "," ",")))

  (check-equal? (string->tokens "..")
                (list (token ".." "..")))

  (check-equal? (string->tokens "->")
                (list (token "->" "->")))

  (check-equal? (string->tokens "?")
                (list (token "?" "?")))

  (check-equal? (string->tokens "??")
                (list (token "??" "??")))

  (check-equal? (string->tokens "!&&||")
                (list (token "!" "!")
                      (token "&&" "&&")
                      (token "||" "||")))

  (check-equal? (string->tokens "+-*/")
                (list (token "+" "+") (token "-" "-")
                      (token "*" "*") (token "/" "/")))

  (check-equal? (string->tokens "[i][@][$][i-1][i+1][0][#]")
                (list (token "[i]" "[i]")
                      (token "[@]" "[@]") (token "[$]" "[$]")
                      (token "[i-1]" "[i-1]") (token "[i+1]" "[i+1]")
                      (token "[0]" "[0]") (token "[#]" "[#]")))

  (check-equal? (string->tokens "%")
                (list (token "%" "%")))

  (check-equal? (string->tokens "<")
                (list (token "<" "<")))

  (check-equal? (string->tokens "<=")
                (list (token "<=" "<=")))

  (check-equal? (string->tokens "!=")
                (list (token "!=" "!=")))

  (check-equal? (string->tokens "==")
                (list (token "==" "==")))

  (check-equal? (string->tokens ">=")
                (list (token ">=" ">=")))

  (check-equal? (string->tokens ">")
                (list (token ">" ">")))

  (check-equal? (string->tokens "12345 . 67890")
                (list (token 'NUMERAL 12345)
                      (token 'WHITESPACE #:skip? #t)
                      (token "." ".")
                      (token 'WHITESPACE #:skip? #t)
                      (token 'NUMERAL 67890)))

  (check-equal? (string->tokens "x y z")
                (list (token 'IDENTIFIER 'x)
                      (token 'WHITESPACE #:skip? #t)
                      (token 'IDENTIFIER 'y)
                      (token 'WHITESPACE #:skip? #t)
                      (token 'IDENTIFIER 'z))))

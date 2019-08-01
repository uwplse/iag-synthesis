#lang rosette

; Print a Rust syntax tree to the current output port.

(provide print-program)

(define indent-level 0)

(define INDENT-UNIT "    ")

(define (indent-line)
  (newline)
  (display (string-append* (make-list indent-level INDENT-UNIT))))

(define (indent++)
  (set! indent-level (+ indent-level 1))
  (indent-line))

(define (indent)
  (indent-line))

(define (indent--)
  (set! indent-level (- indent-level 1))
  (indent-line))

(define (print-each print-item item-list open close #:separator [separator ","] #:indent? [indent? #f])
  (display open)
  (unless (empty? item-list)
    (when indent?
      (indent++))
    (print-item (first item-list))
    (for ([item (rest item-list)])
      (display separator)
      (if indent?
          (indent-line)
          (display " "))
      (print-item item))
    (when indent?
      (indent--)))
  (display close))

;; global ::= `(:: ,name ...) | name
(define/match (print-global type)
  [(`(:: ,mod-list ... ,var))
   (display (string-join (map symbol->string mod-list) "::"))
   (printf "::~a" var)]
  [((? symbol? var))
   (display var)])

;; type ::= `(unit) | ref-type
;; ref-type ::= `(ref ,mut-type) | `(ref ,lifetime ,mut-type)) | gen-type
;; mut-type ::= `(mut ,dyn-type) | dyn-type
;; dyn-type ::= `(dyn ,gen-type) | gen-type
;; gen-type ::= `(gen ,global (,par-type ...)) | global
;; par-type ::= `(life ,name) | type
(define/match (print-type type)
  [(`(unit))
   (display "()")]
  [(`(ref ,type))
   (display "&")
   (print-type type)]
  [(`(ref ,lifetime ,type))
   (display "&")
   (print lifetime)
   (display " ")
   (print-type type)]
  [(`(mut ,type))
   (display "mut ")
   (print-type type)]
  [(`(dyn ,type))
   (display "dyn ")
   (print-type type)]
  [(`(gen ,global ,par-type-list))
   (print-global global)
   (unless (empty? par-type-list)
     (print-each print-type par-type-list "<" ">"))]
  [(`(life ,lifetime))
   (print lifetime)]
  [(global)
   (print-global global)])

;; loc ::= `(select ,expr ,label)
;;       | `(index ,expr ,expr)
;;       | global
(define/match (print-location loc)
  [(`(select ,expr ,label-name))
   (print-expression expr)
   (printf ".~a" label-name)]
  [(`(index ,expr ,index-expr))
   (print-expression expr)
   (display "[")
   (print-expression index-expr)
   (display "]")]
  [(id)
   (print-global id)])

;; field ::= `(: ,name ,expr)
(define/match (print-field field)
  [(`(: ,label ,expr))
   (display label)
   (display ": ")
   (print-expression expr)])

;; expr ::= `(unsafe ,expr)
;;        | `(if ,expr ,expr ,expr)
;;        | `(match ,expr ,match-case ...)
;;        | `(+ ,expr ,expr) | `(* ,expr ,expr) | ...
;;        | `(range ,expr ,expr)
;;        | `(call ,path (,expr ...))
;;        | `(lambda (,name ...) ,body)
;;        | `(ref ,expr)
;;        | `(ref (mut ,expr))
;;        | `(as ,expr ,type)
;;        | `(struct ,global (,field ...))
;;        | int
;;        | str
;;        | loc
(define/match (print-expression expression)
  [(`(unsafe ,expr))
   (display "unsafe { ")
   (print-expression expr)
   (display " }")]
  [(`(if ,cond-expr ,then-expr ,else-expr))
   (display "if ")
   (print-expression cond-expr)
   (display " { ")
   (print-expression then-expr)
   (display " } else { ")
   (print-expression else-expr)
   (display " }")]
  [(`(match ,expr ,match-case-list ...))
   (display "match ")
   (print-expression expr)
   (print-each print-match-case match-case-list " {" "}" #:indent? #t)]
  [(`(,(and operator (or '- '!)) ,expr))
   (display "(")
   (display operator)
   (print-expression expr)
   (display ")")]
  [(`(,(and operator (or '+ '- '* '/ '< '<= '== '!= '>= '> '&& '\|\|)) ,left-expr ,right-expr))
   (display "(")
   (print-expression left-expr)
   (printf " ~a " operator)
   (print-expression right-expr)
   (display ")")]
  [(`(range ,start-expr ,stop-expr))
   (display "(")
   (print-expression start-expr)
   (display "..")
   (print-expression stop-expr)
   (display ")")]
  [(`(call ,fun-expr ,arg-expr-list))
   (print-expression fun-expr)
   (print-each print-expression arg-expr-list "(" ")")]
  [(`(lambda ,name-list ,expr))
   (print-each display name-list "|" "| ")
   (print-expression expr)]
  [(`(mut ,expr))
   (display "mut ")
   (print-expression expr)]
  [(`(ref ,expr))
   (display "&")
   (print-expression expr)]
  [(`(as ,expr ,type))
   (print-expression expr)
   (display " as ")
   (print-type type)]
  [(`(struct ,cons ,field-list))
   (print-global cons)
   (print-each print-field field-list " {" "}" #:indent? #t)]
  [((? integer? int))
   (display int)]
  [((? string? str))
   (printf "\"~a\"" str)]
  [(#t)
   (display "true")]
  [(#f)
   (display "false")]
  [(loc)
   (print-location loc)])

;; field-pattern ::= `(: ,name ,pattern)
;;                 | name
(define/match (print-field-pattern field-pattern)
  [(`(: ,label ,pattern))
   (printf "~a: " label)
   (print-pattern pattern)]
  [((? symbol? name))
   (display name)])

;; content-pattern ::= `(tuple ,pattern ...)
;;                   | `(record ,field-pattern ...)
;;                   | `(unit)
(define/match (print-content-pattern content-pattern)
  [(`(tuple ,pattern-list ...))
   (print-each print-pattern pattern-list "(" ")")]
  [(`(record ,field-pattern-list ...))
   (print-each print-field-pattern field-pattern-list " { " " }")]
  [(`(unit))
   (void)])

;; pattern ::= `(constructor ,global ,content-pattern)
;;           | `(ref ,pattern)
;;           | `(mut ,pattern)
;;           | `(tuple ,pattern ...)
;;           | name
(define/match (print-pattern pattern)
  [(`(constructor ,global ,content-pattern))
   (print-global global)
   (print-content-pattern content-pattern)]
  [(`(ref ,pattern))
   (display "ref ")
   (print-pattern pattern)]
  [(`(mut ,pattern))
   (display "mut ")
   (print-pattern pattern)]
  [(`(tuple ,pattern-list ...))
   (print-each print-pattern pattern-list "(" ")")]
  [(var)
   (display var)])

;; match-case ::= `(=> ,pattern ,body)
(define/match (print-match-case match-case)
  [(`(=> ,pattern ,body))
   (print-pattern pattern)
   (display " =>")
   (print-body body)])

;; body ::= `(do ,stmt ...)
;;        | expr
(define/match (print-body body)
  [(`(skip))
   (display " { }")]
  [(`(do ,statement-list ...))
   (print-each print-statement statement-list " {" "}" #:separator "" #:indent? #t)]
  [(expression)
   (display " ")
   (print-expression expression)])

;; stmt ::= `(if ,expr ,body ,body)
;;        | `(let ,name ,expr)
;;        | `(let-mut ,name ,expr)
;;        | `(:= ,path ,expr)
;;        | `(match ,expr ,match-case ...)
;;        | `(for ,name ,expr ,body)
;;        | `(return ,expr)
;;        | `(skip)
;;        | expr
(define/match (print-statement statement)
  [(`(do ,statement-list ...))
   (print-each print-statement statement-list " {" "}" #:separator "" #:indent? #t)]
  [(`(if ,cond-expr ,then-body ,else-body))
   (display "if ")
   (print-expression cond-expr)
   (print-body then-body)
   (display " else")
   (print-body else-body)]
  [(`(let ,name ,expr))
   (printf "let ~a = " name)
   (print-expression expr)
   (display ";")]
  [(`(let-mut ,name ,expr))
   (printf "let mut ~a = " name)
   (print-expression expr)
   (display ";")]
  [(`(:= ,loc ,expr))
   (print-location loc)
   (display " = ")
   (print-expression expr)
   (display ";")]
  [(`(match ,expr ,match-case-list ...))
   (display "match ")
   (print-expression expr)
   (print-each print-match-case match-case-list " {" "}" #:indent? #t)]
  [(`(for ,pattern ,expr ,body))
   (display "for ")
   (print-pattern pattern)
   (display " in ")
   (print-expression expr)
   (print-body body)]
  [(`(return ,expr))
   (display "return ")
   (print-expression expr)
   (display ";")]
  [(`(skip))
   (void)]
  [(expr)
   (print-expression expr)
   (display ";")])

;; binder ::= `(: ,name ,type)
(define/match (print-binder binder)
  [(`(: ,name ,type))
   (printf "~a: " name)
   (print-type type)])

;; param ::= `(: self Self)
;;         | `(: self (ref Self))
;;         | `(: self (ref (mut Self)))
;;         | binder
(define/match (print-function-parameter parameter)
  [(`(: self Self))
   (display "self")]
  [(`(: self (ref Self)))
   (display "&self")]
  [(`(: self (ref (mut Self))))
   (display "&mut self")]
  [(binder)
   (print-binder binder)])

;; func ::= `(fn ,name (,par-type ...) (,param ...) ,type ,body)
(define/match (print-function function)
  [(`(fn ,name ,par-type-list ,parameter-list ,return-type ,body))
   (printf "pub fn ")
   (print-type `(gen ,name ,par-type-list))
   (print-each print-function-parameter parameter-list "(" ")")
   (display " -> ")
   (print-type return-type)
   (print-body body)])

;; data ::= `(tuple ,type ...)
;;        | `(record ,binder ...)
;;        | `(unit)
(define/match (print-data data)
  [(`(tuple ,type-list ...))
   (print-each print-type type-list "(" ")")]
  [(`(record ,field-list ...))
   (print-each print-binder field-list " {" "}" #:indent? #t)]
  [(`(unit))
   (void)])

;; variant ::= `(variant ,name ,data)
(define/match (print-variant variant)
  [(`(variant ,name ,data))
   (display name)
   (print-data data)])

;; decl ::= `(extern ,name)
;;        | `(use ,name ...)
;;        | `(type ,name ,type)
;;        | `(hash ,name (,name ...))
;;        | `(struct ,name (,par-type ...) ,data)
;;        | `(enum ,name (,par-type ...) ,variant ...)
;;        | `(impl ,name (,par-type ...) ,func ...)
;;        | `(impl (for ,trait ,name) ,func ...)
;;        | func
;;        | `blank
;;        | `(comment ,text)
(define/match (print-declaration declaration)
  [(`(blank))
   (newline)]
  [(`(extern ,package))
   (printf "extern crate ~a;" package)
   (newline)]
  [(`(use ,namespace ... (,identifier ...)))
   (printf "use ~a::{~a};"
           (string-join (map symbol->string namespace) "::")
           (string-join (map symbol->string identifier) ", "))
   (newline)]
  [(`(use ,namespace ...))
   (printf "use ~a;" (string-join (map symbol->string namespace) "::"))
   (newline)]
  [(`(type ,name ,type))
   (printf "type ~a = " name)
   (print-type type)
   (display ";")
   (newline)]
  [(`(hash-bang ,macro ,option-list))
   (printf "#![~a" macro)
   (print-each display option-list "(" ")")
   (display "]")
   (newline)]
  [(`(hash ,macro ,option-list))
   (printf "#[~a" macro)
   (print-each display option-list "(" ")")
   (display "]")
   (newline)]
  [(`(struct ,name ,par-type-list ,data))
   (display "pub struct ")
   (print-type `(gen ,name ,par-type-list))
   (print-data data)
   (unless (eq? (first data) 'record)
    (display ";"))
   (newline)]
  [(`(struct ,lifetime ,name ,data))
   (newline)]
  [(`(enum ,name ,par-type-list ,variant-list ...))
   (display "pub enum ")
   (print-type `(gen ,name ,par-type-list))
   (print-each print-variant variant-list " {" "}" #:indent? #t)
   (newline)]
  [(`(impl (for ,trait ,name) ,function-list ...))
   (printf "impl ~a for ~a" trait name)
   (print-each print-function function-list " {" "}" #:separator "" #:indent? #t)
   (newline)]
  [(`(impl ,name ,par-type-list ,function-list ...))
   (print-type `(gen impl ,par-type-list))
   (display " ")
   (print-type `(gen ,name ,par-type-list))
   (print-each print-function function-list " {" "}" #:separator "" #:indent? #t)
   (newline)]
  [(`(comment ,text))
   (printf "/* ~a */" text)
   (newline)]
  [(function)
   (print-function function)
   (newline)])

;; prog ::= `(,decl ...)
(define (print-program program)
  (for-each print-declaration program))

(define test-binding-statement
  '(let v (call (:: Vec new) ())))

(define test-mutable-binding-statement
  '(let-mut v (call (:: Vec new) ())))

(define test-conditional-statement
  '(if b
       (do (:= (index (select (select self class) children) (- i 1))
               (call make_hvbox ())))
       (do (:= (select (select self class) children)
               (call (:: std Vec new) ())))))

(define test-match-statement
  `(match (select self kind)
     (=> (constructor IsHBox (record (: x x) (: y y) (: z w) (: children _)))
         (do ,test-binding-statement
             ,test-conditional-statement
             (return x)))
     (=> (constructor IsVBox (record (: x x) (: y y) (: z w) (: children _)))
         (do ,test-mutable-binding-statement
             ,test-conditional-statement
             (return y)))
     (=> (constructor IsLeaf (unit))
         (do (return 0)))))

(define test-enum-declaration
  `(enum HVBox
         (constructor IsHBox (record (: x i32) (: y i32) (: z i32) (: children (gen Vec (HVBox)))))
         (constructor IsVBox (record (: x i32) (: y i32) (: z i32) (: children (gen Vec (HVBox)))))
         (constructor IsLeaf (unit))
         (constructor IsAnon (tuple i32 i32 bool (gen Box (HVBox))))))

(define test-function-declaration
  `(fn post ((: self (ref-mut Self)) (: depth u8) (: parallel bool)) (unit) (do ,test-match-statement)))

#lang rosette

; Print a Rust syntax tree to the current output port.

(provide (rename-out [print-program rust:print]))

(define indent-level 0)

(define INDENT-UNIT "    ")

(define (indent-line)
  (newline)
  (display (string-append* (make-list indent-level INDENT-UNIT))))

(define (print-each print-item item-list open close #:separator [separator ","] #:indent? [indent? #f])
  (display open)
  (unless (empty? item-list)
    (when indent?
      (set! indent-level (+ indent-level 1))
      (indent-line))
    (print-item (first item-list))
    (for ([item (rest item-list)])
      (display separator)
      (if indent?
          (indent-line)
          (display " "))
      (print-item item))
    (when indent?
      (set! indent-level (- indent-level 1))
      (indent-line)))
  (display close))

;; global ::= `(:: ,name ...) | name
(define/match (print-global type)
  [(`(:: ,mod-list ... ,var))
   (display (string-join (map symbol->string mod-list) "::"))
   (printf "::~a" var)]
  [((? symbol? var))
   (display var)])

;; type ::= `(unit) | ref-type
;; ref-type ::= `(ref ,gen-type) | `(ref-mut ,gen-type) | gen-type
;; gen-type ::= `(gen ,global (,type ...)) | global
(define/match (print-type type)
  [(`(unit))
   (display "()")]
  [(`(ref ,type))
   (display "&")
   (print-type type)]
  [(`(ref-mut ,type))
   (display "&mut ")
   (print-type type)]
  [(`(gen ,global ,arg-type-list))
   (print-global global)
   (print-each print-type arg-type-list "<" ">")]
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

;; expr ::= `(unsafe ,expr)
;;        | `(if ,expr ,expr ,expr)
;;        | `(+ ,expr ,expr) | `(* ,expr ,expr) | ...
;;        | `(range ,expr ,expr)
;;        | `(call ,path (,expr ...))
;;        | `(lambda (,name ...) ,body)
;;        | `(ref ,expr)
;;        | `(ref-mut ,expr)
;;        | int
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
  [(`(,(and operator (or '+ '- '* '/ '< '<= '== '>= '>)) ,left-expr ,right-expr))
   (display "(")
   (print-expression left-expr)
   (printf " ~a " operator)
   (print-expression right-expr)
   (display ")")]
  [(`(range ,start-expr ,stop-expr))
   (print-expression start-expr)
   (display "..")
   (print-expression stop-expr)]
  [(`(call ,fun-expr ,arg-expr-list))
   (print-expression fun-expr)
   (print-each print-expression arg-expr-list "(" ")")]
  [(`(lambda ,name-list ,body))
   (print-each display name-list "|" "|")
   (print-body body)]
  [(`(ref ,expr))
   (display "& ")
   (print-expression expr)]
  [(`(ref-mut ,expr))
   (display "&mut ")
   (print-expression expr)]
  [((? integer? int))
   (display int)]
  [(loc)
   (print-location loc)])

;; field-pattern ::= `(: ,name ,pattern)
(define/match (print-field-pattern field-pattern)
  [(`(: ,label ,pattern))
   (printf "~a: " label)
   (print-pattern pattern)])

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
;;           | `(ref-mut ,pattern)
;;           | name
(define/match (print-pattern pattern)
  [(`(constructor ,global ,content-pattern))
   (print-global global)
   (print-content-pattern content-pattern)]
  [(`(ref ,pattern))
   (display "ref ")
   (print-pattern pattern)]
  [(`(ref-mut ,pattern))
   (display "ref mut ")
   (print-pattern pattern)]
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
;;        | expr
(define/match (print-statement statement)
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
  [(`(for ,name ,expr ,body))
   (printf "for ~a in " name)
   (print-expression expr)
   (print-body body)]
  [(`(return ,expr))
   (display "return ")
   (print-expression expr)
   (display ";")]
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
;;         | `(: self (ref-mut Self))
;;         | binder
(define/match (print-function-parameter parameter)
  [(`(: self Self))
   (display "self")]
  [(`(: self (ref Self)))
   (display "&self")]
  [(`(: self (ref-mut Self)))
   (display "&mut self")]
  [(binder)
   (print-binder binder)])

;; func ::= `(fn ,name (,param ...) ,type ,body)
(define/match (print-function function)
  [(`(fn ,name ,parameter-list ,return-type ,body))
   (printf "fn ~a" name)
   (print-each print-function-parameter parameter-list "(" ")")
   (display " -> ")
   (print-type return-type)
   (print-body body)])

;; cons ::= `(constructor ,name (tuple ,type ...))
;;        | `(constructor ,name (record ,binder ...))
;;        | `(constructor ,name (unit))
(define/match (print-constructor constructor)
  [(`(constructor ,name (tuple ,type-list ...)))
   (display name)
   (print-each print-type type-list "(" ")")]
  [(`(constructor ,name (record ,field-list ...)))
   (display name)
   (print-each print-binder field-list " {" "}" #:indent? #t)]
  [(`(constructor ,name (unit)))
   (display name)])

;; decl ::= `(extern ,name)
;;        | `(use ,name ...)
;;        | `(type ,name ,type)
;;        | `(struct ,cons)
;;        | `(enum ,name ,cons ...)
;;        | `(impl ,name ,func ...)
;;        | func
;;        | `blank
;;        | `(comment ,text)
(define/match (print-declaration declaration)
  [(`(blank))
   (newline)]
  [(`(extern ,package))
   (printf "extern crate ~a;" package)
   (newline)]
  [(`(use ,namespace ...))
   (printf "use ~a;" (string-join (map symbol->string namespace) "::"))
   (newline)]
  [(`(type ,name ,type))
   (printf "type ~a = " name)
   (print-type type)
   (display ";")
   (newline)]
  [(`(struct ,constructor))
   (printf "struct ")
   (print-constructor constructor)
   (newline)]
  [(`(enum ,name ,constructor-list ...))
   (printf "enum ~a" name)
   (print-each print-constructor constructor-list " {" "}" #:indent? #t)
   (newline)]
  [(`(impl ,name ,function-list ...))
   (printf "impl ~a" name)
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
  
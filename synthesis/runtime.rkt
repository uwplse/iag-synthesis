#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Runtime (library and types)

(require "../utility.rkt")

(provide (struct-out ftl-type)
         (struct-out ftl-runtime)
         ftl-base-runtime)

; FTL has a monomorphic type system with operator overloading

; Note that different instances of these structures ought to inform
; interpretation and compilation. As such, the functions and operations
; may refer to either actual Racket functions or a code fragment that the
; intended code generation backend will understand. For example, this can be
; used to translate the generic addition (+) operator to a string concatenation
; function's name, though it will still be contained in a binary operation AST
; node.

; type descriptor
(struct ftl-type
  (; test whether a literal value is of this type (must be orthogonal w.r.t. all
   ; other types' predicates)
   predicate
   ; generate symbolic value, i.e., oracle for angelic evaluation; may be void
   generate
   ; string -> t
   parse
   ; t*t -> boolean
   equal?
   ; t*t -> boolean
   less?
   ; list associating operator symbols (e.g., -) to unary operations of type
   ; t -> t
   unary
   ; list associating operator symbols (e.g., +, -, *, /)  to binary operations
   ; of type t*t -> t
   binary
   ) #:transparent)

; description of an FTL runtime environment
(struct ftl-runtime
  (; name of boolean type (symbol)
   boolean
   ; list associating typenames (symbols) to type descriptors
   types
   ; list associating function symbols to pairs of types and functions, s.t. the
   ; type a*b->c is '(a b c)
   library
   ) #:transparent)

(define logic-unary
  `((! . ,!)))

(define logic-binary
  `((&& . ,&&)
    (|| . ,||)))

(define arithmetic-unary
  `((- . ,-)
    (+ . ,+)))

(define arithmetic-binary
  `((+ . ,+)
    (- . ,-)
    (* . ,*)
    (/ . ,/)))

(define (string->boolean s)
  (match s
    [(or "true" "#t" "yes") #t]
    [(or "false" "#f" "no") #f]))

; minimal, interpreted runtime environment (basic arithmetic with no external functions)
(define ftl-base-runtime
  (ftl-runtime 'bool
               `((int . ,(ftl-type fixnum?
                                   integer*
                                   string->number
                                   =
                                   <
                                   arithmetic-unary
                                   arithmetic-binary))
                 (float . ,(ftl-type flonum?
                                     number*
                                     string->number
                                     =
                                     <
                                     arithmetic-unary
                                     arithmetic-binary))
                 (bool . ,(ftl-type boolean?
                                    boolean*
                                    string->boolean
                                    =
                                    void
                                    logic-unary
                                    logic-binary))
                 (string . ,(ftl-type string?
                                      void
                                      (λ (x) x)
                                      string=?
                                      string<?
                                      null
                                      `((+ . ,string-append)))))
               `((length . ((string int) . ,string-length)))))

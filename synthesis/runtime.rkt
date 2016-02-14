#lang s-exp rosette

; Functional Tree Language (FTL) synthesis engine
; Runtime (library and types)

(require "../utility.rkt")

(provide (struct-out ftl-type)
         (struct-out ftl-runtime)
         ftl-base-runtime)

(define logic-unary
  (hasheq '! !))

(define logic-binary
  (hasheq '&& &&
          '|| ||))

(define arithmetic-unary
  (hasheq '- -))

(define arithmetic-binary
  (hasheq '+ +
          '- -
          '* *
          '/ /))

; FTL has a monomorphic type system with operator overloading

(struct ftl-type ; type descriptor
  (predicate ; test whether the given value is of this type
   generate ; generate symbolic value, i.e., an oracle; may be void
   equal? ; t*t -> boolean
   less? ; t*t -> boolean
   unary ; hasheq from symbols (e.g., -) to unary operations : t -> t
   binary ; hasheq from symbols (e.g., +, -, *, /)  to binary operations : t*t -> t
   ) #:transparent)

(struct ftl-runtime ; description of an interpreted FTL runtime environment
  (boolean ; name of boolean type (symbol)
   types ; hash(eq) table from types (symbols) to pairs of value predicates and oracles (or void if unsupported)
   library ; hash(eq) table from symbols to pairs of types and functions, s.t. a type a * b -> c is '(a b c)
   ) #:transparent)

(define ftl-base-runtime ; minimal runtime environment (basic arithmetic with no external functions)
  (ftl-runtime 'bool
               (hasheq 'int (ftl-type integer? hole* = < arithmetic-unary arithmetic-binary)
                       'float (ftl-type number? hole* = < arithmetic-unary arithmetic-binary)
                       'bool (ftl-type boolean? decide* = void logic-unary logic-binary)
                       'string (ftl-type string? void string=? string<? (hasheq) (hasheq '+ string-append)))
               (hasheq 'length `((string int) . ,string-length))))

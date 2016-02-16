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

; type descriptor
(struct ftl-type
  (; test whether the given value is of this type
   predicate
   ; generate symbolic value, i.e., an oracle; may be void
   generate
   ; t*t -> boolean
   equal?
   ; t*t -> boolean
   less?
   ; hasheq from symbols (e.g., -) to unary operations : t -> t
   unary
   ; hasheq from symbols (e.g., +, -, *, /)  to binary operations : t*t -> t
   binary
   ) #:transparent)

; description of an interpreted FTL runtime environment
(struct ftl-runtime
  (; name of boolean type (symbol)
   boolean
   ; hash(eq) table from typenames (symbols) to type descriptors
   types
   ; hash(eq) table from symbols to pairs of types and functions, s.t. the type
   ; 'a*b->c' is '(a b c)
   library
   ) #:transparent)

; minimal runtime environment (basic arithmetic with no external functions)
(define ftl-base-runtime
  (ftl-runtime 'bool
               (hasheq 'int (ftl-type integer? hole* = < arithmetic-unary arithmetic-binary)
                       'float (ftl-type number? hole* = < arithmetic-unary arithmetic-binary)
                       'bool (ftl-type boolean? decide* = void logic-unary logic-binary)
                       'string (ftl-type string? void string=? string<? (hasheq) (hasheq '+ string-append)))
               (hasheq 'length `((string int) . ,string-length))))

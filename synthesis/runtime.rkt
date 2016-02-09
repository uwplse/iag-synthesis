#lang s-exp rosette

; Functional Tree Traversal Transformation Language (FT3L) intepreter
; Runtime

(require "../utility.rkt")

(provide (struct-out ftl-runtime)
         ftl-base-runtime)

; an FTL runtime environment descriptor
(struct ftl-runtime
  (boolean ; name of boolean type (symbol)
   types ; hash(eq) table from types (symbols) to pairs of value predicates and oracles (or void if unsupported)
   library ; hash(eq) table from symbols to pairs of types and functions, s.t. a type a * b -> c is '(a b c)
   ) #:transparent)

; FIXME: distinguish operators from functions to allow overloading (perhaps just through an op->fn map?)
; by necessaity, comparison operators require special handling, because they're of type t*t -> 2 rather than t*t -> t
(define ftl-base-runtime
  (ftl-runtime 'bool
               (hasheq 'int (cons integer? hole*) ; (list integer? hole* = < (hasheq '+ + '* *...))
                       'float (cons number? void)
                       'bool (cons boolean? decide*)
                       'string (cons string? void)) ; (list string? void string-append void void void equal?
               (hasheq '+ `((int int int) . ,+)
                       '- `((int int int) . ,-)
                       '* `((int int int) . ,*)
                       '/ `((int int int) . ,/)
                       '== `((int int bool) . ,=)
                       '< `((int int bool) . ,<)
                       '> `((int int bool) . ,>)
                       '<= `((int int bool) . ,<=)
                       '>= `((int int bool) . ,>=)
;                       the following infix operators are not supported by the parser but might/ought to be added
;                       '+. `((float float float) . ,+)
;                       '-. `((float float float) . ,-)
;                       '*. `((float float float) . ,*)
;                       '/. `((float float float) . ,/)
;                       '==. `((float float bool) . ,=)
;                       '<. `((float float bool) . ,<)
;                       '>. `((float float bool) . ,>)
;                       '<=. `((float float bool) . ,<=)
;                       '>=. `((float float bool) . ,>=)
                       'equal `((string string bool) . ,equal?)
                       'concat `((string string string) . ,string-append)
                       'length `((string string string) . ,string-length))))
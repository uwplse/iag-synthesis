#lang s-exp rosette

(provide (struct-out symtab)
         make-symtab
         symtab-keys
         symtab-ref
         symtab-set!
         symtab-map!)

; ---------------------
; Symbol (lookup) table
; ---------------------

(struct symtab (symbols lookup table) #:transparent)

(define (make-symtab symbols default)
  (define (make-lookup symbols index)
    (if (null? symbols)
        (λ (_) -1) ; symbol not found
        (let ([symbol (first symbols)]
              [recurse (make-lookup (rest symbols) (+ index 1))])
          (λ (input)
            (if (eq? input symbol)
                index
                (recurse input))))))
  (let* ([size (length symbols)]
         [lookup (make-lookup symbols 0)]
         [table (make-vector size default)])
    (symtab symbols lookup table)))

(define (symtab-keys symtab)
  (symtab-symbols symtab))

(define (symtab-ref symtab
                    symbol
                    [default (thunk
                              (error "symbol not found: "
                                     (symbol->string symbol)))])
  (let ([index ((symtab-lookup symtab) symbol)])
    (if (eq? index -1)
        (if (procedure? default)
            (default)
            default)
        (vector-ref (symtab-table symtab) index))))

(define (symtab-set! symtab
                     symbol
                     value)
  (let ([index ((symtab-lookup symtab) symbol)]
        [table (symtab-table symtab)])
    (if (eq? index -1)
        (error "symbol not found: "
               (symbol->string symbol))
        (vector-set! table index value))))

(define (symtab-map! symtab
                     function)
  (let ([symbols (symtab-symbols symtab)]
        [lookup (symtab-lookup symtab)]
        [table (symtab-table symtab)])
    (for-each
     (λ (p)
       (let ([i (cdr p)])
         (vector-set! table
                      i
                      (function (car p) (vector-ref table i)))))
     (zip symbols (in-naturals)))))


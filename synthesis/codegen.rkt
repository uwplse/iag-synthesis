#lang racket

(require rosette/lib/meta/meta
         2htdp/batch-io
         parser-tools/lex
         "parse.rkt")

;; Consider the following attribute grammar:
(define example-grammar "
 interface Top {}
   interface Node {
     var a: int ;
   }

   class Root: Top {
     children {
       child: Node;
     }

     actions{
       child.a := x;     
     }
   }

   class MidNode: Node {
     children {
       left:  Node;
       right: Node;
     }
     actions {
       left.a  := a;
       right.a := a;
     }
   }")


;; The above grammar is parsed as: 
(define parsed-example-grammar (parse-ftl (open-input-string example-grammar)))

;; FTL generates a schedule file for the above grammar which is represented in racket as a data structure as shown below
;; (later, this should be read from a sched file, parsed and stored in a racket data structure automatically):
(define example-schedule '([TD (Root child a) (Midnode left a) (Midnode right a)]))

;; Example derivation tree : node name, class, interface, attributes, children
(define example-tree '(root Root Top '() '(
                                           (child Midnode Node '(a  #f) '(
                                                                        (left Midnode Node '(a  #f) '()) (right Midnode Node '(a  #f) '()))))))
;; Functions to access different tree elements
(define node-name car)
(define node-class cadr)
(define node-interface caddr)
(define node-attributes cadddr)
(define node-children last)

(define (set-attribute! n attr-name val)
  (map (lambda (p) (if (equal? attr-name (car p)) (set-box! (cdr p) val) (void))) (cadr(node-attributes n)))
  (void))
(define (get-attribute n attr-name) (unbox (cdr (assoc attr-name (node-attributes n)))))
 
(define codegen-file "generated-code.rkt")

(define code (open-output-file codegen-file #:exists 'truncate))

(display "#lang racket" code)
(newline code) (newline code)

(display "(define node-name car)
(define node-class cadr)
(define node-interface caddr)
(define node-attributes cadddr)
(define node-children last)

(define (set-attribute! n attr-name val)
  (map (lambda (p) (if (equal? attr-name (car p)) (set-box! (cdr p) val) (void))) (cadr(node-attributes n)))
  (void))

(define (get-attribute n attr-name) (unbox (cdr (assoc attr-name (node-attributes n)))))

" code)

(display "(define tree " code)
(display example-tree code)
(display ")"code)

(newline code) (newline code)
;; To generate functions for different traversals by parsing the FTL generated schedule. This schedule is stored in a racket data structure for convenience.
;; At the moment this works for TD and BU.
(define (generate-code code-file schedule tree grammar)
  (define traversal-to-function (map (λ(i)
                                       (display (string-append "(define (" (string-append (string-downcase(symbol->string (car i))) "_assign ")) code)
                                       (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                       (display "_assignment" code)
                                       (display " tree)" code) (newline code)
                                       (display " (unless (null? tree) or (null?(node-attributes tree))" code) (newline code)
                                       (cond
                                         [(string=? (string-downcase(symbol->string (car i))) "td")
                                          (display " (assign_" code)
                                          (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                          (display " " code)
                                          (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                          (display "_assignment tree)" code) (newline code)
                                          (display " (map (lambda (child) (" code)
                                          (display (string-append (string-downcase(symbol->string (car i))) "_assign ") code)
                                          (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                          (display "_assignment child)) (cadr(node-children tree)))))" code) (newline code) (newline code)]                                         
                                         [(string=? (string-downcase(symbol->string (car i))) "bu")
                                          (display "(map (lambda (child) (" code)
                                          (display (string-append (string-downcase(symbol->string (car i))) "_assign ") code)
                                          (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                          (display "_assignment child)) (cadr(node-children tree)))" code)(newline code)
                                          (display "(assign_" code)
                                          (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                          (display " " code)
                                          (display (symbol->string(car(cdr(cdr(cadr i))))) code)
                                          (display "_assignment tree)))" code)])) schedule))
  (newline code)
  (attr-assign-function tree grammar))

;; To generate attribute assignment functions.
;; TODO: handle other types of rhs expressions
;; TODO: handle case where ftl-ast-class has attributes too
;; TODO: handle traits
;; TODO: handle loops
(define (attr-assign-function tree grammar)
  (map (λ(x)(cond
              [(ftl-ast-interface? x)
               (map (λ(i)
                      (display "(define (assign_" code)
                      (display (symbol->string(ftl-ast-declare-name i)) code)
                      (display " " code)
                      (display (symbol->string(ftl-ast-declare-name i)) code)
                      (display " tree)" code) (newline code)
                      (display "(cond " code)
                      (newline code)
                      (map (λ(y)
                             (cond
                               [(ftl-ast-class? y) 
                                (map (λ(j) (cond
                                             [(equal? (ftl-ast-refer-label(ftl-ast-define-lhs j)) (ftl-ast-declare-name i))
                                              (cond
                                                [(ftl-ast-refer? (ftl-ast-define-rhs j))
                                                 (cond
                                                   [(equal? (symbol->string(ftl-ast-refer-object(ftl-ast-define-rhs j))) "self")
                                                     (display "[(equal? (node-class tree) " code)
                                                    (display (ftl-ast-class-name y) code)
                                                    ])
                                                 (display ") " code)
                                                 (display "(map (λ(w)(set-attribute! w " code)
                                                 (display (symbol->string(ftl-ast-declare-name i)) code) (display " " code)
                                                 (cond
                                                   [(not(equal? (ftl-ast-refer-label(ftl-ast-define-rhs j)) (ftl-ast-declare-name i)))
                                                    (display (symbol->string(ftl-ast-refer-label(ftl-ast-define-rhs j))) code)]
                                                   [(equal? (ftl-ast-refer-label(ftl-ast-define-rhs j)) (ftl-ast-declare-name i))
                                                    (display " (get-attribute tree " code)
                                                    (display (symbol->string(ftl-ast-refer-label(ftl-ast-define-rhs j))) code)
                                                    (display ")" code)])
                                                   
                                                 (display ")) (node-children tree))]" code)]
                                                 (newline code))                                                 
                                              (newline code)])) (ftl-ast-body-actions (ftl-ast-class-body y)))])) grammar)
                      (display "))" code)
                      (newline code) (newline code)) (ftl-ast-interface-fields x))])) grammar)) 

(define sample-codegen (generate-code code example-schedule example-tree parsed-example-grammar))

(close-output-port code)
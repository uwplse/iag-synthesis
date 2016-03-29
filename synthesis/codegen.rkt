#lang racket
(require rosette/lib/meta/meta)
(require 2htdp/batch-io)
(require "parse.rkt")

; ------------------
; Example FTL grammar
; ------------------

(define example-grammar "
   interface Top {
     input a: int ;
   }

   interface Node {     
     var a: int ;
     var b: int ;
   }

   class Root: Top {
     children {
       child: Node;
     }

     actions{
       child.a := a;
     }
   }

   class MidNode: Node {
     children {
       child: Node;
     }
     actions {
       child.a := a;
       b := child.b;
     }
   }

   class Leaf: Node {
     actions {
       b := b_input;
     }
   }
")

(define parsed-grammar (parse-ftl (open-input-string example-grammar)))

; Represents derivation trees : each node has a name, class, interface, and zero or more attributes and children
(struct document
  (name
   class
   interface
   attrs
   children) #:mutable #:transparent)

;; Represents the attributes in a node
(struct attribute
  (name
   (value #:mutable)
   ) #:transparent)

(define code-container '())

; pass 1 of schedule gives numbered visitors.
(define (generate-general-visitors schedule)
  (let ([counter 0])
    (for/hash ([sched schedule])
      (begin0
        (values counter (string-append "visit-" (number->string counter)))
      (set! counter (+ 1 counter))))))

; pass 2 of schedule gives concrete node visitors.
(define (generate-node-visitors schedule)
  (let ([counter 0])
    (for/hash ([sched schedule])
      (begin0
        (letrec ([concrete-visitors '()]
                 [traversal (car sched)]
                 [actions (cdr sched)])
          (for/list ([action actions])
            (let ([node-type (car action)])
              (set! concrete-visitors
              (cons (string-append
                     (string-downcase(symbol->string traversal)) "-visit-"
                     (symbol->string node-type) "-"
                     (number->string counter)) concrete-visitors))))
          (values counter concrete-visitors))
        (set! counter (+ 1 counter))))))

; pass 3 of schedule maps traversals to attributes assigned in that traversal such as ((0 . (a)) (1 . (b)))
(define (attribute-traversal-map schedule)
  (let ([counter 0])
    (for/hash ([sched schedule])      
      (begin0
        (letrec ([attributes '()] [actions (cdr sched)])
          (for/list ([action actions])
            (let ([attribute (caddr action)])
              (set! attributes (cons attribute attributes))))   
          (values counter (remove-duplicates attributes)))
      (set! counter (+ 1 counter))))))

; maps a node visitor to the class of the node it visits
(define (visitor-class-map node-visitors)
  (for/hash ([node-visitor node-visitors])
    (begin0
      (let ([class-name (caddr (regexp-split #rx"-" node-visitor))])
        (values node-visitor class-name)))))

; maps concrete node visitors to general visitors
(define (match-concrete-general-visitors schedule)
  (letrec ([general-visitors (generate-general-visitors schedule)]
           [node-visitors (generate-node-visitors schedule)])
    (for ([general-visitor-key (hash-keys general-visitors)])
      (for ([node-visitor-key (hash-keys node-visitors)])
        (if (equal? general-visitor-key node-visitor-key)
            (letrec ([general-visitor (hash-ref general-visitors general-visitor-key)]
                     [concrete-node-visitors (hash-ref node-visitors node-visitor-key)])
              (general-visitor-codegen general-visitor concrete-node-visitors)
              (concrete-visitor-codegen general-visitor concrete-node-visitors)) (void))))))

(define (general-visitor-codegen general-visitor node-visitors)
  (let ([node-visitor-class-list (hash->list (visitor-class-map node-visitors))])
    (switch-case general-visitor node-visitor-class-list))) ;; TODO: problem is that this list is not being expanded. How to fix this?
                                                                         ;; figure out why eval-syntax complains the following
                                                                         ;; node-visitor-class-list: identifier used out of context in: node-visitor-class-list
                                                                         ;; also the value of general-visitor is not used.  
(define-syntax switch-case
 (syntax-rules(document-class node)
   [(switch-case general-visitor visitor-class-pair ...)
    (begin #'(define (general-visitor node)
               (cond [(equal? (cdr visitor-class-pair)(document-class node))
                      ((car visitor-class-pair) node)]...)))]))

(define (concrete-visitor-codegen general-visitor node-visitors)
  (for ([node-visitor node-visitors])
    (cond [(equal? (car (regexp-split #rx"-" node-visitor)) "td")
           (td-codegen general-visitor node-visitor)]
          [(equal? (car (regexp-split #rx"-" node-visitor)) "bu")
           (bu-codegen general-visitor node-visitor)])))

(define (td-codegen general-visitor node-visitor)
  (let ([class-name (caddr (regexp-split #rx"-" node-visitor))])
    (for ([element (attribute-lookup(parse-ftl (open-input-string example-grammar)))])
      (cond [(hash? element)
             (if (equal? class-name (hash-keys element))
                 (let([attributes (hash-values element)])
                   (for ([attribute attributes])
                     (letrec ([rhs-attribute (caar attribute)]
                              [rhs-object (cadar attribute)]
                              [lhs-attribute (caddar attribute)]
                              [lhs-object (cdddar attribute)])
                       (display(td-attribute-assignment general-visitor node-visitor lhs-attribute rhs-attribute)))))
                 (void))])))) 

(define-syntax td-attribute-assignment
  (syntax-rules (attribute-value attribute-name node document-attrs)
    [(td-attribute-assignment general-visitor node-visitor lhs-label rhs-label)
     (begin
       #'(define (general-visitor node)
           (for ([attribute (document-attrs node)])
             (if (equal? (attribute-name attribute) (lhs-label))
                 (set! (attribute-value attribute)(rhs-label))))
           (general-visitor (document-children node))))]))

;; remove later due to repetition, but keeping for now in case other types of traversals have something vastly different.
(define (bu-codegen general-visitor node-visitor)
  (let ([class-name (caddr (regexp-split #rx"-" node-visitor))])
    (for ([element (attribute-lookup(parse-ftl (open-input-string example-grammar)))])
      (cond [(hash? element)
          (if (equal? class-name (hash-keys element))
              (let([attributes (hash-values element)])
                (for ([attribute attributes])
                  (letrec ([rhs-attribute (caar attribute)]
                           [rhs-object (cadar attribute)]
                           [lhs-attribute (caddar attribute)]
                           [lhs-object (cdddar attribute)])
                    (bu-attribute-assignment general-visitor node-visitor lhs-attribute rhs-attribute))))
              (void))]))))

(define-syntax bu-attribute-assignment
  (syntax-rules (attribute-value attribute-name node document-attrs)
    [(bu-attribute-assignment general-visitor node-visitor lhs-label rhs-label)
     (begin
       #'(define (general-visitor node)
           (general-visitor (document-children node))
           (for ([attribute (document-attrs node)])
             (if (equal? (attribute-name attribute) (lhs-label))
                 (set! (attribute-value attribute)(rhs-label))))))]))

(define (attribute-lookup grammar)
  (for/list([grammar-element grammar])
    (begin0
      (if (ftl-ast-class? grammar-element)
          (traverse-actions (ftl-ast-class-name grammar-element) (ftl-ast-body-actions (ftl-ast-class-body grammar-element)))))))

; only unary operators at the moment
(define (traverse-actions class-name actions)
  (let ([assignment-actions '()])
   (for/hash ([action actions])
    (let ([action-list '()])
     (begin0
      (letrec ([lhs-object (ftl-ast-refer-object (ftl-ast-define-lhs action))]
               [lhs-attribute (ftl-ast-refer-label (ftl-ast-define-lhs action))]
               [rhs-object (ftl-ast-refer-object (ftl-ast-define-rhs action))]
               [rhs-attribute (ftl-ast-refer-label (ftl-ast-define-rhs action))])
        (set! action-list (cons lhs-object action-list))
        (set! action-list (cons lhs-attribute action-list))
        (set! action-list (cons rhs-object action-list))
        (set! action-list (cons rhs-attribute action-list))
        (set! assignment-actions (cons action-list assignment-actions))
        (values class-name assignment-actions)))))))

;--------------------
; Example document
;--------------------
(define example-document
  (document 'root 'Root 'Top '(attribute 'a 0)
            '(document 'midnode 'Midnode 'Node '((attribute 'a 0) (attribute 'b 0))
                      '(document 'leaf 'Leaf 'Node '((attribute 'a 0) (attribute 'b 7))
                                '()))))
;---------------------
;Example schedule
;---------------------
;; FTL generates a schedule file for the above grammar which is represented in racket as a data structure as shown below
;; The semantics of every schedule element is: [<traversal> (<class-name> <child-name> <child-attribute>)+]
;; (later, this should be read from a sched file, parsed and stored in a racket data structure automatically):
(define example-schedule '([TD (Root child a) (Midnode child a)]
                           [BU (Leaf () b) (Midnode () b)]))



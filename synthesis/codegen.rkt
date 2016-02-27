#lang racket

(require rosette/lib/meta/meta
         2htdp/batch-io
         parser-tools/lex
         "parse.rkt")

;; Consider the following attribute grammar:
(define example-grammar "
 interface Top {
     var x: int;
   }
   interface Node {
     var a: int ;
   }

   class Root: Top {
     children {
       child: Node;
     }

     actions{
       x := 5;
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

;; Structs for representing derivation trees : each node has a name, class, interface, and zero or more attributes and children
(struct parse_tree
  (name
   parent_name
   class
   interface
   attrs
   children) #:mutable #:transparent)

(struct attribute
  (name
   (value #:mutable)
   ) #:transparent)

;; Example derivation tree
(define example-tree (parse_tree
                      'root
                      'none
                      'Root
                      'Top
                      (list (attribute 'x 5))
                      (list
                       (parse_tree
                        'child
                        'root
                        'Midnode
                        'Node
                        (list (attribute 'a 0))
                        (list
                         (parse_tree
                           'left
                           'child
                           'Midnode
                           'Node
                           (list(attribute 'a 0))
                           '())
                          (parse_tree
                           'right
                           'child
                           'Midnode
                           'Node
                           (list(attribute 'a 0))
                           '()))))))

(define generated-code (list))

(define (set-attr! att val)
  (set-attribute-value! att val))

;; go through the schedule and get the attributes assigned in TD or BU manner. Then call another method to actually do the assignment.
(define (gen-code schedule grammar tree)
 (map (λ(x)
       (cond
         [(equal? (car x) 'TD)
          (let ([node-attr (cdr x)]) (do-td-assignment node-attr grammar tree))] 
         [(equal? (car x) 'BU)
          (let ([node-attr (cdr x)]) (do-bu-assignment node-attr grammar tree))])) schedule) (void))

(define (do-td-assignment attr grammar tree)
 (map (λ(x)
        (cond
          [(and (not (null? tree))(equal? (parse_tree-name tree) (cadr x))) 
           (cond
             [(not (null? (parse_tree-attrs tree)))
              (assign (caddr x) grammar tree)])]
          [(not (equal? (parse_tree-name tree) (cadr x)))
           (map (λ(y) (do-td-assignment attr grammar y)) (parse_tree-children tree))])) attr))

(define (do-bu-assignment attr grammar tree)
  (map (λ(x)
         ((map (λ(y) (do-bu-assignment attr grammar y)) (parse_tree-children tree))
          (cond
            [(and (not (null? tree)) (equal? (parse_tree-name tree) (cadr x)))
             (cond
               [(not (null?(parse_tree-attrs tree)))
                (assign (caddr x) grammar tree)])]))) attr))

(define (assign attr-name grammar node)
  (map (λ(x)
         (cond
           [(ftl-ast-class? x)
            (traverse-actions attr-name (ftl-ast-body-actions (ftl-ast-class-body x)) node)])) grammar))

(define (traverse-actions attr-name class-actions node)
  (map(λ(x)
        (cond
          [(and (and (equal? (parse_tree-name node) (ftl-ast-refer-object (ftl-ast-define-lhs x))) (equal? attr-name (ftl-ast-refer-label (ftl-ast-define-lhs x))))
                (not (equal? (parse_tree-name node) 'self)))
           (cond 
             [(equal? (ftl-ast-refer-object (ftl-ast-define-rhs x)) 'self)
              (map (λ(y)
                     (cond
                       [(equal? (attribute-name y) attr-name)
                        (find-parent-and-assign y example-tree (parse_tree-parent_name node) (ftl-ast-refer-label(ftl-ast-define-rhs x)))])) (parse_tree-attrs node))])])) class-actions))

(define (find-parent-and-assign node_attr entire-tree parent-name assignment-rhs)
  (cond
  [(equal? parent-name (parse_tree-name entire-tree))
   (map(λ(i)
         (cond
           [(equal? (attribute-name i) assignment-rhs)
            (set-attr! node_attr (attribute-value i))
            (display (attribute-value node_attr))])) (parse_tree-attrs entire-tree))]
  [(not (equal? parent-name (parse_tree-name entire-tree)))
   (map (λ(j)
          (unless null? j)
          (find-parent-and-assign node_attr j parent-name assignment-rhs)) (parse_tree-children entire-tree))]))

(define codegen (gen-code example-schedule parsed-example-grammar example-tree))
#lang rosette

(require "../utility.rkt"
         "../grammar/syntax.rkt")

; Build a Rust syntax tree from a completed schedule.

(provide generate-program)

(define (symbol-suffix base suff)
  (string->symbol (string-append (symbol->string base)
                                 (symbol->string suff))))

(define (class-union-type sort)
  (symbol-suffix sort 'Class))

; Generic program header

(define header null)

; Generation of tree data structure

(define/match (generate-child-field child-decl)
  [(`(: ,name (unit ,interface))) `(: ,name (gen Box (,interface)))]
  [(`(: ,name (star ,interface))) `(: ,name (gen Vec (,interface)))]
  [(`(: ,name (plus ,interface))) `(: ,name (gen Vec (,interface)))])

;; (define/match (generate-interface-structure iface-decl)
;;   [(`(,name . ,attributes))
;;    (define fields
;;      (cons `(: class ,(symbol-append name 'Class))
;;            (map generate-label-field attributes)))
;;    `(struct (constructor ,name (record . ,fields)))])

;; (define (generate-interface-enumeration G interface)
;;   (define variants
;;     (for/list ([(name class) (in-dict (ag-grammar-classes G))]
;;                #:when (eq? (ag-class-interface class) interface))

;;       `(constructor ,name (tuple ,name))))
;;   `(enum ,(symbol-append interface 'Class)
;;          .
;;          ,variants))

;; (define/match (generate-class-structure class-decl)
;;   [(`(,name . ,(ag-class _ _ (ag-body children attributes _ _))))
;;    (define fields
;;      (append (map generate-child-field children)
;;              (map generate-label-field attributes)))
;;    `(struct (constructor ,name (record . ,fields)))])

;; (define (generate-data-structure G)
;;   (append
;;    (map generate-interface-structure (ag-grammar-interfaces G))
;;    (map (compose (curry generate-interface-enumeration G) car)
;;         (ag-grammar-interfaces G))
;;    (map generate-class-structure (ag-grammar-classes G))))

; Generation of tree traversal code

;; (define/match (generate-reference reference)
;;   [((ag:reference (ag:object1 'self) label))
;;    `(select self ,label)]
;;   [((ag:reference (ag:object1 child) label))
;;    `(select (select local ,child) ,label)]
;;   [((ag:reference (ag:object$0 child) label))
;;    `(select (index (select local ,child) 0) ,label)]
;;   [((ag:reference (ag:object$$ child) label))
;;    (let ([n `(- (call (select (select local ,child) len) ()) 1)])
;;      `(select (index (select local ,child) ,n) ,label))]
;;   [((ag:reference (ag:object$i child) label))
;;    `(select (index (select local ,child) i) ,label)]
;;   [((ag:reference (ag:object$- child) label))
;;    `(select (index (select local ,child) (- i 1)) ,label)]
;;   [((ag:reference (ag:object$+ child) label))
;;    `(select (index (select local ,child) (+ i 1)) ,label)]
;;   )

(define/match (generate-reference reference)
  [((ag:reference (ag:object1 'self) label))
   `(select self ,label)]
  [((ag:reference (ag:object1 child) label))
   `(select (select self ,child) ,label)]
  [((ag:reference (ag:object$0 child) label))
   `(select (index (select self ,child) 0) ,label)]
  [((ag:reference (ag:object$$ child) label))
   (let ([n `(call (select (select self ,child) len) ())])
     `(if (> ,n 0) (select (index (select self ,child) (- ,n 1)) ,label) 0.0))]
  [((ag:reference (ag:object$i child) label))
   `(select (index (select self ,child) i) ,label)]
  [((ag:reference (ag:object$- child) label))
   `(select (index (select self ,child) (- i 1)) ,label)]
  [((ag:reference (ag:object$+ child) label))
   `(select (index (select self ,child) (+ i 1)) ,label)])

(define/match (generate-expression expression)
  [((ag:reference _ _))
   (generate-reference expression)]
  [(`(ite ,condition ,consequent ,alternate))
   `(if ,(generate-expression condition)
        ,(generate-expression consequent)
        ,(generate-expression alternate))]
  [((list (and operator (or '+ '- '* '/ '< '<= '== '!= '>= '> '&& '\|\| '!)) operands ...))
   (cons operator (map generate-expression operands))]
  [((list function head-argument tail-arguments ...))
   `(call (select ,(generate-expression head-argument) ,function)
          ,(map generate-expression tail-arguments))]
   ;`(call ,function ,(map generate-expression arguments))]
  [((? number?))
   expression]
  [((? boolean?))
   expression])

;; (define/match (generate-command command)
;;   [(`(for+ ,child ,commands))
;;    `(for i (range 1 (call (select (select local ,child) len) ()))
;;          (do . ,(map generate-command commands)))]
;;   [(`(for- ,child ,commands))
;;    `(for i (call (select (range 0 (- (call (select (select local ,child) len) ()))) rev) ())
;;          (do . ,(map generate-command commands)))]
;;   [(`(:= ,ref ,expr))
;;    `(:= ,(generate-reference ref) ,(generate-expression expr))]
;;   [(`(call ,traversal ((unit ,child))))
;;    `(call ,traversal ((select local ,child)))]
;;   [(`(call ,traversal ((curr ,child))))
;;    `(call ,traversal ((index (select local ,child) i)))]
;;   [(`(skip))
;;    `(skip)])

(define/match (generate-command command)
  [(`(when ,child ,commands))
   `(if (> (call (select (select self ,child) len) ()) 0)
        (do . ,(map generate-command commands))
        (skip))]
  [(`(for+ ,child ,commands))
   `(for i (range 1 (call (select (select self ,child) len) ()))
         (do . ,(map generate-command commands)))]
  [(`(for- ,child ,commands))
   (let ([n `(call (select (select self ,child) len) ())])
     `(for i (call (select (range 0 (- ,n 1)) rev) ())
           (do . ,(map generate-command commands))))]
  [(`(:= ,ref ,expr))
   `(:= ,(generate-reference ref) ,(generate-expression expr))]
  [(`(call ,traversal ((unit ,child))))
   `(call (select (select self ,child) ,traversal) ())]
  [(`(call ,traversal ((curr ,child))))
   `(call (select (index (select self ,child) i) ,traversal) ())]
  [(`(call ,traversal ((first ,child))))
   `(call (select (index (select self ,child) 0) ,traversal) ())]
  [(`(call ,traversal ((last ,child))))
   (let ([n `(call (select (select self ,child) len) ())])
     `(call (select (index (select self ,child) (- ,n 1)) ,traversal) ()))]
  [(`(skip))
   `(skip)])

(define (generate-interface-structure name fields)
  `(struct (constructor ,name
                        (record (: box_type BoxType) ; (: class ,(class-union-type name))
                                (: children (gen Vec (,name)))
                                .
                                ,fields))))

(define (generate-interface-enumeration name variants)
  `(enum BoxType ; ,(class-union-type name)
         .
         ,(map (match-lambda
                 [`(of ,kind ,_ ,_) `(constructor ,kind (unit))])
                ;[`(of ,kind ,_ ,_) `(constructor ,kind (tuple ,kind))])
               variants)))

(define (generate-class-structures variants)
  (map (match-lambda
         [`(of ,kind ,children ,fields)
          (let ([child-fields (map generate-child-field children)])
            `(struct (constructor ,kind
                                  (record . ,(append child-fields fields)))))])
       variants))

(define (generate-function name forms)
  (map (match-lambda
         [`(~ ,sort . ,cases)
          `(impl ,sort
                 (fn ,name ((: self (ref (mut Self)))) (unit)
                     (do (match (select self box_type) ; (select self class)
                           .
                           ,(map (match-lambda
                                   [`(=> ,kind ,commands)
                                    `(=> (constructor (:: BoxType ; ,(class-union-type sort)
                                                          ,kind)
                                                      (unit)
                                                      ;(tuple (ref (mut local)))
                                                      )
                                         (do . ,(map generate-command commands)))])
                                 cases)))))])
       forms))

(define (generate-main nest)
  null
  )

(define (generate-program P)
  (append* header
           (map (match-lambda
                  [`(datatype ,name ,fields . ,variants)
                   (list* `(blank)
                          `(blank)
                          (generate-interface-structure name fields)
                          (generate-interface-enumeration name variants)
                          ;`(blank)
                          null ;(generate-class-structures variants)
                          )]
                  [`(function ,name ,forms)
                   (generate-function name forms)
                   ]
                  [`(main ,nest)
                   (generate-main nest)])
                P)))

;; (define (generate-program G schedule)
;;   (append header
;;           (add-between (generate-data-structure G) `(blank))
;;           (list
;;            `(blank)
;;            `(fn reflow ((: tree (ref (mut (dyn Flow))))
;;                         (: layout_context (ref LayoutContext))
;;                         (: relayout_mode RelayoutMode))
;;                 (unit)
;;                 (do . ,(generate-evaluator schedule))))))

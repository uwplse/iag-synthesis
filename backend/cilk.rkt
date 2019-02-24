#lang rosette

; Library for schedule compilation to Cilk

(require "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../tree.rkt"
         "../utility.rkt")

(provide (prefix-out cilk: generate-program))

; TODO: Don't overwhelm Cilk with a flood of tiny tasks; instead, partition the
; tree for a fixed number of threads.

(define port (void))

(define grammar (void))

(define iface-to-classes (void))

(define class-to-iface (void))

(define nodes-by-iface (void))

(define nodes-by-class (void))

(define tree-interface (void))

(define root-interface (void))

(define (initialize grammar0 tree)
  (let ([nodes (tree-gather tree)])
    (set! port (open-output-string))

    (set! grammar grammar0)

    (set! iface-to-classes
          (let* ([classes (ag-grammar-classes grammar)]
                 [classes-by-iface (group-by ag-class-interface classes)])
            (for/hash ([classes-of-iface classes-by-iface])
              (values (ag-class-interface (first classes-of-iface))
                      (map ag-class-name classes-of-iface)))))

    (set! class-to-iface
          (for/hash ([class-ast (ag-grammar-classes grammar)])
            (values (ag-class-name class-ast) (ag-class-interface class-ast))))

    (set! tree-interface (compose (curry hash-ref class-to-iface) tree-class))

    (set! root-interface (tree-interface tree))

    (set! nodes-by-iface (group-by tree-interface nodes))

    (set! nodes-by-class (group-by tree-class nodes))))

(define (generate-program grammar schedule tree)
  (initialize grammar tree)
  (fprintf port "\
#include <stdbool.h>
#include <stdio.h>
#include <sys/time.h>
#include <cilk/cilk.h>
")
  (for ([iface-ast (ag-grammar-interfaces grammar)])
    (generate-interface iface-ast))
  (for ([class-ast (ag-grammar-classes grammar)])
    (generate-class class-ast))
  (generate-pass schedule)
  (fprintf port "
void evaluate(int P) {
    pass0(P);
}

void generate() {
    // FIXME: Implement manually.
}

int main(void) {
    clock_t t;
    int P = __cilk_get_nworkers();
    int N = 1000000;

    generate();

    t = clock();
    evaluate(P);
    t = clock() - t;

    printf(\"Time: %f s\\n\", (double)t / CLOCKS_PER_SEC);
    return 0;
}
"
           )
  (get-output-string port))

(define (generate-interface iface-ast)
  (let* ([iface-name (ag-interface-name iface-ast)]
         [classnames (hash-ref iface-to-classes iface-name)]
         [struct-name (symbol-downcase iface-name)])

    (fprintf port "
static int N_~a = 0;
static struct {
    const int *i_body;
    const enum ~a_class {"
             (symbol-upcase iface-name) struct-name)

    (for ([classname classnames])
      (fprintf port " ~a," (symbol-upcase classname)))
    (fprintf port " } *class;")

    (for ([label-ast (ag-interface-labels iface-ast)])
      (match-let ([(ag-label _ name type) label-ast])
        (fprintf port "
    ~a *~a;"
                 type name)))

    (fprintf port "
} ~a;
"
             struct-name)))

(define (generate-class class-ast)
  (let ([classname (ag-class-name class-ast)]
        [children (ag-class-children class-ast)])

    (fprintf port "
static int N_~a = 0;"
             (symbol-upcase classname))

    (fprintf port "
static struct {"
             )

    (for ([label-ast (ag-class-labels class-ast)])
      (match-let ([(ag-label _ name type) label-ast])
        (fprintf port "
    ~a *~a;"
                 type name)))

    (for ([child-ast children])
      (match-let ([(ag-child name sequence _) child-ast])
        (if sequence
            (fprintf port "
    const int **i_~a;
    const int *n_~a;"
                     name name)
            (fprintf port "
    const int *i_~a;"
                     name))))

    (fprintf port "
} ~a;
"
             (symbol-downcase classname))))


(define (generate-pass schedule [this-pass 0])
  (match schedule
    [(sched-comp composition left-schedule right-schedule)
     (let* ([left-pass (+ this-pass 1)]
            [right-pass (generate-pass left-schedule left-pass)]
            [next-pass (generate-pass right-schedule right-pass)])
       (generate-composition composition this-pass left-pass right-pass)
       next-pass)]
    [(sched-trav order visits)
     (let ([next-pass (+ this-pass 1)])
       (generate-traversal order visits this-pass)
       next-pass)]))

(define (generate-composition composition this-pass left-pass right-pass)
  (define spawn (match composition ['seq ""] ['par "cilk_spawn "]))
  (fprintf port "
void pass~a(int P) {
    ~apass~a(P);
    ~apass~a(P);
}
"
           this-pass spawn left-pass spawn right-pass))

(define (generate-traversal order visits pass)
  (generate-visitors order visits pass)
  (fprintf port "
void pass~a(int P) {"
           pass)
  (match order
    ['un ; looped visitors
     (for ([(iface-name classnames) iface-to-classes])
       (fprintf port "
    int i;
    cilk_for (i = 0; i < N_~a; ++i)
        visit~a_~a(i, 0);"
                (symbol-upcase iface-name) pass (symbol-downcase iface-name)))]
    [(or 'pre 'pre_in 'post 'post_in) ; recursive visitors
     (fprintf port "
    visit~a_~a(0, P);"
              pass (symbol-downcase root-interface))])
  (fprintf port "
}
"))

(define (generate-temporary attribute)
  (let ([object (car attribute)]
        [label (cdr attribute)])
    (fprintf port "
    int ~a_~a;"
             object label)))

(define (possible-accumulators visits)
  (remove-duplicates
   (append* (filter list? (map cdr (append* (map cdr visits)))))))

(define (generate-visitors order visits pass)
  (for ([(iface-name classnames) iface-to-classes])
    (fprintf port "
void visit~a_~a(int self, int P);"
             pass (symbol-downcase iface-name)))

  (let ([preorder (or (equal? order 'pre) (equal? order 'pre_seq))]
        [postorder (or (equal? order 'post) (equal? order 'post_seq))]
        [inorder (or (equal? order 'pre_seq) (equal? order 'post_seq))]
        [unorder (equal? order 'un)])
    (for ([(iface-name classnames) iface-to-classes])
      (fprintf port "
void visit~a_~a(int self, int P) {
    int body = ~a.i_body[self];
    int this;"
               pass (symbol-downcase iface-name) (symbol-downcase iface-name))
      (for-each generate-temporary (possible-accumulators visits))
      (fprintf port "
    switch (~a.class[self]) {"
               (symbol-downcase iface-name))
      (for ([classname classnames])
        (let ([class-ast (get-class grammar classname)]
              [struct-name (symbol-downcase classname)])
          (fprintf port "
    case ~a:"
                   (symbol-upcase classname))

          (when (or preorder unorder)
            (for ([slot (cdr (assoc classname visits))])
              (generate-statement class-ast slot pass
                                  (and preorder inorder) (and postorder inorder))))

          (unless unorder
            (for ([child-ast (ag-class-children class-ast)]
                  #:unless (ag-child-sequence child-ast))
              (let ([child-name (ag-child-name child-ast)]
                    [child-iface (ag-child-interface child-ast)])
                (fprintf port "
        if (P == 1)
             cilk_spawn visit~a_~a(~a.i_~a[body], P - 1);
        else
             visit~a_~a(~a.i_~a[body], P - 1);"
                         pass (symbol-downcase child-iface) struct-name child-name
                         pass (symbol-downcase child-iface) struct-name child-name)))

            (unless inorder
              (for ([child-ast (ag-class-children class-ast)]
                    #:when (ag-child-sequence child-ast))
                (let ([child-name (ag-child-name child-ast)]
                      [child-iface (ag-child-interface child-ast)])
                  (fprintf port "
        if (P == 1) {
            cilk_for (this = 0; this < ~a.n_~a[body]; ++this)
                visit~a_~a(~a.i_~a[body][this], P - 1);
        } else {
            for (this = 0; this < ~a.n_~a[body]; ++this)
                visit~a_~a(~a.i_~a[body][this], P - 1);
        }"
                           struct-name child-name
                           pass (symbol-downcase child-iface) struct-name child-name
                           struct-name child-name
                           pass (symbol-downcase child-iface) struct-name child-name))))

          (when postorder
            (for ([slot (cdr (assoc classname visits))])
              (generate-statement class-ast slot pass
                                  (and preorder inorder) (and postorder inorder))))

            (fprintf port "
        break;"
                     ))))

      (fprintf port "
    }
}
"
               ))))

(define (generate-statement class-ast slot pass seq-pre seq-post)
  (match slot
    ['nop (void)]
    [(cons loop-object (list attributes ...))
     ; Traverse children in sequential post-order (i.e., lockstep with loop)
     (when seq-post
       (let* ([class-struct (symbol-downcase (ag-class-name class-ast))]
              [child-ast (lookup-child (ag-class-children class-ast) loop-object)]
              [child-iface (ag-child-interface child-ast)]
              [child-struct (symbol-downcase child-iface)])
         (fprintf port "
        if (P == 1) {
            cilk_for (this = 0; this < ~a.n_~a[body]; ++this)
                visit~a_~a(~a.i_~a[body][this], P - 1);
        } else {
            for (this = 0; this < ~a.n_~a[body]; ++this)
                visit~a_~a(~a.i_~a[body][this], P - 1);
        }"
                  class-struct loop-object pass child-struct class-struct loop-object
                  class-struct loop-object pass child-struct class-struct loop-object)))

     ; Initialize accumulators
     (for ([attribute attributes])
       (match-let* ([(cons object label) attribute]
                    ; TODO: Use the attribute's actual type.
                    ;[type (ag-label-type (lookup-type class-ast object label))]
                    [type 'int]
                    [rule (lookup-rule (ag-class-rules class-ast) object label)])
         (match (ag-loop-body (ag-rule-right rule))
           [(ag-fold expr _)
            (let ([rhs (generate-expression class-ast expr loop-object)])
              (fprintf port "
        ~a_~a = ~a;"
                       object label rhs))]
           [_ (void)])))

     ; Open loop body
     (fprintf port "
        for (this = 0; this < ~a.n_~a[body]; ++this) {"
              (symbol-downcase (ag-class-name class-ast)) loop-object)

     ; Execute loop body
     (for ([attribute attributes])
       (match-let* ([(cons object label) attribute]
                    [rule (lookup-rule (ag-class-rules class-ast) object label)])
         (match (ag-loop-body (ag-rule-right rule))
           [(or (ag-fold _ expr) expr)
            (let* ([ref (ag-expr-reference object 'current label)]
                   [lhs (generate-expression class-ast ref loop-object)]
                   [rhs (generate-expression class-ast expr loop-object)])
              (fprintf port "
            ~a = ~a;"
                       lhs rhs))])))

     ; Increment accumulators
     (for ([attribute attributes])
       (match-let* ([(cons object label) attribute]
                    ; TODO: Use the attribute's actual type.
                    ;[type (ag-label-type (lookup-type class-ast object label))]
                    [type 'int]
                    [rule (lookup-rule (ag-class-rules class-ast) object label)])
         (when (ag-fold? (ag-loop-body (ag-rule-right rule)))
           (let* ([ref (ag-expr-reference object 'current label)]
                  [rhs (generate-expression class-ast ref loop-object)]
                  )
             (fprintf port "
            ~a_~a = ~a;"
                      object label rhs)))))

     ; Traverse children in sequential pre-order (i.e., lockstep with loop)
     (when seq-pre
       (let* ([class-struct (symbol-downcase (ag-class-name class-ast))]
              [child-ast (lookup-child (ag-class-children class-ast) loop-object)]
              [child-iface (ag-child-interface child-ast)]
              [child-struct (symbol-downcase child-iface)])
         (fprintf port "
        if (P == 1) {
            cilk_for (this = 0; this < ~a.n_~a[body]; ++this)
                visit~a_~a(~a.i_~a[body][this], P - 1);
        } else {
            for (this = 0; this < ~a.n_~a[body]; ++this)
                visit~a_~a(~a.i_~a[body][this], P - 1);
        }"
                  class-struct loop-object pass child-struct class-struct loop-object
                  class-struct loop-object pass child-struct class-struct loop-object)))

     ; Close loop body
     (fprintf port "
        }"
              )]
    [(cons object label)
     (let* ([expr (ag-rule-right (lookup-rule (ag-class-rules class-ast) object label))]
            [ref (ag-expr-reference object #f label)]
            [lhs (generate-expression class-ast ref)]
            [rhs (generate-expression class-ast expr)])
       (fprintf port "
        ~a = ~a;"
                lhs rhs))]))

(define (generate-expression class-ast expression [loop-object #f])
  (define/match (recur expr)
    [((ag-expr-call function arguments))
     (list (symbol->string function)
           "(" (add-between (map recur arguments) ", ") ")")]
    [((ag-expr-unary operator operand))
     (list (symbol->string operator) "(" (recur operand) ")")]
    [((ag-expr-binary left operator right))
     (list "(" (recur left) ") " (symbol->string operator) " (" (recur right) ")")]
    [((ag-expr-condition condition consequent alternate))
     (list "(" (recur condition) ") ? (" (recur consequent) ") : (" (recur alternate) ")")]
    [((ag-expr-reference object index label))
     (generate-reference class-ast object index label loop-object)]
    [((? number?)) (list (number->string expr))]
    [((? boolean?)) (list (if expr "true" "false"))]
    [((? string?)) (list "\"" expr "\"")]) ; FIXME: Escape string literals.)
  (string-append* (flatten (recur expression))))

(define (generate-reference class-ast object index label loop)
  (let* ([class-struct (symbol-downcase (ag-class-name class-ast))]
         [iface-struct (symbol-downcase (ag-class-interface class-ast))]
         [child-ast (lookup-child (ag-class-children class-ast) object)]
         [child-struct (and child-ast (symbol-downcase (ag-child-interface child-ast)))])
    (cond
      [(equal? index 'previous)
       (format "~a_~a" object label)]
      [(and (equal? object 'self) (or (equal? index 'current) (not index)))
       (let ([private? (λ (l) (lookup-label (ag-class-labels class-ast) l))])
         (if (private? label)
             (format "~a.~a[body]" class-struct label)
             (format "~a.~a[self]" iface-struct label)))]
      [(and (equal? object loop) (or (equal? index 'current) (not index)))
       (format "~a.~a[~a.i_~a[body][this]]" child-struct label class-struct object)]
      [(equal? index 'first)
       (format "~a.~a[~a.i_~a[body][0]]" child-struct label class-struct object)]
      [(equal? index 'last)
       (format "~a.~a[~a.i_~a[body][N_~a - 1]]" child-struct label class-struct object
               (symbol-upcase child-struct))]
      [(or (equal? index 'current) (not index))
       (format "~a.~a[~a.i_~a[body]]" child-struct label class-struct object)]
      [else (error "invalid attribute reference")])))

#lang rosette

; Schedule compiler to Rust

(require "../grammar/syntax.rkt"
         "../schedule/syntax.rkt"
         "../tree.rkt"
         "../utility.rkt")

(provide (prefix-out rust: generate-program))

(define port #f)

(define grammar #f)

(define (generate fmt . args)
  (apply (curry fprintf port fmt) args))

; Generate implementation of an elaborated schedule
(define (generate-program port0 grammar0 schedule)
  (set! port port0)
  (set! grammar grammar0)
  (generate "\
extern crate rayon;
extern crate rand;
extern crate time;

use std::boxed::*;
use std::cmp::*;
use rayon::prelude::*;

type int = i32;
type float = f32;

trait Node<I>: Send {
    fn interface(&mut self) -> &mut I;
")
  (for ([_ (sched-flatten schedule)]
        [index (in-naturals 1)])
    (generate "\n    fn traverse~a(&mut self, depth: u32) -> ();\n" index))
  (generate "}\n\n")

  (for ([iface-ast (ag-grammar-interfaces grammar)])
    (generate-interface iface-ast))

  (generate "\n")

  (for ([class-ast (ag-grammar-classes grammar)])
    (generate-class class-ast schedule))

  (generate "
type Tree = ~a;

fn generate(width: u32, height: u32) -> Tree {
    ~a::generate(width, height)
}

fn parallel<L, R>(tree: &mut Tree, f: L, g: R) -> ()
    where L: FnOnce(&mut Tree) + Send,
          R: FnOnce(&mut Tree) + Send {
    unsafe {
        let left = Box::from_raw(tree as *mut Tree);
        let right = Box::from_raw(tree as *mut Tree);
        rayon::join(
            move || f(&mut *Box::into_raw(left)),
            move || g(&mut *Box::into_raw(right))
        );
    }
}

fn layout(t: &mut Tree) -> () {
    "
           (ag-grammar-root grammar) (ag-grammar-root grammar))
  (generate-layout schedule)
  (generate "
}

fn main() -> () {
    rayon::initialize(rayon::Configuration::default().num_threads(6));

    println!(\"Generating tree...\");
    let mut tree = generate(TREE_SPREAD, TREE_HEIGHT);

    println!(\"Beginning layout...\");
    let time = time::precise_time_ns();
    layout(&mut tree);
    let time = time::precise_time_ns() - time;
    println!(\"Time: {}ms\\n\", (time as f64) / 1000000f64);
}

")
  (get-output-string port))

(define (generate-layout schedule [index 1])
  (match schedule
    [(sched-sequential left right)
     (generate "|tree| { ")
     (define left-index (generate-layout left index))
     (generate "(tree); ")
     (define right-index (generate-layout right left-index))
     (generate "(tree) }")
     right-index]
    [(sched-parallel left right)
     (generate "|tree| { parallel(tree, ")
     (define left-index (generate-layout left index))
     (generate ", ")
     (define right-index (generate-layout right left-index))
     (generate ") }")
     right-index]
    [_
     (generate "|tree| { tree.traverse~a(0) }" index)
     (+ index 1)]))

(define (generate-interface iface-ast)
  (generate "struct ~a {\n" (ag-interface-name iface-ast))

  (for ([label-ast (ag-interface-labels iface-ast)])
    (match-let ([(ag-label _ name type) label-ast])
      (generate "    ~a: ~a,\n" name type)))

  (generate "}\n\n")

  ; TODO: Implement random node generation
  )

(define (generate-class class-ast schedule)
  (generate "\
struct ~a {
    public: ~a,
"
           (ag-class-name class-ast) (ag-class-interface class-ast))

  (for ([label-ast (ag-class-labels class-ast)])
    (match-let ([(ag-label _ name type) label-ast])
      (generate "    ~a: ~a,\n" name type)))

  (for ([child-ast (ag-class-children class-ast)])
    (generate
             (if (ag-child-sequence child-ast) "    ~a: Vec<Box<~a>>,\n" "    ~a: Box<~a>,\n")
             (ag-child-name child-ast) (ag-child-interface child-ast)))

  (generate "}\n\n")

  (generate "\
unsafe impl Send for ~a { }

unsafe impl Sync for ~a { }

"
           (ag-class-name class-ast) (ag-class-name class-ast))

  (generate "\
impl Node<~a> for ~a {
    fn interface(&mut self) -> &mut ~a { &mut self.public }
"
           (ag-class-interface class-ast) (ag-class-name class-ast) (ag-class-interface class-ast))

  (for ([traversal (sched-flatten schedule)]
        [index (in-naturals 1)])
    (generate-traversal class-ast traversal index))

  (generate "}\n\n")

  ; TODO: Implement random node generation
  )

(define (generate-traversal class-ast traversal index)
  (let* ([class-name (ag-class-name class-ast)]
         [order (sched-traversal-order traversal)]
         [blocks (sched-traversal-visitors traversal)]
         [template (get-traversal grammar order)]
         [schema (cdr (assoc class-name (ag-traversal-forms template)))])
    (generate "\n    fn traverse~a(&mut self, depth: u32) {\n" index)
    (when (equal? order 'post)
      (generate-recursion class-ast index))
    (for ([statement (cdr (assoc (ag-class-name class-ast) blocks))])
      (generate-statement class-ast statement index (equal? order 'pre) (equal? order 'post)))
    (when (equal? order 'pre)
      (generate-recursion class-ast index))
    (generate "    }\n")))

(define (generate-recursion class-ast index)
  (generate "    if depth < CUTOFF {\n")
  ; FIXME: Use rayon::scope for parallelism across multiple child productions.
  (for ([child-ast (ag-class-children class-ast)])
    (generate "        self.~a" (ag-child-name child-ast))
    (if (ag-child-sequence child-ast)
        (generate ".par_iter_mut().for_each(|child| child.traverse~a(depth + 1));\n" index)
        (generate ".traverse~a(depth + 1);\n" index)))
  (generate "    } else {\n")
  (for ([child-ast (ag-class-children class-ast)])
    (if (ag-child-sequence child-ast)
        (generate ".iter_mut().for_each(|child| child.traverse~a(depth + 1));\n" index)
        (generate ".traverse~a(depth + 1);\n" index)))
  (generate "    }\n"))

(define (generate-statement class-ast slot pass seq-pre seq-post)
  ; FIXME: Blocks
  (match slot
    ['nop (void)]
    [(cons loop-object (list substatements ...))
     ; Initialize accumulators
     (for ([substatement substatements])
       (match-let* ([(cons object label) substatement]
                    [rule (lookup-rule (ag-class-rules class-ast) object label)])
         (match (ag-loop-body (ag-rule-right rule))
           [(ag-fold expr _)
            (let ([rhs (generate-expression class-ast expr loop-object)])
              (generate "\n    let ~a_~a = ~a;" object label rhs))]
           [_ (void)])))

     ; Open loop body
     (generate "
        for child in self.~a.iter_mut() {"
              (symbol-downcase (ag-class-name class-ast)) loop-object)

     ; Execute loop body
     (for ([substatement substatements])
       (match-let* ([(cons object label) substatement]
                    [rule (lookup-rule (ag-class-rules class-ast) object label)])
         (match (ag-loop-body (ag-rule-right rule))
           [(or (ag-fold _ expr) expr)
            (let* ([ref (ag-expr-reference object 'current label)]
                   [lhs (generate-expression class-ast ref loop-object)]
                   [rhs (generate-expression class-ast expr loop-object)])
              (generate "
            ~a = ~a;"
                       lhs rhs))])))

     ; Increment accumulators
     (for ([substatement substatements])
       (match-let* ([(cons object label) substatement]
                    ; TODO: Use the substatement's actual type.
                    ;[type (ag-label-type (lookup-type class-ast object label))]
                    [type 'int]
                    [rule (lookup-rule (ag-class-rules class-ast) object label)])
         (when (ag-fold? (ag-loop-body (ag-rule-right rule)))
           (let* ([ref (ag-expr-reference object 'current label)]
                  [rhs (generate-expression class-ast ref loop-object)]
                  )
             (generate "
            ~a_~a = ~a;"
                      object label rhs)))))

     ; Traverse children in sequential pre-order (i.e., lockstep with loop)
     (when seq-pre
       (let* ([class-struct (symbol-downcase (ag-class-name class-ast))]
              [child-ast (lookup-child (ag-class-children class-ast) loop-object)]
              [child-iface (ag-child-interface child-ast)]
              [child-struct (symbol-downcase child-iface)])
         (generate "
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
     (generate "
        }"
              )]
    [(cons object label)
     (let* ([expr (ag-rule-right (lookup-rule (ag-class-rules class-ast) object label))]
            [ref (ag-expr-reference object #f label)]
            [lhs (generate-expression class-ast ref)]
            [rhs (generate-expression class-ast expr)])
       (generate "
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

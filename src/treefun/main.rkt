#lang racket/base

(require "reader.rkt")

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>

;; (require brag/support
;;          "tokenizer.rkt" "parser.rkt"
;;          "reader.rkt")

(module+ reader
  (provide read-syntax get-info)

  (define (get-info port src-mod src-line src-col src-pos)
    (define (handle-query key default)
      (case key
        #;[(color-lexer)
           (dynamic-require "colorer.rkt" 'color)]
        #;[(drracket:indentation)
           (dynamic-require "indenter.rkt" 'indent)]
        #;[(drracket:toolbar-buttons)
           (dynamic-require "toolbar.rkt" 'button-list)]
        [else
         default]))
    handle-query))

(module+ test
  (check-equal? (read-datum "tests/simple.ag" (open-input-file "tests/simple.ag"))
                '(program
                  (define-traversal (traversal postorder)
                    (visitor (class Node)
                             (iter-left (child kids) (hole))
                             (hole)))
                  (define-interface (interface Tree)
                    (input (attribute v) (type int))
                    (output (attribute x) (type int))
                    (output (attribute y) (type int))
                    (output (attribute z) (type int)))
                  (define-trait (trait Mixin)
                    (fold-right (self)
                                (attribute y)
                                (sel (self) (attribute v))
                                (add (acc (self) (attribute y))
                                     (sel (child kids) (attribute w)))))
                  (define-class (class Node) ((trait Mixin)) (interface Tree)
                    (output (attribute w) (type float))
                    (assign (self) (attribute w)
                            3.1459)
                    (scan-left (child kids) (attribute x)
                               (sel (self) (attribute v))
                               (add (acc (child kids) (attribute x))
                                    (cur (child kids) (attribute v))))))))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
   #:program "my-program"
   #:once-each
   [("-n" "--name") name "Who to say hello to" (set-box! who name)]
   #:args ()
   (printf "hello ~a~n" (unbox who))))

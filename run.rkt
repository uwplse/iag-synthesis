#!/usr/bin/env racket
#lang rosette

; Script to run the synthesizer on a given attribute grammar.

(require racket/cmdline
         racket/pretty
         "src/grammar/parse.rkt"
         "src/grammar/validate.rkt"
         "src/grammar/syntax.rkt"
         "src/grammar/tree.rkt"
         "src/schedule/parse.rkt"
         "src/tracing/synthesizer.rkt"
         "src/backend/generate.rkt"
         "src/backend/printer.rkt")

;(define verbose? (make-parameter #f))
(define *root* (make-parameter 'Root))
(define *output* (make-parameter #f))
(define *examples* (make-parameter null))
(define *sketches* (make-parameter null))

(define (parse-grammar filename)
  (let ([G (file->grammar filename)])
    (validate-grammar G)
    G))

; FIXME: This is a horrid hack and only supports sequential schedule sketches.
(define (parse-schedule-sketch G S0)
  (define traversals
    (reverse
     (map (compose ag:traverse string->symbol)
          (filter non-empty-string?
                  (map string-trim (string-split S0 ";"))))))
  (foldr ag:sequential (first traversals) (rest traversals)))

(command-line
 #:program "synthesize"
 #:once-each
 ;[("-v" "--verbose") "Display verbose intermediate information"
 ;                    (verbose? #t)]
 [("-R" "--root") classname "Root interface of the attribute grammar"
                  (*root* (string->symbol classname))]
 [("-o" "--out") filename "File to output generated Rust implementation"
                 (*output* filename)]
 #:multi
 [("-x" "--example") filename "Example tree in XML format to constrain synthesis"
                     (*examples* (cons filename (*examples*)))]
 [("-s" "--sketch") sketch "Schedule sketch against which to attempt synthesis"
                    (*sketches* (cons sketch (*sketches*)))]
 #:args (grammar-filename)

 (define G (parse-grammar grammar-filename))
 (define E
   (if (empty? (*examples*))
       (tree-examples G (*root*))
       (map (curry file->tree G) (*examples*))))

 (define S*
   (for/or ([sketch (reverse (*sketches*))])
     (printf "Schedule Sketch: ~a~n" sketch)
     (define S (parse-schedule-sketch G sketch))
     (define S* (complete-sketch G S E))
     (newline)
     S*))

 (when S*
   (displayln (schedule->string S*))

   (when (*output*)
     (define P (generate-program G S*))
     (parameterize ([current-output-port (open-output-file (*output*) #:mode 'text #:exists 'replace)])
       (print-program P)))))

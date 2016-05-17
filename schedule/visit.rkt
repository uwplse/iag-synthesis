#lang rosette

; Functional Tree Language (FTL) intepreter
; Scheduled Visits

(require rosette/lib/angelic
         "../utility.rkt"
         "../grammar/intermediate.rkt"
         "../grammar/tree.rkt"
         "../grammar/traversal.rkt"
         "syntax.rkt")

(provide ftl-visit!)

; interpret the given evaluation steps as part of a visit to the given node
(define (ftl-visit! grammar visit-steps bind self)
  (match-let* ([(ftl-tree symbol option _ _) self]
               [(ftl-ir-production _ _ definitions singletons sequences)
                (assoc-lookup (assoc-lookup grammar symbol) option)]
               [steps (assoc-lookup visit-steps (cons symbol option))])

    ; perform each step (i.e., sequenced direct evaluation or lockstep
    ; iteration) of the visit in sequence
    (for/all ([steps steps])
      (for ([step steps])
        (for/all ([step step])
          (unless (void? step)
            (match step
              [(ftl-step-iter substeps)
               ; note that void is propagated to preserve list concreteness
               (ftl-visit-iterate! (for/list ([substep substeps])
                                     (match substep
                                       [(ftl-step-eval attribute)
                                        (cons attribute
                                              (assoc-lookup definitions
                                                            attribute))]
                                       [else (void)]))
                                   bind
                                   self)]
              [(ftl-step-eval attribute)
               (ftl-visit-evaluate! attribute
                                    (assoc-lookup definitions attribute)
                                    bind
                                    self)]
              ; Symbolically evaluation becomes intractable for all but the
              ; smallest trees when recur[sive] steps are symbolically unioned;
              ; given a perfect tree with n uniterated attributes, m iterated
              ; attributes, and w children per inner node, the number of
              ; symbolically explored paths is given by
              ; P(h) = (1+n+(1+m)^2+w*P(h-1))^2 and P(-1) = 0, which is
              ; exponential in h.
              [(ftl-step-recur child-name)
               (let* ([children (assoc-lookup sequences child-name)]
                      [children (if (void? children)
                                    (list (assoc-lookup singletons child-name))
                                    children)]
                      [recurse (curry ftl-visit! grammar visit-steps bind)])
                 (for ([child children])
                   (ftl-tree-inorder recurse child)))])))))))

; perform an attribute evaluation
(define (ftl-visit-evaluate! attribute rule bind self)
  (match-let* ([(cons object label) attribute]
               [(ftl-ir-definition (? void?)
                                   (ftl-ir-evaluation fun deps)) rule]
               [load (curry ftl-tree-load self (void) null null)]
               [value (fun (vector-map load deps))])
    (bind self object label value)))

; perform one or more iterated attribute evaluations in lockstep
(define (ftl-visit-iterate! actions bind self)
  (define iterated ; this better be concrete!
    (ftl-ir-definition-iterate (cdr (findf (compose not void?) actions))))

  ; let's just double-check that these actions are actually meant to iteratex
  ; over the same child sequence
  (for ([action actions])
    (assert (eq? iterated (ftl-ir-definition-iterate (cdr action)))))

  ; compute the iterated evaluation in lockstep
  (let* ([children (assoc-lookup (ftl-tree-children self) iterated)]
         [name-rule-map (for/list ([action actions])
                          (match action
                            [(cons name (ftl-ir-definition _ rule))
                             (cons name rule)]
                            [else (void)]))])

    ; collect initial values into an association list
    (define initial
      (for/fold ([current null])
                ([name-rule name-rule-map])
        (unless (void? name-rule)
          (match-let ([(cons name rule) name-rule]
                      [load (curry ftl-tree-load self (void) null null)])
            (match rule
              [(ftl-ir-evaluation _ _)
               ; no initial accumulator for straight evaluations
               current]
              [(ftl-ir-reduction (ftl-ir-evaluation fun deps) _)
               ; compute the initial evaluation for each reduction
               (cons (cons name (fun (vector-map load deps)))
                     current)])))))

    ; iterate over the child sequence to compute the evaluations and acccumulate
    ; the final values into an association list
    (define final
      (for/fold ([previous initial])
                ([child children])
        (for/fold ([current null])
                  ([name-rule name-rule-map])
          (unless (void? name-rule)
            (match-let* ([(cons name rule) name-rule]
                         [(cons object label) name]
                         [load (curry ftl-tree-load self child previous current)])
              (match rule
                [(ftl-ir-evaluation fun deps)
                 ; straight evaluation, assigning to an attribute on
                 ; either some singleton child or the iterated child
                 ; sequence
                 (let ([value (fun (vector-map load deps))])
                   (if (eq? object iterated)
                       (bind child 'self label value)
                       (bind self object label value))
                   ; pass through the current accumulator unchanged
                   current)]
                [(ftl-ir-reduction _ (ftl-ir-evaluation fun deps))
                 ; reduction, so perform the step evaluation, assign to
                 ; iterated child sequence if necessary, and pass
                 ; through new accumulator value
                 (let ([value (fun (vector-map load deps))])
                   (when (eq? object iterated)
                     (bind child 'self label value))
                   ; pass through the current accumulator unchanged
                   (cons (cons name (fun (vector-map load deps)))
                         current))]))))))

    ; lastly, bind the final value for each accumulated value to its attribute
    ; destination if on a singleton child
    (for/all ([final final])
      (for ([result final])
        (match-let ([(cons (cons object label) value) result])
          (unless (eq? object iterated)
            (bind self object label value)))))))

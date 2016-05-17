#lang rosette

; Functional Tree Language (FTL) synthesis engine
; Intermediate Representation

(provide (struct-out ftl-ir-dependency)
         (struct-out ftl-ir-evaluation)
         (struct-out ftl-ir-reduction)
         (struct-out ftl-ir-definition)
         (struct-out ftl-ir-production))

; -----------------------------------
; Grammar Intermediate Representation
; ------------------------------------

; Attribute dependencies with 'previous indices (e.g., object$-.alpha) valid if
; and only if the referenced attribute is defined by a fold in the same loop and
; However, this condition is not currently currently checked and must be dealt
; with at interpretation time. The only check performed is whether an indexed
; attribute dependency happens in a proper loop context and is not recursive
; (except for 'previous, of course).

; attribute definition dependency
(struct ftl-ir-dependency
  (; singleton child name, sequence child name, or 'self
   object
   ; 'first, 'previous, 'current, 'last, or 'none
   index
   ; name of attribute on object
   label
   ) #:transparent)

; attribute evaluation
(struct ftl-ir-evaluation
  (; accepts dependency values packed in an argument vector
   function
   ; vector of IR dependencies
   dependencies ; TODO: maybe rename parameters? or inputs?
   ) #:transparent)

; attribute definition for a reduction (fold)
(struct ftl-ir-reduction
  (; IR evaluation for initial accumulator value
   init
   ; IR evaluation for next accumulator value
   step
   ) #:transparent)

; attribute definition
(struct ftl-ir-definition
  (; name of sequence child to iterate/reduce over or (void) if not applicable
   iterate
   ; IR evaluation or reduction to compute attribute
   evaluate
   ) #:transparent)

; production descriptor
(struct ftl-ir-production
  (; list of labels that must be supplied by the input derivation
   inputs
   ; list associating each label to its type (as a symbol)
   labels
   ; list associating each object-label pair to its IR definition; an individual
   ; association (pair) is sometimes referred to as an action
   definitions
   ; list associating each singleton child name with its symbol [in the implicit
   ; alphabet]
   singletons
   ; list associating each sequence child name with its symbol [in the implicit
   ; alphabet]
   sequences
   ) #:transparent)

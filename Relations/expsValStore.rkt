#lang racket
; Expressions that interact with the values' store

(require redex
         "../grammar.rkt"
         "../Meta-functions/valStoreMetafunctions.rkt"
         )


(define exps-val-store
  (reduction-relation
   core-lang
   #:domain (σ : e)
   ; Implicit dereferencing
   ;
   ; Note that we don't need to mention explicitly the context where this
   ; operation occurs, as the evaluation contexts never allow reduction on
   ; l-values, if they are not fields of tables.
   ; As an example of a situation where evaluation contexts are needed
   ; in order to describe correctly the meaning of a contraction, see
   ; E-ConvertVoidToNilWhereTruncate.
   [--> (σ : r)
        (σ : (derefSigma σ r))
        E-RefDeref]
   ))
(provide exps-val-store)

#lang racket
; Statements that interact with the values' store

(require redex
         "../grammar.rkt"
         "../Meta-functions/substitution.rkt"
         "../Meta-functions/valStoreMetafunctions.rkt")

(define stats-val-store
  (reduction-relation
   core-lang
   #:domain (σ : s)

   ; State change
   [--> (σ : ((r_1) = (v_1)))
        ((sigmaAlter σ r_1 v_1) : \;)
        E-RefMapChange

        (side-condition
         (term (refsBelongsTo (r_1) σ)))]
   
   ; Local variables
   [--> (σ : (local (Name ...) = vlist in s end))
        (σ_2 : (substBlock s any))
        E-Local
        
        (side-condition
         (= (length (term (Name ...))) 
            (length (term vlist))))

        (where (σ_2 (r ...)) (addSimpVal σ vlist))
        (where any (createSubstMap (Name ...) (r ...)))]
   ))
(provide stats-val-store)

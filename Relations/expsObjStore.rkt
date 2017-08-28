#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         "../Meta-functions/tablesMetafunctions.rkt")

; Expressions that interact with the objects' store

(define exps-obj-store
  (reduction-relation
   core-lang
   #:domain (θ : e)
   ; Function creation
   [--> (θ_1 : (function Name parameters s end))
        (θ_2 : objref)

        E-StoreClosure
        
        (side-condition (not (term (functionIsStored? 
                                    θ_1 
                                    (function Name parameters s end)))))
        
        (where (θ_2 objref) (addObject θ_1 (function Name parameters s end)))]
   
   [--> (θ : (function Name parameters s end))
        (θ : objref)

        E-ReuseClosure
        
        (where objref (functionIsStored? θ (function Name parameters s end)))]
   
   ; Table creation
   [--> (θ_1 : evaluatedtable)
        (θ_2 : objref)

        E-CreateTable
        
        (where (θ_2 objref) (addObject θ_1 (addKeys evaluatedtable)))]
   
   ; Table indexing
   [--> (θ : (objref \[ v_1 \]))
        (θ : v_2)

        E-IndexTable
        
        ; objref points to a table
        (side-condition (eq? (term (δ (type objref θ))) 
                             (term "table")))

        (where v_2 (δ (rawget objref v_1 θ)))

        (side-condition (not (equal? (term v_2)
                                     (term nil))))]
   
   ; Abnormal situations
   [--> (θ : (objref \[ v \]))
        (θ : ((objref \[ v \])KeyNotFound))
        
        E-AlertKeyNotFound
        ; Determine if we are trying to index a table value
        (side-condition (eq? (term (δ (type objref θ))) (term "table")))


        (where nil (δ (rawget objref v θ)))]
   
   [--> (θ : (v_1 \[ v_2 \]))
        (θ : ((v_1 \[ v_2 \])NonTableIndexed))
        
        E-AlertNonTableIndexed
        
        ; v_1 is not a reference to a table
        (side-condition (not (eq? (term (δ (type v_1 θ)))
                                  (term "table"))))]

   
   ; Coercion in string operations
;   [--> (θ : (v_1 .. v_2))
;        (θ : (any_1 .. any_2))
;        E-StringConcatCoercion
;        
;        (side-condition (or (equal? (term (δ (type v_1 θ)))
;                                    "number")
;                            
;                            (equal? (term (δ (type v_2 θ)))
;                                    "number")))
;
;        (where any_1 (δ (tostring v_1 θ)))
;        (where any_2 (δ (tostring v_2 θ)))]
   ))

(provide exps-obj-store)

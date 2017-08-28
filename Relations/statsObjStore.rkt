#lang racket
; Statements that interact with the objects' store

(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         "../Meta-functions/tablesMetafunctions.rkt")

(define stats-obj-store
  (reduction-relation
   core-lang
   #:domain (θ : s)
   
   ; Table assignment
   [--> (θ_1 : (((objref \[ v_1 \])) = (v_2)))
        (θ_2 : (< any >))
        E-AssignTable
        
        ; objref points to a table
        (side-condition (eq? (term (δ (type objref θ_1)))
                             (term "table")))
        
        ; This way of determining if v_1 is a key of objref, is mentioned in
        ; Lua's reference manual.
        (where v_3 (δ (rawget objref v_1 θ_1)))
        
        ; The key belongs to the table
        (side-condition (not (equal? (term v_3)
                                     (term nil))))
        
        (where (θ_2 any) (δ (rawset objref v_1 v_2 θ_1)))]
   
   
   [--> (θ : (((objref \[ v_1 \])) = (v_2)))
        (θ : ((((objref \[ v_1 \])) = (v_2))FieldAssignWrongKey))
        
        E-AlertFieldAssignWrongKey
        ; Determine if objref points to a table
        (side-condition (eq? (term (δ (type objref θ)))
                             (term "table")))

        ; The table doesn't have v_1 as key
        (where nil (δ (rawget objref v_1 θ)))]
   
   [--> (θ : (((v_1 \[ v_2 \])) = (v_3)))
        (θ : ((((v_1 \[ v_2 \])) = (v_3))FieldAssignOverNonTable))
        
        E-AlertFieldAssignOverNonTable
        ; Determine if simplevalue is not an reference pointing to a table
        (side-condition (not (eq? (term (δ (type v_1 θ)))
                                  (term "table"))))]
   ))

(provide stats-obj-store)

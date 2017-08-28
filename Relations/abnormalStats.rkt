#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/tablesMetafunctions.rkt"
         "../Meta-functions/delta.rkt")

; Statements managed by the meta-tables mechanism

(define abnormal-stats
  (reduction-relation
   core-lang
   #:domain (θ : s)
   
   ; Abnormal statements involving table field assignment
   [--> (θ : ((((v_1 \[ v_2 \])) = (v_3))specCondLabel))
        (θ : (v_4 (v_1 v_2 v_3)))
        
        E-FieldAssignWrongKeyNormal
        ; Determine if the table efectively has a meta-table
        (where v_4 (indexMetaTable v_1 (misceventkey specCondLabel) θ))

        (side-condition (eq? (term (δ (type v_4 θ)))
                             (term "function")))]
   
   [--> (θ : ((((v_1 \[ v_2 \])) = (v_3))specCondLabel))
        (θ : (((v_4 \[ v_2 \])) = (v_3)))
        
        E-FieldAssignWrongKeyRepeat
        ; Determine if the table efectively has a meta-table
        (where v_4 (indexMetaTable v_1 (misceventkey specCondLabel) θ))
        ; Determine if in that field we don't have a reference to a function...
        (side-condition (and (not (eq? (term v_4)
                                       (term nil)))

                             (not (eq? (term (δ (type v_4 θ)))
                                       (term "function")))))]
   
   [--> (θ_1 : ((((objref \[ v_1 \])) = (v_2))FieldAssignWrongKey))
        (θ_2 : (< any_2 >))

        E-FieldAssignWrongKeyNoHandler
        ; Determine if the table has no meta-table or its meta-table
        ; doesn't has "__newindex" as a key
        (where any_1 (indexMetaTable objref "__newindex" θ_1))
        (side-condition (eq? (term any_1)
                             (term nil)))

        (where (θ_2 any_2) (δ (rawset objref v_1 v_2 θ_1)))]
   
   [--> (θ : ((((v_1 \[ v_2 \])) = (v_3))FieldAssignOverNonTable))
        (θ : ($builtIn error (String)))

        E-FieldAssignOverNonTableNoHandler

        ; Determine if simplevalue efectively has a meta-table
        (where nil (indexMetaTable v_1 "__newindex" θ))

        (where String ,(string-append "attempt to index a "
                                      (term (δ (type v_1 θ)))
                                      " value"))]
   ))

(provide abnormal-stats)

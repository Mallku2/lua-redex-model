#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         "../Meta-functions/tablesMetafunctions.rkt")

; Expressions handled by the meta-table mechanism.

(define abnormal-exps
  (reduction-relation
   core-lang
   #:domain (θ : e)
   
   ; Call over a non-function value
   
   [--> (θ : ((v_1 (v_2 ...))WrongFunCall))
        (θ : (any  (v_1 v_2 ...)))
        
        E-WrongFunCallWithHandler
        ; Determine if sv has a meta-table
        (where any (indexMetaTable v_1 
                                   (misceventkey WrongFunCall) 
                                   θ))
        
        (side-condition (and (not (eq? (term any)
                                       (term nil)))

                             (not (eq? (term any)
                                       (term false)))))]
   
   [--> (θ : ((v_1 (v_2 ...))WrongFunCall))
        (θ : ($builtIn error (String)))
        
        E-WrongFunCallNoHandler
        ; Determine if v_1 has a meta-table
        (where v_3 (indexMetaTable v_1 
                                   (misceventkey WrongFunCall) 
                                   θ))
        

        (side-condition (or (eq? (term v_3) (term nil))
                            (eq? (term v_3) (term false))))
        
        (where String (errmessage WrongFunCall (δ (type v_1 θ))))]

   [--> (θ : ((v_1 \[ v_2 \])specCondLabel))
        (θ : (v_3 (v_1 v_2)))
        
        E-KeyNotFoundWithHandlerNormal
        ; Determine if the table efectively has a meta-table

        (where v_3 (indexMetaTable v_1 
                                   (misceventkey specCondLabel) 
                                   θ))
        
        (side-condition (equal? (term (δ (type v_3 θ))) 
                                "function"))
        ]
   
   [--> (θ : ((v_1 \[ v_2 \])specCondLabel))
        (θ : (v_3 \[ v_2 \]))
        
        E-KeyNotFoundWithHandlerRepeat
        ; Determine if the table efectively has a meta-table
        (where v_3 (indexMetaTable v_1 
                                   (misceventkey specCondLabel) 
                                   θ))
        
        (side-condition (and (not (eq? (term v_3) 
                                  (term nil)))
                             
                             (not (eq? (term (δ (type v_3 θ))) 
                                       "function"))))]
   
   [--> (θ : ((objref \[ v \])KeyNotFound))
        (θ : nil)
        
        E-KeyNotFoundNoHandler
        ; Determine if the table doesn't have a meta-table or
        ; its meta-table doesn't have a field with key "__index"
        (where nil (indexMetaTable objref 
                                   (misceventkey KeyNotFound) 
                                   θ))]
   
   [--> (θ : ((v_1 \[ v_2 \])NonTableIndexed))
        (θ : ($builtIn error (String)))
        
        E-NonTableIndexedNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where nil (indexMetaTable v_1 
                                   (misceventkey NonTableIndexed) 
                                   θ))
        
        (where String (errmessage NonTableIndexed 
                                  (δ (type v_1 θ))))]

   [--> (θ : ((v_1 binop v_2)specCondLabel))
        (θ : (v_3 (v_1 v_2)))
        
        E-ArithOpWrongOperandsWithHandler
        
        (side-condition (or (redex-match core-lang
                                          arithop
                                          (term binop))
                             
                             (redex-match core-lang
                                          ..
                                          (term binop))))
        
        ; Determine if simplevalue efectively has a meta-table
        (where v_3 (getBinHandler v_1
                                  v_2
                                  (binopeventkey binop)
                                  θ))
        
        (side-condition (and (not (equal? (term v_3)
                                         (term nil)))

                            (not (equal? (term v_3)
                                         (term false)))))]
   
   [--> (θ : ((v_1 binop v_2)specCondLabel))
        (θ : ($builtIn error (String)))
        
        E-ArithWrongOperandsNoHandler

        (side-condition (or (redex-match core-lang
                                          arithop
                                          (term binop))
                             
                             (redex-match core-lang
                                          ..
                                          (term binop))))
        
        ; Determine if sv_1 or sv_2 efectively has a meta-table
        (where v_3 (getBinHandler v_1 v_2 (binopeventkey binop) θ))

        (side-condition (or (equal? (term v_3)
                                    (term nil))

                            (equal? (term v_3)
                                    (term false))))
        
        (where String (errmessage specCondLabel 
                                  (δ (type v_1 θ)) 
                                  (δ (type v_2 θ))))]
   
   [--> (θ : ((- v_1)NegWrongOp))
        (θ : (v_2 (v_1)))
        
        E-NegationWrongOperandWithHandler
        ; Determine if v_1 efectively has a meta-table
        (where v_2 (getUnaryHandler v_1 
                                    (unopeventkey -) 
                                    θ))
        
        (side-condition (and (not (equal? (term v_2)
                                         (term nil)))
                             
                             (not (equal? (term v_2)
                                         (term false)))))]
   
   [--> (θ : ((- v_1)NegWrongOp))
        (θ : ($builtIn error (String)))
        
        E-NegationWrongOperandNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where v_2 (getUnaryHandler v_1
                                    (unopeventkey -)
                                    θ))

        (side-condition (or (equal? (term v_2)
                                    (term nil))
                             
                             (equal? (term v_2)
                                     (term false))))
        
        (where String (errmessage NegWrongOp (δ (type v_1 θ))))]
   
    
   [--> (θ : ((\# v_1)StrLenWrongOp))
        (θ : (v_2 (v_1)))
        
        E-StringLengthWrongOperandWithHandler
        ; Determine if we have a handler for the operation
        (where v_2 (getUnaryHandler v_1 
                                    (unopeventkey \#) 
                                    θ))
        
        (side-condition (not (equal? (term v_2) 
                                     (term nil))))]
   
   [--> (θ : ((\# objref)StrLenWrongOp))
        (θ : (δ (\# tableconstructor)))
        
        E-StringLengthWrongOperandTableLength
        ; Determine if we have a handler for the operation
        (where nil (getUnaryHandler objref
                                    (unopeventkey \#)
                                    θ))
        
        (side-condition (equal? (term (δ (type objref θ))) 
                                "table"))
        
        (where tableconstructor (getTable (derefTheta θ objref)))]
   
   [--> (θ : ((\# v)StrLenWrongOp))
        (θ : ($builtIn error (String_2)))
        
        E-StringLengthWrongOperandNoHandler
        ; Determine if we have a handler for the operation
        (where nil (getUnaryHandler v 
                                    (unopeventkey \#)
                                    θ))
        
        (where String_1 (δ (type v θ)))
        
        (side-condition (not (equal? (term String_1) 
                                     "table")))
        
        (where String_2 (errmessage StrLenWrongOp String_1))]
   
   ; Abnormal expressions with relational operators
   
   [--> (θ : ((v_1 == v_2)EqFail))
        (θ : (v_3 (v_1 v_2)))
        
        E-EqualityFailWithHandler
        ; Determine the type of sv_1
        (where v_3 (getEqualHandler v_1 v_2 θ))
        (side-condition (and (not (equal? (term v_3)
                                          (term nil)))
                            
                             (not (equal? (term v_3)
                                          (term false)))))]
   
   [--> (θ : ((v_1 == v_2)EqFail))
        (θ : false)
        
        E-EqualityFailNoHandler
        ; Determine the type of sv_1
        (where v_3 (getEqualHandler v_1 v_2 θ))
        
        (side-condition (or (equal? (term v_3) 
                                    (term nil))
                            
                            (equal? (term v_3) 
                                    (term false))))]
   
   [--> (θ : ((v_1 relop v_2)OrdCompWrongOps))
        (θ : (v_3 (v_1 v_2)))
        
        E-OrdCompFailWithHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey relop) 
                                  θ))
        
        (side-condition (and (not (equal? (term v_3)
                                          (term nil)))
                             
                             (not (equal? (term v_3)
                                          (term false)))))]
   
   [--> (θ : ((v_1 < v_2)OrdCompWrongOps))
        (θ : ($builtIn error (String)))
        
        E-LessThanFailNoHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <) 
                                  θ))
        
        (side-condition (or (equal? (term v_3)
                                    (term nil))
                             
                             (equal? (term v_3)
                                     (term false))))
        
        (where String (errmessage OrdCompWrongOps 
                                  (δ (type v_1 θ))
                                  (δ (type v_2 θ))))]
   
   [--> (θ : ((v_1 <= v_2)OrdCompWrongOps))
        (θ : (not (v_4 (v_2 v_1))))
        
        E-LessThanOrEqualFailWithAltHandler
        ; Obtain a handler for the operation
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <=) 
                                  θ))
        
        (side-condition (or (equal? (term v_3)
                                    (term nil))
                            
                            (equal? (term v_3)
                                    (term false))))

        (where v_4 (getBinHandler v_1
                                  v_2
                                  (binopeventkey <)
                                  θ))
        
        (side-condition (and (not (equal? (term v_4)
                                          (term nil)))
                            
                            (not (equal? (term v_4)
                                         (term false)))))]
   
   [--> (θ : ((v_1 <= v_2)OrdCompWrongOps))
        (θ : ($builtIn error (String)))
        
        E-LessThanOrEqualFailNoHandler
        ; Determine if simplevalue efectively has a meta-table
        (where v_3 (getBinHandler v_1 
                                  v_2 
                                  (binopeventkey <=) 
                                  θ))
        
        (side-condition (or (equal? (term v_3)
                                    (term nil))
                            
                            (equal? (term v_3)
                                    (term false))))

        (where v_4 (getBinHandler v_1
                                  v_2
                                  (binopeventkey <)
                                  θ))
        
        (side-condition (or (equal? (term v_4)
                                    (term nil))
                            
                            (equal? (term v_4)
                                    (term nil))))
        
        (where String (errmessage OrdCompWrongOps
                                  (δ (type v_1 θ))
                                  (δ (type v_2 θ))))]
   ))

(provide abnormal-exps)

#lang racket
; Function call

(require redex
         "../grammar.rkt"
         "../Meta-functions/substitution.rkt"
         "../Meta-functions/valStoreMetafunctions.rkt"
         "../Meta-functions/objStoreMetafunctions.rkt"
         "../Meta-functions/delta.rkt")

(define func-call
  (reduction-relation
   core-lang
   #:domain (σ : θ : s) 
   
   ; Function call
   [--> (σ_1 : θ : (objref (v ...)))
        (σ_2 : θ : (((substBlock s ((id e) ...)))Return))

        E-Apply
        ; v_1 is a reference to a function
        (side-condition (eq? (term (δ (type objref θ)))
                             "function"))
        
        ; Obtain the function value
        (where (function Name_1 (Name_2 ...) s end) (derefTheta θ objref))

        ; Length of the formal parameters list
        (where Number_1 ,(length (term (Name_2 ...))))

        ; Length of the actual parameters list
        (where Number_2 ,(length (term (v ...))))

        ; Take the corresponding amount of actual parameters
        (where (v_1 ...) ,(take (term (v ...))
                                (min (term Number_1)
                                     (term Number_2))))

        (where Number_3 ,(- (term Number_1) (term Number_2)))

        ; Calculate the amount of extra nil values
        (where Number_4 ,(if (>= (term Number_3) 0)
                             (term Number_3)
                             0))
        
        ; Add nil values, if needed
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (term Number_4)
                                             (term nil))))

        ; Add, to the value store, mappings to the actual paramaters
        (where (σ_2 (r ...)) (addSimpVal σ_1 (v_2 ...)))

        ; Create the substitution mapping that will replace formal
        ; parameters' identifiers by the corresponding references
        (where ((id e) ...) (createSubstMap (Name_2 ...) (r ...)))]

   ; Vararg function call
   [--> (σ_1 : θ : (objref (v ...)))
        (σ_2 : θ : (((substBlock s ((id_1 e_1) ... (<<< (< v_3 ... >)))))Return))

        E-ApplyVararg
        
        ; objref is a reference to a function
        (side-condition (eq? (term (δ (type objref θ)))
                             "function"))
        
        ; Obtain the function value
        (where (function Name_1 (Name_2 ... <<<) s end) (derefTheta θ objref))

        ; Length of the formal parameters list
        (where Number_1 ,(length (term (Name_2 ...))))

        ; Length of the actual parameters list
        (where Number_2 ,(length (term (v ...))))

        ; Take the corresponding amount of actual parameters
        (where (v_1 ...) ,(take (term (v ...))
                                (min (term Number_1)
                                     (term Number_2))))

        (where Number_3 ,(- (term Number_1) (term Number_2)))

        ; Calculate the amount of extra nil values
        (where Number_4 ,(if (>= (term Number_3) 0)
                             (term Number_3)
                             0))

        ; Take the extra-arguments, if present
        (where Number_5 ,(if (eq? (term Number_4) 0)
                             (- (term Number_2) (term Number_1))
                             0))
        
        ; Add nil values, if needed
        (where (v_2 ...) ,(append (term (v_1 ...))
                                  (make-list (term Number_4)
                                             (term nil))))

        ; (v_3 ...) are the values that go wrapped into the tuple
        (where (v_3 ...) ,(take-right (term (v ...)) (term Number_5)))

        ; Add, to the value store, mappings to the actual paramaters
        (where (σ_2 (r ...)) (addSimpVal σ_1 (v_2 ...)))

        ; Create the substitution mapping that will replace formal
        ; parameters' identifiers by the corresponding references
        (where ((id_1 e_1) ...) (createSubstMap (Name_2 ...) (r ...)))]
   

   ; Call over a non-function value
   [--> (σ : θ : (v (v_1 ...)))
        (σ : θ : ((v (v_1 ...))WrongFunCall))

        E-AlertWrongFunCall
        ; Determine that v_1 is not a reference to a function
        (side-condition (not (eq? (term (δ (type v θ)))
                                  "function")))]

   ; Method call
   [--> (σ : θ : (v : Name (e ...)))
        (σ : θ : ((v \[ String \]) (v e ...)))
        E-MethodCall

        (where String ,(~a (term Name)))]

   ; Return
   [--> (σ : θ : (((in-hole Elf (return (< v ... >))))Break))
        (σ : θ : (return (< v ... >)))
        E-InteractionReturnBreak]

   [--> (σ : θ : (((in-hole Elf (return (< v ... >))))Return))
        (σ : θ : (< v ... >))
        E-ReturnValues]

   [--> (σ : θ : (return v))
        (σ : θ : (return (< v >)))
        E-ReturnPutIntoTuple]

   [--> (σ : θ : ((\;)Return))
        (σ : θ : \;)
        E-ReturnNoValues]

   [--> (σ : θ : (((< v ... >))Return))
        (σ : θ : \;)
        E-ReturnNoValues2]
   )
  )

(provide func-call)

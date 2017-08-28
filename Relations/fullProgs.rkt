#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetafunctions.rkt"
         "./exps.rkt"
         "./expsObjStore.rkt"
         "./expsValStore.rkt"
         "./functionCall.rkt"
         "./builtIn.rkt"
         "./abnormalExps.rkt"
         "./stats.rkt"
         "./statsValStore.rkt"
         "./statsObjStore.rkt"
         "./abnormalStats.rkt")

; Semantics of complete programs

(define full-progs-rel
  (reduction-relation
   core-lang
   #:domain (σ : θ : s)
   
   ; Expressions
   [--> (σ : θ : (in-hole E e_1))
        (σ : θ : (in-hole E e_2))
        
        E-Exps
        (where (e_2) ,(apply-reduction-relation exps (term e_1)))
        
        (side-condition (not (equal? (term E) (term hole))))]
   
   ; Tuples
   [--> (σ : θ : (in-hole E_1 (in-hole E_2 (< >))))
        (σ : θ : (in-hole E_1 (in-hole E_2 nil)))
        
        E-TruncateUnwrapEmptyTuple
        
        (side-condition (and (or (redex-match core-lang
                                              Et
                                              (term E_2))
                                 
                                 (redex-match core-lang
                                              Eu
                                              (term E_2))
                                 )
                             (not (redex-match core-lang
                                               functioncall
                                               (term (in-hole E_2 (< >)))))

                             (not (redex-match core-lang
                                               ($builtIn Name (v ... (< >) e ...))
                                               (term (in-hole E_2 (< >)))))))]

   ; This rules are added to avoid adding an extra nil value when calling
   ; a function. From the point of view of an implementation, it would result
   ; in a stack frame with one extra value, which sometimes leads to non compliance
   ; with some Lua services (like select("#",< >)).
   [--> (σ : θ : (in-hole E_1 (v_1 (v_2 ... (< >) e ...))))
        (σ : θ : (in-hole E_1 (v_1 (v_2 ... e ...))))
        
        E-DiscEmpTupFunCall]

   [--> (σ : θ : (in-hole E_1 ($builtIn Name (v ... (< >) e ...))))
        (σ : θ : (in-hole E_1 ($builtIn Name (v ... e ...))))
        
        E-DiscEmpTupBuiltIn]
   
   [--> (σ : θ : (in-hole E (in-hole Et (< v_1 v_2 ... >))))
        (σ : θ : (in-hole E (in-hole Et v_1)))
        E-TruncateNonEmptyTuple]
   
   [--> (σ : θ : (in-hole E (in-hole Eu (< v_1 v_2 ... >))))
        (σ : θ : (in-hole E (fixUnwrap Eu (v_1 v_2 ...))))
        E-UnwrapNonEmptyTuple]
   
   
   [--> (σ : θ : (in-hole E (in-hole Ed (< v ... >))))
        (σ : θ : (in-hole E (in-hole Ed \;)))
        E-DiscardTuple
        
        (side-condition (not (redex-match core-lang
                                          hole
                                          (term Ed))))]
   
   [--> (σ : θ : (< v ... >))
        (σ : θ : \;)
        E-DiscardTuple2]
   
   ; Expressions that interact with the value store
   [--> (σ_1 : θ : (in-hole E e_1))
        (σ_2 : θ : (in-hole E e_2))
        
        E-valStoreExpressions
        (where ((σ_2 : e_2)) ,(apply-reduction-relation exps-val-store
                                                        (term (σ_1 : e_1))))]
   
   ; Expressions that interact with the object store
   [--> (σ : θ_1 : (in-hole E e_1))
        (σ : θ_2 : (in-hole E e_2))
        
        E-objStoreExpressions
        (where ((θ_2 : e_2)) ,(apply-reduction-relation exps-obj-store
                                                        (term (θ_1 : e_1))))]
   
   
   [--> (σ_1 : θ_1 : (in-hole E functioncall))
        (σ_2 : θ_2 : (in-hole E ((s_1)FunCall)))
        E-FunCallExp
        
        (where ((σ_2 : θ_2 : s_1)) ,(apply-reduction-relation func-call
                                                              (term (σ_1 : θ_1 : functioncall))))
        
        (side-condition (not (redex-match core-lang
                                          ((v_1 (v_2 ...))WrongFunCall)
                                          (term s_1))))
        
        (side-condition (redex-match core-lang
                                     s
                                     (term (in-hole E ((s_1)FunCall)))))]
   
   [--> (σ_1 : θ_1 : (in-hole E functioncall))
        (σ_2 : θ_2 : (in-hole E s_1))
        E-FunCallStat
        
        (where ((σ_2 : θ_2 : s_1)) ,(apply-reduction-relation func-call
                                                              (term (σ_1 : θ_1 : functioncall))))
        
        (side-condition (redex-match core-lang
                                     s
                                     (term (in-hole E s_1))))]
   
   [--> (σ : θ : (in-hole E s_1))
        (σ : θ : (in-hole E s_2))
        E-Return
        
        (side-condition (not (redex-match core-lang
                                          functioncall
                                          (term s_1))))
        
        (where ((σ : θ : s_2)) ,(apply-reduction-relation func-call
                                                          (term (σ : θ : s_1))))]
   
   ; Built-in services
   [--> (σ : θ_1 : (in-hole E ($builtIn Name (v ...))))
        (σ : θ_2 : (in-hole E any))
        E-BuiltIn
        (where ((θ_2 : any)) ,(apply-reduction-relation built-in
                                                        (term (θ_1 : ($builtIn Name (v ...))))))]
   
   ; Abnormal expressions
   [--> (σ : θ_1 : (in-hole E e_1))
        (σ : θ_2 : (in-hole E e_2))
        
        E-abnormalExpressions
        (where ((θ_2 : e_2)) ,(apply-reduction-relation abnormal-exps
                                                        (term (θ_1 : e_1))))]
   
   ; Statements
   [--> (σ : θ : (in-hole E s_1))
        (σ : θ : (in-hole E s_2))
        E-simpleStatements
        (where (s_2) ,(apply-reduction-relation stats
                                                (term s_1)))]
   
   [--> (σ : θ : (break Name evaluatedtuple))
        (σ : θ : \;)
        E-breakStatement3]
   
   ; Error
   [--> (σ : θ : (in-hole Enp ($err v)))
        (σ : θ : ($err v))
        
        E-ErrorPropagation
        (side-condition (not (eq? (term Enp)
                                  (term hole))))]
   
   ; Protected mode
   [--> (σ : θ : (in-hole E (((in-hole Enp ($err v)))ProtectedMode)))
        (σ : θ : (in-hole E (< false v >)))
        
        E-ProtectedModeErrorCatched]

   [--> (σ : θ : (in-hole E (((in-hole Enp ($err v_1)))ProtectedMode v_2)))
        (σ : θ : (in-hole E (v_2 (v_1))))
        
        E-XProtectedModeErrorCatched]
   
   [--> (σ : θ : (in-hole E (((< v ... >))ProtectedMode)))
        (σ : θ : (in-hole E (< true (< v ... >) >)))
        
        E-ProtectedModeNoErrorWithReturnedValues]

   [--> (σ : θ : (in-hole E (((< v_1 ... >))ProtectedMode v_2)))
        (σ : θ : (in-hole E (< true (< v_1 ... >) >)))
        
        E-XProtectedModeNoErrorWithReturnedValues]
   
   [--> (σ : θ : (in-hole E ((\;)ProtectedMode)))
        (σ : θ : (in-hole E (< true (< >) >)))
        E-ProtectedModeNoErrorWithoutReturnedValues]

   [--> (σ : θ : (in-hole E ((\;)ProtectedMode v)))
        (σ : θ : (in-hole E (< true (< >) >)))
        E-XProtectedModeNoErrorWithoutReturnedValues]
   
   
   ; Statements that interact with the object store
   [--> (σ : θ_1 : (in-hole E s_1))
        (σ : θ_2 : (in-hole E s_2))
        
        E-objStoreStatements
        (where ((θ_2 : s_2)) ,(apply-reduction-relation stats-obj-store
                                                        (term (θ_1 : s_1))))]
   
   ; Statements that interact with the value store
   [--> (σ_1 : θ : (in-hole E s_1))
        (σ_2 : θ : (in-hole E s_2))
        
        E-simpValStoreStatements
        (where ((σ_2 : s_2)) ,(apply-reduction-relation stats-val-store
                                                        (term (σ_1 : s_1))))]
   
   ; Abnormal statements
   [--> (σ : θ_1 : (in-hole E s_1))
        (σ : θ_2 : (in-hole E s_2))
        
        E-abnormalStatements
        (where ((θ_2 : s_2)) ,(apply-reduction-relation abnormal-stats
                                                        (term (θ_1 : s_1))))]
   ))

(provide full-progs-rel)

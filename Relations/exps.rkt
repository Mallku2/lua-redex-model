#lang racket
; Expressions that don't interact with some store

(require redex
         "../grammar.rkt"
         "../Meta-functions/grammarMetafunctions.rkt"
         "../Meta-functions/delta.rkt"
         racket/format ; String conversion
         )

(define exps
  (reduction-relation
   core-lang                                             
   #:domain e
   
   ; Operator ()
   [--> (\( (< v_1 v ... >) \))
        v_1
        E-ParenthesOnNonEmptyTuple]
   
   [--> (\( (< >) \))
        nil
        E-ParenthesOnEmptyTuple]
   
   [--> (\( v \))
        v
        E-ParenthesisOnValue]
   
   ; Function call finalization
   [--> ((\;)FunCall)
        (< >)
        E-EndingFuncionCallStat]
   
   [--> (((< v_1 ... >))FunCall)
        (< v_1 ... >)
        E-EndingFuncionCallExp]
   
   ; Primitive Operations
   ; Binary operations
   [--> (v_1 binop v_2)
        (δ (binop v_1 v_2))
        E-BinOp
        
        (side-condition (or (and (redex-match core-lang Number (term v_1))
                                 (redex-match core-lang Number (term v_2))
                                 (or (term (isArithBinOp binop))
                                     (term (isRelationalOperator binop))))
                            
                            (and (redex-match core-lang String (term v_1))
                                 (redex-match core-lang String (term v_2))
                                 (or (term (isRelationalOperator binop))
                                     (equal? (term binop) (term ..))))))]
   
   ; Logical conectives
   [--> (v binop e)
        (δ (binop v e))
        E-LogicOp
        
        (side-condition (term (isBooleanBinOp binop)))]
   
   ; Unary operations
   [--> (unop v)
        (δ (unop v))
        E-UnOp
        
        (side-condition (or (and (redex-match core-lang String (term v))
                                 (equal? (term unop) (term \#)))
                            
                            (and (redex-match core-lang Number (term v))
                                 (equal? (term unop) (term -)))
                            
                            (equal? (term unop) (term not))))]
   
   ; Equality comparison
   [--> (v_1 == v_2)
        true
        E-EqualitySuccess
        (side-condition (redex-match core-lang 
                                     true
                                     (term (δ (== v_1 v_2)))))]
   
   ; Translation of expressions involving > and >=
   [--> (v_1 binop v_2)
        (v_2 any v_1)
        E-TranslateComparison
        
        (side-condition (or (equal? (term binop) (term >))
                            (equal? (term binop) (term >=))))
        (where any (translateComparisonOp binop))]
   
   ; Coercion
   [--> (v_1 binop v_2)
        (e_1 binop e_2)
        E-ArithBinOpCoercion
        
        (side-condition (and (or (not (redex-match core-lang
                                                   Number
                                                   (term v_1)))
                                 (not (redex-match core-lang
                                                   Number
                                                   (term v_2))))
                             
                             (term (isArithBinOp binop))))
        
        (where e_1 (δ (tonumber v_1 nil)))
        (where e_2 (δ (tonumber v_2 nil)))

        (side-condition (and (not (redex-match core-lang
                                               nil
                                               (term e_1)))
                             (not (redex-match core-lang
                                               nil
                                               (term e_2)))))]
   
   [--> (- v)
        (- e)

        (side-condition (not (redex-match core-lang Number (term v))))

        E-NegationCoercion
        
        (where e (δ (tonumber v nil)))

        (side-condition (not (redex-match core-lang
                                          nil
                                          (term e))))]

   [--> (v_1 .. v_2)
        (any_1 .. any_2)
        E-StringConcatCoercion
        
        (side-condition (and
                         (or (not (redex-match core-lang
                                               String
                                               (term v_1)))
                             (not (redex-match core-lang
                                               String
                                               (term v_2))))
                         
                         (or (redex-match core-lang
                                          Number
                                          (term v_1))

                             (redex-match core-lang
                                          String
                                          (term v_1)))

                         (or (redex-match core-lang
                                          Number
                                          (term v_2))

                             (redex-match core-lang
                                          String
                                          (term v_2)))))
        ; No need to look into the store for
        ; information about the given values, for
        ; coercion purposes
        (where any_1 (δ (tostring v_1 ())))
        (where any_2 (δ (tostring v_2 ())))]
   
   ; Abnormal situations with primitive operators
   [--> (v_1 binop v_2)
        ((v_1 binop v_2)ArithWrongOps)
        E-ArithBinOpWrongOps
        
        (side-condition (and (or (not (redex-match core-lang
                                                   Number
                                                   (term v_1)))
                                 (not (redex-match core-lang
                                                   Number
                                                   (term v_2))))
                             (term (isArithBinOp binop))))
        
        (where any_1 (δ (tonumber v_1 nil)))
        (where any_2 (δ (tonumber v_2 nil)))
        
        (side-condition (or (not (redex-match core-lang
                                              e
                                              (term any_1)))

                            (redex-match core-lang
                                         nil
                                         (term any_1))
                            
                            (not (redex-match core-lang
                                              e
                                              (term any_2)))

                            (redex-match core-lang
                                         nil
                                         (term any_2))))]
   
   [--> (- v)
        ((- v)NegWrongOp)
        E-AlertNegationWrongOperand
        
        (side-condition (not (redex-match core-lang
                                          Number
                                          (term v))))
        (where any (δ (tonumber v nil)))
        
        (side-condition (or (not (redex-match core-lang e (term any)))
                            (redex-match core-lang nil (term any))))]
   
   [--> (v_1 .. v_2)
        ((v_1 .. v_2)StrConcatWrongOps)
        E-AlertStringConcatWrongOperands
        
        (side-condition (or (and (not (redex-match core-lang
                                                   String
                                                   (term v_1)))
                                 (not (redex-match core-lang
                                                   Number
                                                   (term v_1))))
                            
                            (and (not (redex-match core-lang
                                                   String
                                                   (term v_2)))
                                 (not (redex-match core-lang
                                                   Number
                                                   (term v_2))))))]
   
   [--> (\# v)
        ((\# v)StrLenWrongOp)
        E-AlertStringLengthWrongOperand
        
        (side-condition (not (redex-match core-lang String (term v))))]
   
   [--> (v_1 == v_2)
        ((v_1 == v_2)EqFail)
        E-AlertEqualityFail
        
        (side-condition (equal? (term (δ (== v_1 v_2))) (term false)))]
   
   [--> (v_1 binop v_2)
        ((v_1 binop v_2)OrdCompWrongOps)
        E-AlertOrdCompWrongOps
        
        (side-condition (and (term (isRelationalOperator binop))
                             
                             (not (and (redex-match core-lang
                                                    String
                                                    (term v_1))
                                       (redex-match core-lang
                                                    String
                                                    (term v_2))))
                             
                             (not (and (redex-match core-lang
                                                    Number
                                                    (term v_1))
                                       (redex-match core-lang
                                                    Number
                                                    (term v_2))))))] 
   ))

(provide exps)

#lang racket
; Black-box testing for abnormal statements

(require redex
         "../grammar.rkt"
         "../Relations/abnormalStats.rkt")

(define (abnormal-stats-test-suite)
  
  ; E-FieldAssignWrongKeyNormal
  (test-->> abnormal-stats
            (term ((((objr 1) ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 2) ((\{ \}) (objr 1)))
                    ((objr 6) (function X () ((ref 1) ()) end))) 
                   : (((((objr 2) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 2) ((\{ \}) (objr 1)))
                    ((objr 6) (function X () ((ref 1) ()) end))) 
                   : ((objr 6) ((objr 2) 1 2)))))
  
  ; E-FieldAssignWrongKeyRepeat
  (test-->> abnormal-stats
            (term ((((objr 1)
                     ((\{ (\[ "__newindex" \] = (objr 3)) \}) nil))
                    ((objr 2)
                     ((\{ \}) (objr 1)))
                    ((objr 3)
                     ((\{ (\[ 1 \] = 1) \}) nil)))
                   : (((((objr 2) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1)
                     ((\{ (\[ "__newindex" \] = (objr 3)) \}) nil))
                    ((objr 2)
                     ((\{ \}) (objr 1)))
                    ((objr 3)
                     ((\{ (\[ 1 \] = 1) \}) nil))) 
                   : ((((objr 3) \[ 1 \])) = (2)))))
  
  ; E-FieldAssignWrongKeyNoHandler
  (test-->> abnormal-stats
            (term ((((objr 1) ((\{ \}) nil))
                    ((objr 2) ((\{ \}) (objr 1)))) 
                   : (((((objr 2) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil))
                    ((objr 2) ((\{ (\[ 1 \] = 2) \}) (objr 1)))) 
                   : (< (objr 2) >))))
  
  (test-->> abnormal-stats
            (term ((((objr 1) ((\{ \}) nil)))
                   : (((((objr 1) \[ 1 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : (< (objr 1) >))))

  (test-->> abnormal-stats
            (term ((((objr 1) ((\{ \}) nil)))
                   : (((((objr 1) \[ nil \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : (< ($err "table index is nil") >))))

  (test-->> abnormal-stats
            (term ((((objr 1) ((\{ \}) nil)))
                   : (((((objr 1) \[ +nan.0 \])) = (2))FieldAssignWrongKey)))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : (< ($err "table index is NaN") >))))
  
  ; E-FieldAssignOverNonTableNormal
  (test-->> abnormal-stats
            (term ((((objr 1) 
                     ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 6) 
                     (function X () ((ref 1) ()) end)))
                   : ((((1 \[ 2 \])) = (2))FieldAssignOverNonTable)))
            
            (term ((((objr 1) 
                     ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                    ((objr 6) 
                     (function X () ((ref 1) ()) end)))
                   : ((objr 6) (1 2 2)))))
  
    ; E-FieldAssignOverNonTableRepeat
    (test-->> abnormal-stats
              (term ((((objr 1)
                       ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                      ((objr 6)
                       ((\{ \}) nil)))
                     : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable)))
              
              (term ((((objr 1)
                       ((\{ (\[ "__newindex" \] = (objr 6)) \}) nil))
                      ((objr 6)
                       ((\{ \}) nil)))
                     : ((((objr 6) \[ 2 \])) = (3)))))
    
    ; E-FieldAssignOverNonTableNoHandler
    (test-->> abnormal-stats
              (term (() : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable)))
              (term (() : ($builtIn error
                                    ("attempt to index a number value"))))

              )
    
    (test-->> abnormal-stats
              (term ((((objr 1) ((\{ \}) nil)))
                     : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable)))


              (term ((((objr 1) ((\{ \}) nil)))
                     : ($builtIn error ("attempt to index a number value"))))

              )
  
  (test-results))

(provide abnormal-stats-test-suite)

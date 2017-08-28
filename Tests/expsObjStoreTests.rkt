#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/expsObjStore.rkt")

(define (exps-obj-store-test-suite)
  ; Function creation
  (test-->> exps-obj-store
            (term (() : (function X () (Y ()) end)))
            (term ((((objr 6) (function X () (Y ()) end))) 
                   : (objr 6))))
  
  (test-->> exps-obj-store
            (term ((((objr 1) (function X () (Y ()) end))) 
                   : (function X () (Y ()) end)))
            (term ((((objr 1) (function X () (Y ()) end))) 
                   : (objr 1))))
  
  (test-->> exps-obj-store
            (term ((((objr 1) (function X () ((ref 1) ()) end))) 
                   : (function X ()((ref 2) ()) end)))
            (term ((((objr 1) (function X () ((ref 1) ()) end))
                    ((objr 2) (function X () ((ref 2) ()) end))) 
                   : (objr 2))))
  
  ; Table creation
  (test-->> exps-obj-store
            (term (() : (\{ \})))
            (term ((((objr 6) ((\{ \}) nil))) : (objr 6))))
  
  (test-->> exps-obj-store
            (term (() : (\{ 1 (\[ 1 \] = 2) 2 (\[ 2 \] = 3) nil 4 \})))
            (term ((((objr 6) ((\{ (\[ 1 \] = 1) 
                                   (\[ 2 \] = 2) 
                                   (\[ 4 \] = 4) \}) nil))) 
                   : (objr 6))))
  
  ; Table indexing
  (test-->> exps-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 4 \] = 4) 
                                   \}) nil))) 
                   : ((objr 1) \[ 4 \])))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 4 \] = 4) 
                                   \}) nil))) : 4)))

  ; Coercion to string
;  (test-->> exps-obj-store
;            (term (() : ("1" .. 2.0)))
;            (term (() : ("1" .. "2"))))
;  
;  (test-->> exps-obj-store
;            (term (() : (1 .. "2.0")))
;            (term (() : ("1" .. "2.0"))))
  
  ; E-AlertKeyNotFound
  (test-->> exps-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 1) \}) (objr 2)))) 
                   : ((objr 1) \[ 2 \])))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1) \}) (objr 2)))) 
                   : (((objr 1) \[ 2 \])KeyNotFound))))
  ; E-AlertNonTableIndexed
  (test-->> exps-obj-store
            (term (() : (1 \[ 2 \])))
            
            (term (() : ((1 \[ 2 \])NonTableIndexed))))
  
  (test-results))

(provide exps-obj-store-test-suite)

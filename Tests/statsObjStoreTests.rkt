#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/statsObjStore.rkt")

(define (stats-obj-store-test-suite)
  ; Table assignment
  ; Normal assignment
  (test-->> stats-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil))) 
                   : ((((objr 1) \[ 4 \])) = (9))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 9) \}) nil))) 
                   : (< (objr 1) >))))
  
  (test-->> stats-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil))) 
                   : ((((objr 1) \[ 2 \])) = (10))))

            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 10)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil))) 
                   : (< (objr 1) >))))
  
  (test-->> stats-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 2 \] = 6)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil))) 
                   : ((((objr 1) \[ 2 \])) = (nil))))

            (term ((((objr 1) ((\{ (\[ 1 \] = 5)
                                   (\[ 3 \] = 7)
                                   (\[ 4 \] = 8) \})
                               nil))) 
                   : (< (objr 1) >))))
  
  ; Trying to index with key nil
  (test-->> stats-obj-store
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 3 \] = nil)
                                   (\[ 4 \] = 4) \}) nil))) 
                   : ((((objr 1) \[ nil \])) = (1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 1)
                                   (\[ 2 \] = 2)
                                   (\[ 3 \] = nil)
                                   (\[ 4 \] = 4) \}) nil)))
                   : (((((objr 1) \[ nil \])) = (1))FieldAssignWrongKey))))
  
  ; E-AlertFieldAssignWrongKey
  (test-->> stats-obj-store
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ((((objr 1) \[ 1 \])) = (2))))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : (((((objr 1) \[ 1 \])) = (2))FieldAssignWrongKey))))
  
  ; E-AlertFieldAssignOverNonTable
  (test-->> stats-obj-store
            (term (() : (((1 \[ 2 \])) = (3))))
            (term (() : ((((1 \[ 2 \])) = (3))FieldAssignOverNonTable))))                                                                
  
  (test-results))

(provide stats-obj-store-test-suite)

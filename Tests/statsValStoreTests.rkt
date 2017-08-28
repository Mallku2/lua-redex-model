#lang racket
; Black-box testing for statements that interact with the simple value store

(require redex
         "../grammar.rkt"
         "../Relations/statsValStore.rkt")

(define (stats-val-store-test-suite)
  ; Ordinary variable assignment
  (test-->> stats-val-store
            (term ((((ref 1) 1) ((ref 2) 2)) : 
                  (((ref 1)) = (3))))
  
            (term ((((ref 1) 3) ((ref 2) 2)) : \;)))
  ; Local statement
  (test-->> stats-val-store
            (term (() : 
                  (local (X) = (1) in (X ()) end)))
            
            (term ((((ref 1) 1)) :  ((ref 1) ()))))

  (test-->> stats-val-store
            (term (() : 
                  (local (X Y) = (1 nil) in (X ()) end)))
            (term ((((ref 1) 1) ((ref 2) nil)) : ((ref 1) ()))))
  (test-results))

(provide stats-val-store-test-suite)

#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/expsValStore.rkt")

(define (exps-val-store-test-suite)
  ; Implicit dereferencing
  (test-->> exps-val-store
            (term ((((ref 1) 2)) : (ref 1)))
            (term ((((ref 1) 2)) : 2)))
  
  (test-->> exps-val-store
            (term ((((ref 1) 1)
                    ((ref 2) 2)
                    ((ref 3) 3)
                    ((ref 4) 4)) : (ref 3)))
            (term ((((ref 1) 1)
                    ((ref 2) 2)
                    ((ref 3) 3)
                    ((ref 4) 4)) : 3)))
  (test-results))

(provide exps-val-store-test-suite)

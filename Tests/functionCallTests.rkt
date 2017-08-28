#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/functionCall.rkt")

(define (func-call-test-suite)
  ; Function call
  ; Normal case
  (test-->> func-call
            (term ((((ref 1) 1) ((ref 2) 2)) 
                   : (((objr 1) (function X (Y) (Y ()) end))) 
                   : ((objr 1) (2))))
            
            (term ((((ref 1) 1) ((ref 2) 2) ((ref 3) 2)) 
                   : (((objr 1) (function X (Y) (Y ()) end))) 
                   : ((((ref 3) ()))Return))))
  
  ; More values than needed passed in the call
  (test-->> func-call
            (term ((((ref 1) 1) ((ref 2) 2)) 
                   : (((objr 1) (function X (Y) (Y ()) end))) 
                   : ((objr 1) (2 1))))
            
            (term ((((ref 1) 1) ((ref 2) 2) ((ref 3) 2)) 
                   : (((objr 1) (function X (Y) (Y ()) end))) 
                   : ((((ref 3) ()))Return))))
  
  ; Lesser values than needed passed in the call
  (test-->> func-call
            (term ((((ref 1) 1) ((ref 2) 2)) 
                   : (((objr 1) (function X (Y Z) (Z ()) end))) 
                   : ((objr 1) (2))))
            
            (term ((((ref 1) 1)
                    ((ref 2) 2)
                    ((ref 3) 2)
                    ((ref 4) nil)) 
                   : (((objr 1) (function X (Y Z) (Z ()) end))) 
                   : ((((ref 4) ()))Return))))
  
  ; Vararg function: normal case
  (test-->> func-call
            (term (() 
                   : (((objr 1) (function X (X <<<) ((\( <<< \)) ()) end))) 
                   : ((objr 1) (1 2))))
            
            (term ((((ref 1) 1)) 
                   : (((objr 1) (function X (X <<<) ((\( <<< \)) ()) end))) 
                   : ((((\( (< 2 >) \)) ()))Return))))
  
  ; Vararg function: few arguments
  (test-->> func-call
            (term (() 
                   : (((objr 1) (function X (X Y <<<) ((\( <<< \)) ()) end))) 
                   : ((objr 1) (1))))
            
            (term ((((ref 1) 1) ((ref 2) nil)) 
                   : (((objr 1) (function X (X Y <<<) ((\( <<< \)) ()) end))) 
                   : ((((\( (< >) \)) ()))Return))))
  
  ; E-AlertWrongFunCall
  (test-->> func-call
            (term (() : () : (1 (1))))
            
            (term (() : () : ((1 (1))WrongFunCall))))

  ; Method call
  (test-->> func-call
            (term (() 
                   : (((objr 1) ((\{ (\[ "method_name" \] = (objr 2)) \}) nil))
                      ((objr 2) (function $method_name (self X) (return (< self X >)) end))) 
                   : ((objr 1) : method_name (1))))
            
            (term (() 
                   : (((objr 1) ((\{ (\[ "method_name" \] = (objr 2)) \}) nil))
                      ((objr 2) (function $method_name (self X)  (return (< self X >)) end))) 
                   : (((objr 1) \[ "method_name" \]) ((objr 1) 1)))))

  ; Return
  (test-->> func-call
            (term (() : () : ((\;)Return)))
            (term (() : () : \;)))
  
  (test-->> func-call
            (term (() : () : (((return (< 1 >)))Return)))
            (term (() : () : (< 1 >))))

  (test-->> func-call
            (term (() : () : (((return (< 1 >)))Break)))
            (term (() : () : (return (< 1 >)))))
  
  (test-results))

(provide func-call-test-suite)

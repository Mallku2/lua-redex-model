#lang racket
(require redex
         "../Meta-functions/substitution.rkt")

; "black-box testing" with "equivalence class partitioning"

(define (subst-exp-test-suite)
  ; Expression without structure
  (test-equal (term (substExp nil ((X (ref 1))))) (term nil))
  (test-equal (term (substExp true ((X (ref 1))))) (term true))
  (test-equal (term (substExp void ((X (ref 1))))) (term void))
  (test-equal (term (substExp "" ((X (ref 1))))) (term ""))
  (test-equal (term (substExp 1 ((X (ref 1))))) (term 1))
  (test-equal (term (substExp (objr 1) ((X (ref 1))))) (term (objr 1)))
  (test-equal (term (substExp (ref 1) ((X (ref 1))))) (term (ref 1)))
  (test-equal (term (substExp X ((X (ref 1))))) (term (ref 1)))
  (test-equal (term (substExp X ((Y (ref 1))))) (term X))
  (test-equal (term (substExp <<< ((<<< (ref 1))))) (term (ref 1)))
  ; Function call
  (test-equal (term (substExp (X ()) ((X (ref 1))))) (term ((ref 1) ())))
  (test-equal (term (substExp (X (1 2 3)) ((X (ref 1))))) (term ((ref 1) (1 2 3))))
  (test-equal (term (substExp ((\( <<< \)) ()) ((<<< (ref 1)))))
                    (term ((\( (ref 1) \)) ())))
  ; '(' ')' operator
  (test-equal (term (substExp (\( X \)) ((X (ref 1))))) (term (\( (ref 1) \))))
  (test-equal (term (substExp (\( <<< \)) ((<<< (ref 1))))) (term (\( (ref 1) \))))
  ; Table indexing
  (test-equal (term (substExp (X \[ Y \]) ((X (ref 1)) (Y (ref 2))))) 
              (term ((ref 1) \[ (ref 2) \])))
  (test-equal (term (substExp (X \[ <<< \]) ((X (ref 1)) (<<< (ref 2))))) 
              (term ((ref 1) \[ (ref 2) \])))
  ; Function definition
  (test-equal (term (substExp (function A () (X ()) end) ((X (ref 1))))) 
              (term (function A () ((ref 1) ()) end)))
  
  (test-equal (term (substExp (function A (X Y) ((X ()) (Z ())) 
                                 end) ((X (ref 1))))) 
              (term (function A (X Y) ((X ()) (Z ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<) ((X ()) (Z ())) 
                                 end) ((X (ref 1)))))
              (term (function A (X Y <<<) ((X ()) (Z ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<) ((X ()) (Z ())) 
                                 end) ((Z (ref 1)) (<<< (ref 2)))))
              (term (function A (X Y <<<) ((X ()) ((ref 1) ())) end)))
  
  (test-equal (term (substExp (function A (X Y) ((X ()) (Z ())) 
                                 end) ((Z (ref 1))))) 
              (term (function A (X Y) ((X ()) ((ref 1) ())) end)))
  
  (test-equal (term (substExp (function A (X Y <<<) ((X ()) (Z ())) 
                                            end) ((Z (ref 1))))) 
              (term (function A (X Y <<<) ((X ()) ((ref 1) ())) end)))
  ; Table constructor
  (test-equal (term (substExp (\{ (\[ X \] = Y) (\[ Z \] = 1) \}) 
                              ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (\{ (\[ (ref 1) \] = (ref 2)) (\[ (ref 3) \] = 1) \})))
  
  (test-equal (term (substExp (\{ (\[ X \] = Y) (\[ Z \] = 1) \}) 
                              ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (\{ (\[ (ref 1) \] = (ref 2)) (\[ (ref 3) \] = 1) \})))
  
  (test-equal (term (substExp (\{ (\[ X \] = Y) (\[ <<< \] = 1) \}) 
                              ((X (ref 1)) (Y (ref 2)) (<<< (ref 3)))))
              (term (\{ (\[ (ref 1) \] = (ref 2)) (\[ (ref 3) \] = 1) \})))
  ; Binary operators
  (test-equal (term (substExp (X + Y) ((X (ref 1)) (Y (ref 2))))) 
              (term ((ref 1) + (ref 2))))
  (test-equal (term (substExp (X + <<<) ((X (ref 1)) (<<< (ref 2))))) 
              (term ((ref 1) + (ref 2))))
  ; Unary operators
  (test-equal (term (substExp (- X) ((X (ref 1))))) (term (- (ref 1))))
  (test-results))


(define (subst-block-test-suite)
  ; Concatenation statement
  (test-equal (term (substBlock ((X ()) (Y ())) ((X (ref 1)) (Y (ref 2))))) 
              (term (((ref 1) ()) ((ref 2) ()))))
  ; Block Bo-End
  (test-equal (term (substBlock (do (X ()) end) ((X (ref 1))))) 
              (term (do ((ref 1) ()) end)))
  ; Return statement
  (test-equal (term (substBlock (return (< X >)) ((X (ref 1))))) 
              (term (return (< (ref 1) >))))
  
  (test-equal (term (substBlock (return <<<) ((<<< (< (ref 1) >))))) 
              (term (return (< (ref 1) >))))
  ; Conditional
  (test-equal (term (substBlock (if X then (Y ()) else (Z ()) end) 
                                ((X (ref 1)) (Y (ref 2)) (Z (ref 3)))))
              (term (if (ref 1) then ((ref 2) ()) else ((ref 3) ()) end)))
  
  (test-equal (term (substBlock (if <<< then (Y ()) else (Z ()) end) 
                                ((Y (ref 1)) (Z (ref 2)) (<<< (ref 3)))))
              (term (if (ref 3) then ((ref 1) ()) else ((ref 2) ()) end)))
  ; While loop
  (test-equal (term (substBlock (while X do (Y ()) end) 
                                ((X (ref 1)) (Y (ref 2)))))
              (term (while (ref 1) do ((ref 2) ()) end)))
  
  (test-equal (term (substBlock (while <<< do (Y ()) end)
                                ((Y (ref 1)) (<<< (ref 2)))))
              (term (while (ref 2) do ((ref 1) ()) end)))
  ; Local statement
  (test-equal (term (substBlock (local (X Y) = (X Y) in ((X ()) (Y ())) end) 
                                ((X (ref 1)) (Y (ref 2)))))
              (term (local (X Y) = ((ref 1) (ref 2)) in ((X ()) (Y ())) end)))
  
  (test-equal (term (substBlock (local (X Y) = (X Y) in ((U ()) ((V ())  
                                                        ((X ()) 
                                                         (Y ())))) end) 
                                ((U (ref 1)) (V (ref 2)))))
              (term (local (X Y) = (X Y) in (((ref 1) ()) 
                                             (((ref 2) ()) 
                                              ((X ())
                                               (Y ())))) end)))
  
  (test-equal (term (substBlock (local (X Y) = (X Y) in ((U ()) 
                                                         ((V (<<<)) 
                                                        ((X ())
                                                         (Y ())))) end) 
                                ((U (ref 1)) (V (ref 2)) (<<< (ref 3)))))
              (term (local (X Y) = (X Y) in (((ref 1) ()) (((ref 2) ((ref 3))) 
                                              ((X ()) (Y ())))) end)))
  ; Variable assignment
  (test-equal (term (substBlock ((X Y) = (U V))
                                ((X (ref 1)) (Y (ref 2)) (U (ref 3)) (V (ref 4)))))
              (term (((ref 1) (ref 2)) = ((ref 3) (ref 4)))))
  
  (test-equal (term (substBlock (((X \[ Y \])) = (U V))
                                ((X (ref 1)) (Y (ref 2)) (U (ref 3)) (V (ref 4)))))
              (term ((((ref 1) \[ (ref 2) \])) = ((ref 3) (ref 4)))))
  
  (test-equal (term (substBlock ((X Y) = (U <<<))
                                ((X (ref 1)) (Y (ref 2)) (U (ref 3)) (<<< (ref 4)))))
              (term (((ref 1) (ref 2)) = ((ref 3) (ref 4)))))
  
  (test-results))

(define (subs-test-suite)
  (subst-block-test-suite)
  (subst-exp-test-suite)
  )

(provide subs-test-suite)

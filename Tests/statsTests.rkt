#lang racket
; Black-box testing for simple statements

(require redex
         "../grammar.rkt"
         "../Relations/stats.rkt")

(define (stats-test-suite)
  ; Conditional
  (test-->> stats
            (term (if true then (X ()) else (Y ()) end))
            (term (X ())))
  (test-->> stats
            (term (if (objr 1) then (X ()) else (Y ()) end))
            (term (X ())))
  (test-->> stats
            (term (if false then (X ()) else (Y ()) end))
            (term (Y ())))
  (test-->> stats
            (term (if nil then (X ()) else (Y ()) end))
            (term (Y ())))
  ; While loop
  (test-->> stats
            (term (while false do (X ()) end))
            (term ((($nextItWhile false do (X ()) end))Break)))
  ; Concatenation of statements
  (test-->> stats
            (term (\; (Y ())))
            (term (Y ())))
  ; Block Do-End
  (test-->> stats
            (term (do \; end))
            (term \;))
  
  ; List length-equating rules
  ; E-AssignDiscardValues
  (test-->> stats
            (term (((ref 1)) = (1 2)))
            (term (((ref 1)) = (1))))
  ; E-AssignCompleteValues
  (test-->> stats
            (term (((ref 1) (ref 2)) = (1)))
            (term ((((ref 2)) = (nil)) (((ref 1)) = (1)))))
  
  ; E-AssignSplit
  (test-->> stats
            (term (((ref 1) (ref 2)) = (1 2)))
            (term ((((ref 2)) = (2)) (((ref 1)) = (1)))))
  
  ; E-LocalDiscardValues
  (test-->> stats
            (term (local (X) = (1 2) in \; end))
            (term (local (X) = (1) in \; end)))
  
  ; E-LocalCompleteValues
  (test-->> stats
            (term (local (X Y) = (1) in \; end))
            (term (local (X Y) = (1 nil) in \; end)))
  
  ; Break
  (test-->> stats
            (term ((\;)Break))
            (term \;))
  
  (test-->> stats
            (term ((break)Break))
            (term \;))

  (test-results))

(provide stats-test-suite)

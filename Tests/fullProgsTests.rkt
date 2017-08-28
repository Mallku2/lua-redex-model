#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/fullProgs.rkt")

(define (full-progs-rel-test-suite)
  ; Full "while" loop
  (test-->> full-progs-rel
            (term ((((ref 1) 2))
                   : ()
                   : (while (1 < (ref 1)) do
                             (((ref 1)) = (((ref 1) - 1)))
                             end)))
            
            (term ((((ref 1) 1.0)) 
                   : () : \;)))
  
  (test-->> full-progs-rel
            (term ((((ref 1) 0)) 
                   : () : (while ((ref 1) < 2) do (((ref 1)) = (((ref 1) + 1))) end)))
            
            (term ((((ref 1) 2.0)) 
                   : () : \;)))
  
  ; Full while loop + short-circuit evaluation operator in the guard
  (test-->> full-progs-rel
            (term ((((ref 1) 0)
                    ((ref 2) true)) 
                   : () : (while (((ref 1) < 1) and (ref 2)) 
                                   do (((ref 1)) = (((ref 1) + 1))) end)))
            
            (term ((((ref 1) 1.0)
                    ((ref 2) true)) 
                   : () : \;)))
  
  ; While loop + break
  (test-->> full-progs-rel
            (term ((((ref 1) 0)) 
                   : () : (while ((ref 1) < 2) 
                                   do ((((ref 1)) = (((ref 1) + 1)))
                                       break) end)))
            
            (term ((((ref 1) 1.0)) 
                   : () : \;)))
  
  ; Function definition + function call
  (test-->> full-progs-rel
            (term ((((ref 1) nil)) 
                   : () 
                   : ((((ref 1)) = ((function X () (return (< 1 >)) end)))
                      (if ((ref 1) ()) then 
                               (((ref 1)) = (1)) 
                               else (((ref 1)) = (2)) 
                               end))))
            (term ((((ref 1) 1)) 
                   : (((objr 6) (function X () (return (< 1 >))end))) 
                   : \;)))

  ; Vararg function definition + function call
  (test-->> full-progs-rel
            (term ((((ref 1) nil)) 
                   : () 
                   : ( (((ref 1)) = ((function X (<<<) (return <<<) end)))
                        (if ((ref 1) (1 2 3)) then 
                               (((ref 1)) = (1))
                               else (((ref 1)) = (2)) 
                               end))))
            
            (term ((((ref 1) 1)) 
                   : (((objr 6) (function X (<<<) (return <<<) end))) 
                   : \;)))

  ; From http://www.luafaq.org/: function with a modified _ENV as upvalue
  (test-->> full-progs-rel

            (term (() : () :  (local (t) = ((\{ (\[ "x" \] = 10.000000 ) 
                                    (\[ "y" \] = 20.000000 ) \})) in 
                        (local (f1) = (nil) in 
                            (do 
                                (local (ENV) = (t) in 
                                    (((f1) = ((function $func1 ( )
                                                    (return (< ((ENV \[ "x" \]) + (ENV \[ "y" \])) >))
                                            end)))
                                     (((ENV \[ "z" \])) = ((f1 ()))))
                                end) 
                            end)
                        end)
                    end)))

            (term ((((ref 1) (objr 6))
                    ((ref 2) (objr 7))
                    ((ref 3) (objr 6)))
                   :
                   (((objr 6) ((|{| (|[| "z" |]| = 30.0)
                                    (|[| "x" |]| = 10.0)
                                    (|[| "y" |]| = 20.0) |}|) nil))
                    ((objr 7) (function $func1 ()
                                            (return (< (((ref 3) |[| "x" |]|)
                                                            +
                                                            ((ref 3) |[| "y" |]|))
                                                           >))
                                        end)))
                   :
                   |;|)))

  ; Propagation of an error message.
  (test-->> full-progs-rel
            (term ((((ref 1) nil)) 
                   : () 
                   : ((((ref 1)) = ((function X (<<<) (return (< ($builtIn error ("error message")) >))
                                               end)))
                      (if ((ref 1) (1 2 3)) then
                          (((ref 1)) = (1))
                          else (((ref 1)) = (2))
                          end))))

            (term ((((ref 1) (objr 6))) 
                   : (((objr 6) (function X (<<<)
                                          (return  (< ($builtIn error ("error message")) >)) end))) 
                   : ($err "error message")))

            )

  ; Errors
  (test-->> full-progs-rel
            (term (() : () : ((($builtIn error ("error")))Return)))
            (term (() : () : ($err "error"))))
  
  (test-->> full-progs-rel
            (term (() : () : (do ($builtIn error ("error")) end)))
            (term (() : () : ($err "error"))))
  
  (test-->> full-progs-rel
            (term (() : () : (if ($builtIn error ("error")) then \; else \; end)))
            (term (() : () : ($err "error"))))
  
  (test-->> full-progs-rel
            (term (() : () : (local (X) = (($builtIn error ("error"))) in \; end)))
            (term (() : () : ($err "error"))))
  
  (test-->> full-progs-rel
            (term (() : () : ($builtIn error ("error"))))
            (term (() : () : ($err "error"))))

  ; Protected Mode
  (test-->> full-progs-rel
            (term (() : () : ((($builtIn error ("error")))ProtectedMode)))
            (term (() : () : \;)))
  
  (test-->> full-progs-rel
            (term (() : () : (((< 1 >))ProtectedMode)))
            (term (() : () : \;)))
  
  (test-->> full-progs-rel
            (term (() : () : ((\;)ProtectedMode)))
            (term (() : () : \;)))

  (test-->> full-progs-rel
            (term (() : (((objr 1) (function $1 (m) (local (a) = ((m .. " received")) in \; end) end))) :
                      ((($err "error"))ProtectedMode (objr 1))))
            (term ((((ref 1) "error")
                    ((ref 2) "error received"))
                   : (((objr 1) (function $1 (m) (local (a) = ((m .. " received")) in \; end) end))) :
                                       \;)))
  
  (test-->> full-progs-rel
            (term (() : (((objr 1) (function $1 (m) (local (a) = ((m .. " received")) in \; end) end))) :
                      (((< 1 >))ProtectedMode (objr 1))))
            (term (() : (((objr 1) (function $1 (m) (local (a) = ((m .. " received")) in \; end) end))) :
                      \;)))
  
  (test-->> full-progs-rel
            (term (() : (((objr 1) (function $1 (m) (local (a) = ((m .. " received")) in \; end) end))) :
                      ((\;)ProtectedMode (objr 1))))
            (term (() : (((objr 1) (function $1 (m) (local (a) = ((m .. " received")) in \; end) end))) :
                      \;)))
  

  (test-results))

(provide full-progs-rel-test-suite)

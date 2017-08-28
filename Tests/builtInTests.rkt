#lang racket
(require redex
         "../grammar.rkt"
         "../Relations/builtIn.rkt")

(define (built-in-test-suite)
  ; Testing just the interface between the $builtIn form and the delta function.
  
  ;                                                                                                                          
  ;   ;                         ;                        ;;                                     ;                            
  ;   ;                                                 ;                                                                    
  ;   ;                                                 ;                               ;                                    
  ;   ;                                                 ;                               ;                                    
  ;   ;;;;;     ;;;    ;;;;   ;;;       ;;;           ;;;;;   ;    ;  ; ;;;     ;;;   ;;;;;;  ;;;      ;;;;   ; ;;;    ;;;;  
  ;   ;;  ;;   ;   ;  ;    ;    ;      ;   ;            ;     ;    ;  ;;   ;   ;   ;    ;       ;     ;;  ;;  ;;   ;  ;    ; 
  ;   ;    ;       ;  ;         ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;  ;      
  ;   ;    ;   ;;;;;   ;;;;     ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;   ;;;;  
  ;   ;    ;  ;    ;       ;    ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;       ; 
  ;   ;;  ;;  ;   ;;  ;    ;    ;      ;   ;            ;     ;   ;;  ;    ;   ;   ;    ;       ;     ;;  ;;  ;    ;  ;    ; 
  ;   ;;;;;    ;;; ;   ;;;;   ;;;;;     ;;;             ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;    ;;;;   ;    ;   ;;;;  
  ;                                                                                                                          
  ;                                                                                                                          
  ;                                                                                                                          

  ; error
  (test-->> built-in
            (term (() : ($builtIn error ("error message"))))
            (term (() : (< ($err "error message") >))))
  
  ; assert
  (test-->> built-in
            (term (() : ($builtIn assert (false nil))))
            
            (term (() : (< ($err "assertion failed!") >))))
  
  (test-->> built-in
            (term (() : ($builtIn assert (false "this assertion is false"))))
            
            (term (() : (< ($err "this assertion is false") >))))
  
  (test-->> built-in
            (term (() : ($builtIn assert (nil nil))))
            
            (term (() : (< ($err "assertion failed!") >))))
  
  (test-->> built-in
            (term (() : ($builtIn assert (true "this assertion is true"))))
            
            (term (() : (< (< true "this assertion is true" >) >))))
  
  (test-->> built-in
            (term (() : ($builtIn assert (1 "this assertion is true"))))
            
            (term (() : (< (< 1 "this assertion is true" >) >))))
  
  ; table.pack
  (test-->> built-in
            (term (() : ($builtIn table.pack ())))
            
            (term (() : (< (\{ (\[ "n" \] = 0) \}) >))))
  
  (test-->> built-in
            (term (() : ($builtIn table.pack (1))))
            
            (term (() : (< (\{ (\[ 1 \] = 1) (\[ "n" \] = 1) \}) >))))
  
  (test-->> built-in
            (term (() : ($builtIn table.pack (1 2))))
            
            (term (() : (< (\{ (\[ 1 \] = 1) (\[ 2 \] = 2) (\[ "n" \] = 2) \}) >))))
  
  ; pcall
  (test-->> built-in
            (term (() : ($builtIn pcall ((objr 1) 1 2))))
            
            (term (() : (< ((((objr 1) (1 2)))ProtectedMode) >))))
  
  ; tonumber
  (test-->> built-in
            (term (() : ($builtIn tonumber (1 10))))
            
            (term (() : (< 1 >))))
  
  (test-->> built-in
            (term (() : ($builtIn tonumber ("1" 10))))
            
            (term (() : (< 1 >))))
  
  (test-->> built-in
            (term (() : ($builtIn tonumber ("0x1" 10))))
            
            (term (() : (< nil >))))
  
  ; select
  (test-->> built-in
            (term (() : ($builtIn select ("#" 1 2 3))))
            
            (term (() : (< (< 3 >) >))))
  
  (test-->> built-in
            (term (() : ($builtIn select (1 1 2 3))))
            
            (term (() : (< (< 1 2 3 >) >))))
  
  (test-->> built-in
            (term (() : ($builtIn select (3 1 2 3))))
            
            (term (() : (< (< 3 >) >))))
  
  (test-->> built-in
            (term (() : ($builtIn select (-1 1 2 3))))
            
            (term (() : (< (< 3 >) >))))
  
  (test-->> built-in
            (term (() : ($builtIn select (-3 1 2 3))))
            
            (term (() : (< (< 1 2 3 >) >))))
  
  (test-->> built-in
            (term (() : ($builtIn select (-5 1 2 3))))
            
            (term (() : (< ($err "bad argument #1 to 'select' (index out of range)") >))))
  
  ; rawequal
  (test-->> built-in
            (term (() : ($builtIn rawequal (true false))))
            
            (term (() : (< false >))))
  
  (test-->> built-in
            (term (() : ($builtIn rawequal ((objr 1) 1))))
            
            (term (() : (< false >))))
  
  (test-->> built-in
            (term (() : ($builtIn rawequal (1 1))))
            
            (term (() : (< true >))))

  ; getmetatable
  (test-->> built-in
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ($builtIn getmetatable (1))))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : (< (objr 1) >))))
  
  (test-->> built-in
            (term (() : ($builtIn getmetatable (1))))
            
            (term (() : (< nil >))))
  
  ; type
  (test-->> built-in
            (term (() : ($builtIn type (1))))
            
            (term (() : (< "number" >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ($builtIn type ((objr 1)))))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : (< "table" >))))
  
  (test-->> built-in
            (term ((((objr 1) (function X () ((ref 1) ()) end))) 
                   : ($builtIn type ((objr 1)))))
            
            (term ((((objr 1) (function X () ((ref 1) ()) end))) 
                   : (< "function" >))))
  ; next
  (test-->> built-in
            (term ((((objr 1) ((\{ \}) nil))) 
                   : ($builtIn next ((objr 1) nil))))
            
            (term ((((objr 1) ((\{ \}) nil))) 
                   : (< (< nil >) >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \})
                               nil))) 
                   : ($builtIn next ((objr 1) 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \})
                               nil))) 
                   : (< (< 3 4 >) >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) 
                                   (\[ 3 \] = 4) \})
                               nil))) 
                   : ($builtIn next ((objr 1) 2))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) 
                                   (\[ 3 \] = 4) \})
                               nil))) 
                   : (< ($err "invalid key to 'next'") >)))) 
  
  ; rawget
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : ($builtIn rawget ((objr 1) 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : (< 2 >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : ($builtIn rawget ((objr 1) 2))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : (< nil >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : ($builtIn rawget (1 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : (< ($err  "bad argument #1 (table expected, got number)") >))))
  
  ; rawlen
  (test-->> built-in
            (term (() : ($builtIn rawlen ("asd"))))
            
            (term (() : (< 3 >))))
  
  ; rawset
  ; Alter field
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : ($builtIn rawset ((objr 1) 1 3))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 3) \}) nil))) 
                   : (< (objr 1) >))))
  ; Add new field
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : ($builtIn rawset ((objr 1) 2 3))))
            
            (term ((((objr 1) ((\{ (\[ 2 \] = 3) (\[ 1 \] = 2) \}) nil))) 
                   : (< (objr 1) >))))
  ; setmetatable
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))
                    ((objr 2) ((\{ \}) nil))) 
                   : ($builtIn setmetatable ((objr 1) (objr 2)))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) (objr 2)))
                    ((objr 2) ((\{ \}) nil))) 
                   : (< (objr 1) >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : ($builtIn setmetatable ((objr 1) 1))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) 
                   : (< ($err
                      "bad argument #2 to 'setmetatable' (nil or table expected)") >))))
  
  (test-->> built-in
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) (objr 2)))
                    ((objr 2) ((\{ (\[ "__metatable" \] = false) \}) nil))) 
                   : ($builtIn setmetatable ((objr 1) nil))))
            
            (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) (objr 2)))
                    ((objr 2) ((\{ (\[ "__metatable" \] = false) \}) nil))) 
                   : (< ($err "cannot change a protected metatable") >))))

  ; tostring
  (test-->> built-in
            (term ((((objr 1) ((\{ \}) (objr 2))) ((objr 2) ((\{ \}) nil)))
                   : ($builtIn tostring ((objr 1)))))
            
            (term ((((objr 1) ((\{ \}) (objr 2))) ((objr 2) ((\{ \}) nil)))
                   : (< "table: (objr 1)" >))))

  (test-->> built-in
            (term ((((objr 1) ((\{ \}) (objr 2))) ((objr 2) ((\{ (\[ "__tostring" \] = "this ain't a function") \}) nil)))
                   : ($builtIn tostring ((objr 1)))))
            
            (term ((((objr 1) ((\{ \}) (objr 2))) ((objr 2) ((\{ (\[ "__tostring" \] = "this ain't a function") \}) nil)))
                   : (< ("this ain't a function" ((objr 1))) >))))

  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;                     ;     ;      
  ;                     ;     ;      
  ;   ;;;;;;;   ;;;   ;;;;;;  ; ;;;  
  ;   ;  ;  ;  ;   ;    ;     ;;   ; 
  ;   ;  ;  ;      ;    ;     ;    ; 
  ;   ;  ;  ;  ;;;;;    ;     ;    ; 
  ;   ;  ;  ; ;    ;    ;     ;    ; 
  ;   ;  ;  ; ;   ;;    ;     ;    ; 
  ;   ;  ;  ;  ;;; ;     ;;;  ;    ; 
  ;                                  
  ;                                  
  ;
  ; abs
  (test-->> built-in
            (term (()
                   : ($builtIn math.abs (-1))))
            
            (term (()
                   : (< 1 >))))

  ; ceil
  (test-->> built-in
            (term (()
                   : ($builtIn math.ceil (2.3))))
            
            (term (()
                   : (< 3.0 >))))

  ; cos
  (test-->> built-in
            (term (()
                   : ($builtIn math.cos (0))))
            
            (term (()
                   : (< 1 >))))

  ; cosh
  (test-->> built-in
            (term (()
                   : ($builtIn math.cosh (0))))
            
            (term (()
                   : (< 1.0 >))))

  ; deg
  (test-->> built-in
            (term (()
                   : ($builtIn math.deg (0))))
            
            (term (()
                   : (< 0 >))))

  ; exp
  (test-->> built-in
            (term (()
                   : ($builtIn math.exp (0))))
            
            (term (()
                   : (< 1 >))))

  ; fmod
  (test-->> built-in
            (term (()
                   : ($builtIn math.fmod (10 3))))
            
            (term (()
                   : (< 1 >))))

  ; log
  (test-->> built-in
            (term (()
                   : ($builtIn math.log (1 nil))))
            
            (term (()
                   : (< 0 >))))
  
  ; rad
  (test-->> built-in
            (term (()
                   : ($builtIn math.rad (0))))
            
            (term (()
                   : (< 0 >))))

  ; sin
  (test-->> built-in
            (term (()
                   : ($builtIn math.sin (0))))
            
            (term (()
                   : (< 0 >))))

  ; sinh
  (test-->> built-in
            (term (()
                   : ($builtIn math.sinh (0))))
            
            (term (()
                   : (< 0 >))))

  ; sqrt
  (test-->> built-in
            (term (()
                   : ($builtIn math.sqrt (9))))
            
            (term (()
                   : (< 3 >))))

  ; tan
  (test-->> built-in
            (term (()
                   : ($builtIn math.tan (0))))
            
            (term (()
                   : (< 0 >))))

  ; tanh
  (test-->> built-in
            (term (()
                   : ($builtIn math.tanh (0))))
            
            (term (()
                   : (< 0 >))))

  (test-results))

(provide built-in-test-suite)

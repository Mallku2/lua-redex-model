#lang racket
(require redex
         "./grammar.rkt"
         "./Relations/fullProgs.rkt")

; Definition of the execution environment as an evaluation context.
(define-metafunction core-lang
  [(plugIntoExecutionEnvironment s)
   (in-hole 
    (()
     :
     ()
     : (((local (~ENV) = ((\{ \})) 
         in (; Basic functions
             ($builtIn rawset (~ENV
                               "assert"
                               (function $assert (v message <<<)
                                         (return ($builtIn assert (v message <<<)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "error"
                               (function $error (message level) 
                                         (return ($builtIn error (message level)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "getmetatable"
                               (function $getmetatable (value)
                                         (return ($builtIn getmetatable (value)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "load"
                               (function $load (ld source mode env)
                                         (return ($builtIn load (ld source mode env)))
                                         end)
                               )
                       )

             ($builtIn rawset (~ENV
                               "loadfile"
                               (function $loadfile (filename mode env)
                                         (return ($builtIn loadfile (filename mode env)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "ipairs"
                               (function $ipairs (value)
                                         (return ($builtIn ipairs (value))) 
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "next"
                               (function $next (table index) 
                                         (return ($builtIn next (table index)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "pairs"
                               (function $pairs (value)
                                         (return ($builtIn pairs (value))) 
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "pcall"
                               (function $pcall (v <<<)
                                         (return ($builtIn pcall (v <<<)))
                                         end)
                               )
                       )

             ($builtIn rawset (~ENV
                               "print"
                               (function $print (<<<)
                                         \;
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "rawequal"
                               (function $rawequal (v1 v2)
                                         (return ($builtIn rawequal (v1 v2))) 
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "rawget"
                               (function $rawget (table index)
                                         (return ($builtIn rawget (table index))) 
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "rawset"
                               (function $rawset (table index value)
                                         (return ($builtIn rawset (table index value)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "rawlen"
                               (function $rawlen (v)
                                         (return ($builtIn rawlen (v)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "select"
                               (function $select (index <<<)
                                         (return ($builtIn select (index <<<))) 
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "setmetatable"
                               (function $setmetatable (table metatable)
                                         (return ($builtIn setmetatable (table metatable)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "tonumber"
                               (function $tonumber (e base)
                                         (return ($builtIn tonumber (e base)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "tostring"
                               (function $tostring (v)
                                         (return ($builtIn tostring (v)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "type"
                               (function $type (value)
                                         (return ($builtIn type (value)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset (~ENV
                               "~G"
                               ~ENV
                               )
                       )
                         
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
                         
             ($builtIn rawset (~ENV
                               "math"
                               (\{ \})
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "abs"
                               (function $mathAbs (x)
                                         (return ($builtIn math.abs (x)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "acos"
                               (function $mathAcos (x)
                                         (return ($builtIn math.acos (x)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "asin"
                               (function $mathAsin (x)
                                         (return ($builtIn math.asin (x)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "atan"
                               (function $mathAtan (x)
                                         (return ($builtIn math.atan (x)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "ceil"
                               (function $mathCeil (x)
                                         (return ($builtIn math.ceil (x)))
                                         end)
                               )
                       )
             
             ($builtIn rawset ((~ENV \[ "math" \])
                               "cos"
                               (function $mathCos (rad)
                                         (return ($builtIn math.cos (rad)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "cosh"
                               (function $mathCosh (rad)
                                         (return ($builtIn math.cosh (rad)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "deg"
                               (function $mathDeg (rad)
                                         (return ($builtIn math.deg (rad)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "exp"
                               (function $mathExp (x)
                                         (return ($builtIn math.exp (x)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset ((~ENV \[ "math" \])
                               "floor"
                               (function $mathFloor (x)
                                         (return ($builtIn math.floor (x)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset ((~ENV \[ "math" \])
                               "fmod"
                               (function $mathFmod (x y)
                                         (return ($builtIn math.fmod (x y)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "huge"
                               +inf.0
                               )
                       )

             
             ($builtIn rawset ((~ENV \[ "math" \])
                               "log"
                               (function $mathLog (x base)
                                         (return ($builtIn math.log (x base)))
                                         end)
                               )
                       )            
             ($builtIn rawset ((~ENV \[ "math" \])
                               "max"
                               (function $mathMax (<<<)
                                         (return ($builtIn math.max (<<<)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "modf"
                               (function $mathModf (x)
                                         (return ($builtIn math.modf (x)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "pi"
                               ,pi
                               )
                       )
                         
             ($builtIn rawset ((~ENV \[ "math" \])
                               "rad"
                               (function $mathRad (deg)
                                         (return ($builtIn math.rad (deg)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "sin"
                               (function $mathSin (rad)
                                         (return ($builtIn math.sin (rad)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "sinh"
                               (function $mathSinh (rad)
                                         (return ($builtIn math.sinh (rad)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "sqrt"
                               (function $mathSqrt (x)
                                         (return ($builtIn math.sqrt (x)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "tan"
                               (function $mathTan (rad)
                                         (return ($builtIn math.tan (rad)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "math" \])
                               "tanh"
                               (function $mathTanh (rad)
                                         (return ($builtIn math.tanh (rad)))
                                         end)
                               )
                       )
                         
                         
             ;                                                          
             ;                           ;                              
             ;                           ;                              
             ;                           ;                              
             ;                           ;                              
             ;   ;;;;;     ;;;     ;;;   ;   ;     ;;;    ;;;;;   ;;;;  
             ;   ;;  ;;   ;   ;   ;   ;  ;  ;     ;   ;  ;;  ;;  ;;  ;; 
             ;   ;    ;       ;  ;       ; ;          ;  ;    ;  ;    ; 
             ;   ;    ;   ;;;;;  ;       ;;;      ;;;;;  ;    ;  ;;;;;; 
             ;   ;    ;  ;    ;  ;       ;  ;    ;    ;  ;    ;  ;      
             ;   ;;  ;;  ;   ;;   ;   ;  ;   ;   ;   ;;  ;;  ;;  ;;   ; 
             ;   ;;;;;    ;;; ;    ;;;   ;    ;   ;;; ;   ;;; ;   ;;;;  
             ;   ;                                            ;         
             ;   ;                                        ;   ;         
             ;   ;                                         ;;;          
                         
             ($builtIn rawset (~ENV
                               "require"
                               (function $require (modname)
                                         (return ($builtIn require (modname)))
                                         end)
                               )
                       )
                         
             ;                                                  
             ;                             ;                    
             ;                                                  
             ;             ;                                    
             ;             ;                                    
             ;    ;;;;   ;;;;;;   ;;;;   ;;;     ; ;;;    ;;;;; 
             ;   ;    ;    ;      ;;  ;    ;     ;;   ;  ;;  ;; 
             ;   ;         ;      ;        ;     ;    ;  ;    ; 
             ;    ;;;;     ;      ;        ;     ;    ;  ;    ; 
             ;        ;    ;      ;        ;     ;    ;  ;    ; 
             ;   ;    ;    ;      ;        ;     ;    ;  ;;  ;; 
             ;    ;;;;      ;;;   ;      ;;;;;   ;    ;   ;;; ; 
             ;                                                ; 
             ;                                            ;   ; 
             ;                                             ;;;  
                         
             ($builtIn rawset (~ENV
                               "string"
                               (\{ \})
                               )
                       )
                         
             ($builtIn rawset ((~ENV \[ "string" \])
                               "dump"
                               (function $stringDump (func)
                                         (return ($builtIn string.dump (func)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "string" \])
                               "len"
                               (function $stringLen (string)
                                         (return ($builtIn string.len (string)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "string" \])
                               "rep"
                               (function $stringRep (string n sep)
                                         (return ($builtIn string.rep (string n sep)))
                                         end)
                               )
                       )

             ($builtIn rawset ((~ENV \[ "string" \])
                               "reverse"
                               (function $stringReverse (string)
                                         (return ($builtIn string.reverse (string)))
                                         end)
                               )
                       )

                         
             ($builtIn rawset ((~ENV \[ "string" \])
                               "sub"
                               (function $stringSub (string i j)
                                         (return ($builtIn string.sub (string i j)))
                                         end)
                               )
                       )
                         
             ;                                          
             ;                   ;       ;;;            
             ;                   ;         ;            
             ;     ;             ;         ;            
             ;     ;             ;         ;            
             ;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
             ;     ;      ;   ;  ;;  ;;    ;     ;;  ;; 
             ;     ;          ;  ;    ;    ;     ;    ; 
             ;     ;      ;;;;;  ;    ;    ;     ;;;;;; 
             ;     ;     ;    ;  ;    ;    ;     ;      
             ;     ;     ;   ;;  ;;  ;;    ;     ;;   ; 
             ;      ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
             ;                                          
             ;                                          
             ;                                          
                         
             ($builtIn rawset (~ENV
                               "table"
                               (\{ \})
                               )
                       )

             ($builtIn rawset ((~ENV \[ "table" \])
                               "concat"
                               (function $tableConcat (list sep i j)
                                         (return ($builtIn table.concat (list sep i j)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset ((~ENV \[ "table" \])
                               "pack"
                               (function $tablePack (<<<)
                                         (return ($builtIn table.pack (<<<)))
                                         end)
                               )
                       )
                         
             ($builtIn rawset ((~ENV \[ "table" \])
                               "unpack"
                               (function $tableUnpack (list i j)
                                         (return ($builtIn table.unpack (list i j)))
                                         end)
                               )
                       )
                         
             ; NOTE: this is done in order to manage, in a simple way, the cases where
             ; we plug into the hole a concatenation of statements, and not some other
             ; kind of statement. This could lead to some errors, like when the concatenated
             ; statements are function calls. An the resulting phrase could be incorrected
             ; interpreted as a function call: the previous assignment to the ~ENV table
             ; result in a tuple value, and the whole phrase could be seen as a function call.
             (local ($dummyVar) = (nil) in hole end)
             )
         end))Return) ; Lua handles a chunk as the body of an anonymous function with a variable number of arguments
     )
    
    s)]
  )

(provide plugIntoExecutionEnvironment)

; Empty execution environment for easy testing of...ideas.
(define-metafunction core-lang
  [(plugIntoEmptyExecEnv s)
   (in-hole 
    (() : () : hole)
    s)]
  )

(provide plugIntoEmptyExecEnv)
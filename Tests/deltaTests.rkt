#lang racket
(require redex
         math/base
         "../grammar.rkt"
         "../Meta-functions/delta.rkt")

(define (delta-test-suite)
  ; Arithmetic operations
  (test-equal (term (δ (+ 1 1.1)))
              2.1)
  
  (test-equal (term (δ (- 1 1)))
              0.0)
  
  (test-equal (term (δ (* 2.5 2)))
              5.0)
  
  (test-equal (term (δ (/ 1 2)))
              0.5)
  
  (test-equal (term (δ (/ 0 0)))
              (term ,+nan.0))
  
  (test-equal (term (δ (/ 1 0)))
              (term ,+inf.0))
  
  (test-equal (term (δ (^ 1 2.1)))
              1.0)
  
  (test-equal (term (δ (% 1 2)))
              1.0)
  
  (test-equal (term (δ (% 1 0)))
              (term $nan))

  (test-equal (term (δ (% 4 -3)))
              (term -2.0))

  (test-equal (term (δ (- ,pi (δ (% ,pi 1)))))
              (term 3.0))
  
  (test-equal (term (δ (- 1)))
              -1)
  
  ; Number comparison
  (test-equal (term (δ (< 1 2)))
              (term true))
  
  (test-equal (term (δ (== 1 1)))
              (term true))
  
  (test-equal (term (δ (<= 1 1)))
              (term true))
  
  ; String comparison
  (test-equal (term (δ (< "a" "b")))
              (term true))
  
  (test-equal (term (δ (== "a" "a")))
              (term true))
  
  (test-equal (term (δ (<= "a" "a")))
              (term true))
  
  ; String concatenation
  (test-equal (term (δ (.. "a" "b")))
              "ab")
  
  ; String length
  (test-equal (term (δ (\# "abc")))
              3)
  
  (test-equal (term (δ (\# "abcñ")))
              5)
  
  (test-equal (term (δ (\# "abcá")))
              5)
  
  ; Table length
  
  (test-equal (term (δ (\# (\{ (\[ 1 \] = 1) \}))))
              1)
  
  (test-equal (term (δ (\# (\{ (\[ 1 \] = 1) (\[ "a" \] = 1) (\[ 2 \] = 1) 
                               \}))))
              2)
  
  (test-equal (term (δ (\# (\{ (\[ -1 \] = 1) (\[ "a" \] = 1) (\[ 1 \] = 1) 
                               \}))))
              1)
  
  ; Equality comparison
  (test-equal (term (δ (== (objr 1) (objr 1))))
              (term true))
  
  (test-equal (term (δ (== true true)))
              (term true))
  
  ; Logical connectives
  (test-equal (term (δ (and true (\{ \}))))
              (term (\( (\{ \}) \))))
  
  (test-equal (term (δ (and (objr 1) nil)))
              (term (\( nil \))))
  
  (test-equal (term (δ (and (objr 1) (objr 1))))
              (term (\( (objr 1) \))))
  
  (test-equal (term (δ (or true true)))
              (term true))
  
  (test-equal (term (δ (or nil nil)))
              (term (\( nil \))))
  
  (test-equal (term (δ (or nil (objr 1))))
              (term (\( (objr 1) \))))
  
  (test-equal (term (δ (not true)))
              (term false))
  
  (test-equal (term (δ (not nil)))
              (term true))
  
  (test-equal (term (δ (not (objr 1))))
              (term false))
  
  ; getmetatable
  (test-equal (term (δ (getmetatable 1 (((objr 6)
                                         (function X () ((ref 1) ()) end))))))
              (term nil))
  
  (test-equal (term (δ (getmetatable 1 (((objr 1) ((\{ \}) nil))))))
              (term (objr 1)))
  
  (test-equal (term (δ (getmetatable
                        (objr 1)
                        (((objr 1) (function X () ((ref 1) ()) end))))))
              (term nil))
  
  (test-equal (term (δ (getmetatable (objr 1)
                                     (((objr 1) ((\{ \}) (objr 2)))
                                      ((objr 2) ((\{ \}) nil))))))
              (term (objr 2)))
  
  (test-equal (term (δ (getmetatable (objr 1)
                                     (((objr 1) ((\{ \}) (objr 2)))
                                      ((objr 2) ((\{ (\[ "__metatable" \] = 1) \}) nil))))))
              (term 1))
  
  ; load
  (test-equal (term (δ (load "do end" nil nil nil ())))
              (term (function $loaded (<<<) (do \; end) end)))
  
  (test-equal (term (δ (load "a = 1" nil nil nil ())))
              (term (function $loaded (<<<) ((((ref 1) \[ "a" \])) = (1.0)) end)))
  
  (test-equal (term (δ (load "local a = 1; a = 2" nil nil nil ())))
              (term (function $loaded (<<<) (local (a) = (1.0) in (\; \; ((a) = (2.0))) end) end)))

  (test-equal (term (δ (load "local a" nil nil (objr 1) ())))
              (term (function $loaded (<<<) (local ($old_env) = ((ref 1))
                                              in
                                              ((((ref 1)) = ((objr 1)))
                                               (local (a) = (nil) in \; end)
                                               (((ref 1)) = ($old_env)))
                                               end)
                                              end)))
  
  ; Ill-formed program's string
  (test-equal (term (δ (load "a=" nil nil nil ())))
              (term (< nil "[string a=]">)))
  
  ; NOTE: no significant test for load(f) can be done here, with f a function.
  
  ; rawequal
  (test-equal (term (δ (rawequal 1 1)))
              (term true))
  
  (test-equal (term (δ (rawequal (objr 1) (objr 2))))
              (term false))
  
  ; rawget
  (test-equal (term (δ (rawget (objr 1)
                               1
                               (((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))))))
              (term 2))
  
  (test-equal (term (δ (rawget (objr 1)
                               1
                               (((objr 1) ((\{ \}) nil))))))
              (term nil))
  
  (test-equal (term (δ (rawget (objr 1)
                               3
                               (((objr 1) ((\{ (\[ 1 \] = 2)
                                               (\[ 3 \] = 4)
                                               (\[ 5 \] = 6)
                                               \}) nil))))))
              (term 4))
  
  (test-equal (term (δ (rawget (objr 1)
                               3.0
                               (((objr 1) ((\{ (\[ 1 \] = 2)
                                               (\[ 3 \] = 4)
                                               (\[ 5 \] = 6)
                                               \}) nil))))))
              (term 4))
  
  (test-equal (term (δ (rawget (objr 1)
                               7
                               (((objr 1) ((\{ (\[ 1 \] = 2)
                                               (\[ 3 \] = 4)
                                               (\[ 5 \] = 6)
                                               \}) nil))))))
              (term nil))
  
  (test-equal (term (δ (rawget (objr 1)
                               true
                               (((objr 1) ((\{ (\[ true \] = 2) \}) nil))))))
              (term 2))
  
  ; rawset
  (test-equal (term (δ (rawset (objr 1) 1 2 (((objr 1) ((\{ \}) nil))))))
              (term ((((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))) (objr 1))))
  
  (test-equal (term (δ (rawset (objr 1) 3 5
                               (((objr 1)
                                 ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \}) nil))))))
              
              (term ((((objr 1) ((\{ (\[ 1 \] = 2) (\[ 3 \] = 5) \}) nil)))
                     (objr 1))))
  
  (test-equal (term (δ (rawset (objr 1) 3.0 5
                               (((objr 1)
                                 ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \}) nil))))))
              
              (term ((((objr 1) ((\{ (\[ 1 \] = 2) (\[ 3 \] = 5) \}) nil)))
                     (objr 1))))
  
  (test-equal (term (δ (rawset (objr 1) 1 nil
                               (((objr 1) ((\{ (\[ 1 \] = 2) \}) nil))))))
              (term ((((objr 1) ((\{ \}) nil)))
                     (objr 1))))

  ; tonumber
  (test-equal (term (δ (tonumber "6" nil)))
              6.0)

  (test-equal (term (δ (tonumber " 6 " nil)))
              6.0)
  
  ; tostring
  (test-equal (term (δ (tostring (objr 1) (((objr 1) ((\{ \}) (objr 2))) ((objr 2) ((\{ \}) nil))))))
              "table: (objr 1)")
  
  (test-equal (term (δ (tostring (objr 1) (((objr 1) ((\{ \}) (objr 2)))
                                           ((objr 2) ((\{ (\[ "__tostring" \] = "this ain't a function") \}) nil))))))
              (term ("this ain't a function" ((objr 1)))))
  
  (test-equal (term (δ (tostring (objr 1) (((objr 1) ((\{ \}) (objr 2))) ((objr 2) ((\{ \}) nil))))))
              "table: (objr 1)")
  
  (test-equal (term (δ (tostring 1.0 ())))
              "1")
  
  (test-equal (term (δ (tostring 1.1 ())))
              "1.1")
  
  ; type
  (test-equal (term (δ (type (objr 1) (((objr 1) ((\{ \}) nil))))))
              (term "table"))
  
  (test-equal (term (δ (type (objr 1) (((objr 1) (function X () \; end))))))
              (term "function"))
  
  (test-equal (term (δ (type 1 ())))
              (term "number"))
  
  (test-equal (term (δ (type 1.1 ())))
              (term "number"))
  
  (test-equal (term (δ (type "asd" ())))
              (term "string"))
  
  (test-equal (term (δ (type true ())))
              (term "boolean"))
  
  (test-equal (term (δ (type false ())))
              (term "boolean"))
  
  (test-equal (term (δ (type nil ())))
              (term "nil"))
  
  ; assert
  (test-equal (term (δ (assert 1 nil)))
              (term (< 1 nil >)))
  
  (test-equal (term (δ (assert 1 "error-message?")))
              (term (< 1 "error-message?" >)))
  
  (test-equal (term (δ (assert nil "error-message?")))
              (term ($err "error-message?")))
  
  (test-equal (term (δ (assert nil nil)))
              (term ($err "assertion failed!")))
  
  ; next
  (test-equal (term (δ (next 1 2 (((objr 1)
                                   ((\{ (\[ 1 \] = 2)
                                        (\[ 3 \] = 4) \}) nil))))))
              (term ($err
                     "bad argument #1 to 'next' (table expected, got number)")))
  
  (test-equal (term (δ (next (objr 1) nil (((objr 1) ((\{ (\[ 1 \] = 2)
                                                          (\[ 3 \] = 4)
                                                          \}) nil))))))
              (term (< 1 2 >)))
  
  (test-equal (term (δ (next (objr 1) 5 (((objr 1) ((\{ (\[ 1 \] = 2)
                                                        (\[ 3 \] = 4)
                                                        \}) nil))))))
              (term ($err
                     "invalid key to 'next'")))
  
  (test-equal (term (δ (next (objr 1) nil (((objr 1) ((\{ \}) nil))))))
              (term (< nil >)))
  
  (test-equal (term (δ (next (objr 1) 1 (((objr 1)
                                          ((\{ (\[ 1 \] = 2)
                                               (\[ 3 \] = 4) \}) nil))))))
              (term (< 3 4 >)))
  
  (test-equal (term (δ (next (objr 1) 3 (((objr 1)
                                          ((\{ (\[ 1 \] = 2)
                                               (\[ 3 \] = 4) \}) nil))))))
              (term (< nil >)))
  
  ; pcall
  (test-equal (term (δ (pcall (objr 1) 1 2)))
              (term ((((objr 1) (1 2)))ProtectedMode)))
  
  ; select
  (test-equal (term (δ (select -3 1 2)))
              (term ($err "bad argument #1 to 'select' (index out of range)")))
  
  (test-equal (term (δ (select 1 1 2)))
              (term (< 1 2 >)))
  
  (test-equal (term (δ (select 3 1 2)))
              (term (< >)))
  
  (test-equal (term (δ (select -1 1 2)))
              (term (< 2 >)))
  
  (test-equal (term (δ (select "#" 1 2)))
              (term (< 2 >)))
  
  (test-equal (term (δ (select true 1 2)))
              (term ($err "bad argument #1 to 'select' (number expected)")))
  
  ; setmetatable
  (test-equal (term (δ (setmetatable 1 1 ())))
              (term (()
                     ($err
                      "bad argument #1 to 'setmetatable' (table expected, got number)"))))
  
  (test-equal (term (δ (setmetatable (objr 1) 1 (((objr 1) ((\{ \}) nil))))))
              (term ((((objr 1) ((\{ \}) nil)))
                     ($err
                      "bad argument #2 to 'setmetatable' (nil or table expected)"))))
  
  (test-equal (term (δ (setmetatable (objr 1)
                                     (objr 3)
                                     (((objr 1) ((\{ \}) (objr 2)))
                                      ((objr 2) ((\{
                                                  (\[ "__metatable" \] = 1)
                                                  \}) nil))
                                      ((objr 3) ((\{ \}) nil))))))
              
              (term ((((objr 1) ((\{ \}) (objr 2)))
                      ((objr 2) ((\{ (\[ "__metatable" \] = 1) \}) nil))
                      ((objr 3) ((\{ \}) nil)))
                     ($err "cannot change a protected metatable"))))
  
  (test-equal (term (δ (setmetatable (objr 1) (objr 3)
                                     (((objr 1) ((\{ \}) (objr 2)))
                                      ((objr 2) ((\{ \}) nil))
                                      ((objr 3) ((\{ (\[ 1 \] = 2) \})
                                                 nil))))))
              
              (term ((((objr 1) ((\{ \}) (objr 3)))
                      ((objr 2) ((\{ \}) nil))
                      ((objr 3) ((\{ (\[ 1 \] = 2) \}) nil)))
                     (objr 1))))
  
  
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
  ; string.dump
  (test-equal (term (δ (string.dump (objr 1) (((objr 1) (function $1 () \; end))))))     
              (term "(function $1 () \\; end )"))
  
  (test-equal (term (δ (string.dump 1 (((objr 1) (function $1 () \; end))))))     
              (term ($err "bad argument #1 (function expected, got number)")))
  
  
                                    
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
  (test-equal (term (δ (math.abs -1)))     
              (term 1))

  (test-equal (term (δ (math.abs 1)))     
              (term 1))

  ; acos
  (test-equal (term (δ (math.acos 1)))     
              (term 0))
  
  (test-equal (term (δ (math.acos "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; asin
  (test-equal (term (δ (math.asin 0)))     
              (term 0))
  
  (test-equal (term (δ (math.asin "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; atan
  (test-equal (term (δ (math.atan 0)))     
              (term 0))
  
  (test-equal (term (δ (math.atan "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; ceil
  (test-equal (term (δ (math.ceil 2.34)))     
              (term 3.0))
  
  (test-equal (term (δ (math.ceil "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; cos
  (test-equal (term (δ (math.cos 0)))     
              (term 1))
  
  (test-equal (term (δ (math.cos "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; cosh
  (test-equal (term (δ (math.cosh 1)))     
              (term 1.5430806348152437))
  
  (test-equal (term (δ (math.cosh "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; deg
  (test-equal (term (δ (math.deg 0)))     
              (term 0))
  
  (test-equal (term (δ (math.deg "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; exp
  (test-equal (term (δ (math.exp 1)))     
              (term ,euler.0))
  
  (test-equal (term (δ (math.exp "0")))     
              (term ($err "bad argument #1 (number expected)")))
  
  ; floor
  (test-equal (term (δ (math.floor 1.8)))     
              (term 1.0))

  ; fmod
  (test-equal (term (δ (math.fmod 10 3)))     
              (term 1))

  ; log
  (test-equal (term (δ (math.log 1 nil)))     
              (term 0))

  (test-equal (term (δ (math.log 8 2)))     
              (term 3.0))
  
  ; max
  (test-equal (term (δ (math.max 1 2 8 4 5 6)))     
              (term 8.0))

  ; rad
  (test-equal (term (δ (math.rad 0)))     
              (term 0))
  
  (test-equal (term (δ (math.rad "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; sin
  (test-equal (term (δ (math.sin 0)))     
              (term 0))
  
  (test-equal (term (δ (math.sin "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; sinh
  (test-equal (term (δ (math.sinh 1)))     
              (term 1.1752011936438014))
  
  (test-equal (term (δ (math.sinh "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; sqrt
  (test-equal (term (δ (math.sqrt 9)))     
              (term 3))
  
  (test-equal (term (δ (math.sqrt "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; tan
  (test-equal (term (δ (math.tan 0)))     
              (term 0))
  
  (test-equal (term (δ (math.tan "0")))     
              (term ($err "bad argument #1 (number expected)")))

  ; tanh
  (test-equal (term (δ (math.tanh 1)))     
              (term 0.7615941559557649))
  
  (test-equal (term (δ (math.tanh "0")))     
              (term ($err "bad argument #1 (number expected)")))
  
  
  
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
  ; table.concat
  (test-equal (term (δ (table.concat (objr 1) "a" 1 1 (((objr 1) ((\{ (\[ 1 \] = "b") \}) nil))))))
              (term ((objr 1) \[ 1 \])))

  (test-equal (term (δ (table.concat (objr 1) "a" 1 2 (((objr 1) ((\{ (\[ 1 \] = "b")
                                                                      (\[ 2 \] = "b") \}) nil))))))
              (term (((objr 1) \[ 1 \]) .. ("a" .. ((objr 1) \[ 2 \])))))

  (test-equal (term (δ (table.concat (objr 1) "a" 1 3 (((objr 1) ((\{ (\[ 1 \] = "b")
                                                                      (\[ 2 \] = "b")
                                                                      (\[ 3 \] = "b")\}) nil))))))
              (term ((((objr 1) \[ 1 \]) .. ("a" .. ((objr 1) \[ 2 \]))) .. ("a" .. ((objr 1) \[ 3 \])))))

  (test-equal (term (δ (table.concat (objr 1) "" nil nil (((objr 1) ((\{ \}) nil))))))
              "")

  (test-equal (term (δ (table.concat (objr 1) nil nil nil (((objr 1) ((\{ \}) nil))))))
              "")
  
  ; table.pack
  (test-equal (term (δ (table.pack)))
              (term (\{ (\[ "n" \] = 0) \})))
  
  (test-equal (term (δ (table.pack 4 5 6)))
              (term (\{ (\[ 1 \] = 4)
                        (\[ 2 \] = 5)
                        (\[ 3 \] = 6)
                        (\[ "n" \] = 3) \})))
  
  (test-equal (term (δ (table.pack 1 nil 2)))
              (term (\{ (\[ 1 \] = 1)
                        (\[ 3 \] = 2)
                        (\[ "n" \] = 3) \})))
  
  ; table.unpack
  (test-equal (term (δ (table.unpack (objr 1) nil nil (((objr 1) ((\{ (\[ 1 \] = "a")
                                                                      (\[ 2 \] = "a")
                                                                      (\[ 3 \] = "a") \}) nil))))))
              (term (< ((objr 1) \[ 1 \]) ((objr 1) \[ 2 \]) ((objr 1) \[ 3 \]) >)))
  
  (test-equal (term (δ (table.unpack (objr 1) 2 nil (((objr 1) ((\{ (\[ 1 \] = "a")
                                                                    (\[ 2 \] = "a")
                                                                    (\[ 3 \] = "a") \}) nil))))))
              (term (< ((objr 1) \[ 2 \]) ((objr 1) \[ 3 \]) >)))
  
  (test-equal (term (δ (table.unpack (objr 1) 2 4 (((objr 1) ((\{ (\[ 1 \] = "a")
                                                                  (\[ 2 \] = "a")
                                                                  (\[ 3 \] = "a") \}) nil))))))
              (term (< ((objr 1) \[ 2 \]) ((objr 1) \[ 3 \]) ((objr 1) \[ 4 \]) >)))

  (test-equal (term (δ (table.unpack (objr 1) -1 4 (((objr 1) ((\{ (\[ 1 \] = "a")
                                                                  (\[ 2 \] = "a")
                                                                  (\[ 3 \] = "a") \}) nil))))))
              (term (< ((objr 1) \[ -1 \]) ((objr 1) \[ 0 \]) ((objr 1) \[ 1 \])
                       ((objr 1) \[ 2 \]) ((objr 1) \[ 3 \]) ((objr 1) \[ 4 \]) >)))

  ; testing details outside the reference manual
  (test-equal (term (δ (table.unpack (objr 1) 2.1 4 (((objr 1) ((\{ (\[ 1 \] = "a")
                                                                  (\[ 2 \] = "a")
                                                                  (\[ 3 \] = "a") \}) nil))))))
              (term (< ((objr 1) \[ 2 \]) ((objr 1) \[ 3 \]) ((objr 1) \[ 4 \]) >)))
  
  (test-equal (term (δ (table.unpack "table" nil nil ())))
              (term ($err "bad argument #1 (table expected, got string)")))

  (test-equal (term (δ (table.unpack (objr 1) "2" nil (((objr 1) ((\{ \}) nil))))))
              (term ($err "bad argument #2 (number expected, got string)")))

  (test-equal (term (δ (table.unpack (objr 1) 2 "3" (((objr 1) ((\{ \}) nil))))))
              (term ($err "bad argument #3 (number expected, got string)")))
  
  (test-results))

(provide delta-test-suite)

;                                                                                                          
;                                                                                                          
;                                                                                                          
;                                      ;                                         ;   ;;;                   
;                     ;     ;;;;;                   ;    ;                       ;     ;                   
;                     ;     ;    ;                  ;    ;                       ;     ;                   
;    ;;;;;;  ;;;;   ;;;;;;  ;    ;   ;;;    ; ;;;   ;    ;   ;;;;   ; ;;;    ;;; ;     ;     ;;;;    ; ;;; 
;   ;    ;  ;;  ;;    ;     ;   ;;     ;    ;;   ;  ;    ;       ;  ;;   ;  ;;  ;;     ;    ;;  ;;   ;;    
;   ;    ;  ;    ;    ;     ;;;;       ;    ;    ;  ;;;;;;       ;  ;    ;  ;    ;     ;    ;    ;   ;     
;   ;    ;  ;;;;;;    ;     ;   ;;     ;    ;    ;  ;    ;   ;;;;;  ;    ;  ;    ;     ;    ;;;;;;   ;     
;    ;;;;   ;         ;     ;    ;     ;    ;    ;  ;    ;  ;    ;  ;    ;  ;    ;     ;    ;        ;     
;   ;       ;;        ;     ;   ;;     ;    ;    ;  ;    ;  ;   ;;  ;    ;  ;;  ;;     ;    ;;       ;     
;    ;;;;;   ;;;;;     ;;;  ;;;;;    ;;;;;  ;    ;  ;    ;   ;;; ;  ;    ;   ;;; ;   ;;;;;   ;;;;;   ;     
;   ;     ;                                                                                                
;   ;     ;                                                                                                
;    ;;;;;                                                                                                 
;                                                                                                          
(define (getBinHandler-test-suite)
  (test-equal (term (getBinHandler 1 2 "__add" 
                                   (((objr 1) ((\{
                                                (\[ "__add" \] = (objr 2))
                                                \}) nil))
                                    ((objr 2) (function X () ((ref 1) ()) end)))))
              (term (objr 2)))
  
  (test-equal (term (getBinHandler true 2 "__add" 
                                   (((objr 1) ((\{ (\[ "__add" \] = (objr 2)) \})
                                               nil))
                                    ((objr 2) (function X () ((ref 1) ()) end)))))
              (term (objr 2)))
  
  (test-equal (term (getBinHandler true false "__add" 
                                   ()))
              (term nil))
  
  (test-results))


;                                                                                                                          
;                                                                                                                          
;                                                                                                                          
;                                                            ;;;                                 ;   ;;;                   
;                     ;     ;;;;;;                             ;    ;    ;                       ;     ;                   
;                     ;     ;                                  ;    ;    ;                       ;     ;                   
;    ;;;;;;  ;;;;   ;;;;;;  ;        ;;; ;  ;    ;   ;;;;      ;    ;    ;   ;;;;   ; ;;;    ;;; ;     ;     ;;;;    ; ;;; 
;   ;    ;  ;;  ;;    ;     ;       ;;  ;;  ;    ;       ;     ;    ;    ;       ;  ;;   ;  ;;  ;;     ;    ;;  ;;   ;;    
;   ;    ;  ;    ;    ;     ;;;;;;  ;    ;  ;    ;       ;     ;    ;;;;;;       ;  ;    ;  ;    ;     ;    ;    ;   ;     
;   ;    ;  ;;;;;;    ;     ;       ;    ;  ;    ;   ;;;;;     ;    ;    ;   ;;;;;  ;    ;  ;    ;     ;    ;;;;;;   ;     
;    ;;;;   ;         ;     ;       ;    ;  ;    ;  ;    ;     ;    ;    ;  ;    ;  ;    ;  ;    ;     ;    ;        ;     
;   ;       ;;        ;     ;       ;;  ;;  ;   ;;  ;   ;;     ;    ;    ;  ;   ;;  ;    ;  ;;  ;;     ;    ;;       ;     
;    ;;;;;   ;;;;;     ;;;  ;;;;;;   ;;; ;   ;;; ;   ;;; ;   ;;;;;  ;    ;   ;;; ;  ;    ;   ;;; ;   ;;;;;   ;;;;;   ;     
;   ;     ;                              ;                                                                                 
;   ;     ;                              ;                                                                                 
;    ;;;;;                               ;                                                                                 
;                                                                                                                          

(define (getEqualHandler-test-suite)
  ; Values different than tables, equal types. No handler is used
  (test-equal (term (getEqualHandler 1 2 
                                     (((objr 1) ((\{ (\[ "__eq" \] = (objr 2)) \})
                                                 nil))
                                      ((objr 2) (function X () ((ref 1) ()) end)))))
              (term nil))
  
  ; Values of different types. No handler is used
  (test-equal (term (getEqualHandler 1 false
                                     (((objr 1) ((\{ (\[ "__eq" \] = (objr 2)) \}) nil))
                                      ((objr 2) (function X () ((ref 1) ()) end)))))
              (term nil))
  
  ; Tables with different handlers
  (test-equal (term (getEqualHandler (objr 5) (objr 6)
                                     (((objr 1) ((\{ (\[ "__eq" \] = (objr 2)) \})
                                                 nil))
                                      ((objr 2) (function X () ((ref 1) ()) end))
                                      ((objr 3) ((\{ (\[ "__eq" \] = (objr 4)) \}) nil))
                                      ((objr 4) (function X () ((ref 1) ()) end))
                                      ((objr 5) ((\{ \}) (objr 1)))
                                      ((objr 6) ((\{ \}) (objr 3))))))
              (term nil))
  
  ; Tables with the same handler
  (test-equal (term (getEqualHandler (objr 3) (objr 4)
                                     (((objr 1) ((\{ (\[ "__eq" \] = (objr 2)) \}) nil))
                                      ((objr 2) (function X () ((ref 1) ()) end))
                                      ((objr 3) ((\{ \}) (objr 1)))
                                      ((objr 4) ((\{ \}) (objr 1))))))
              (term (objr 2)))
  
  (test-results))


(define (test-all-meta-table-mech-metafunctions-test-suites)
  (getBinHandler-test-suite)
  (getEqualHandler-test-suite)
  )

(provide test-all-meta-table-mech-metafunctions-test-suites)

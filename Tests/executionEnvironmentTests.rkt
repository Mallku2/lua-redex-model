#lang racket
(require redex
         "../grammar.rkt"
         "../executionEnvironment.rkt"
         "../Relations/fullProgs.rkt")

; "black-box testing"


(define (basic-functions-test-suite)
  
  (test-predicate
   (redex-match core-lang ((σ : θ : \;)))
   (apply-reduction-relation*
    full-progs-rel
    (term
     (plugIntoExecutionEnvironment
      (
      
                                                     
                                                         
       ;                                                  
       ;                                                  
       ;                                             ;    
       ;                                             ;    
       ;     ;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;;;; 
       ;    ;   ;  ;    ;  ;    ;  ;;  ;;   ;;  ;    ;    
       ;        ;  ;       ;       ;    ;   ;        ;    
       ;    ;;;;;   ;;;;    ;;;;   ;;;;;;   ;        ;    
       ;   ;    ;       ;       ;  ;        ;        ;    
       ;   ;   ;;  ;    ;  ;    ;  ;;   ;   ;        ;    
       ;    ;;; ;   ;;;;    ;;;;    ;;;;    ;         ;;; 
       ;                                                  
       ;                                                  
       ;                                                  
        
          
       (local (status errmessage) = (((~ENV \[ "pcall" \]) ((~ENV \[ "assert" \]) false "error message")))
         in
         (((~ENV \[ "assert" \]) ((status == false)))
          ((~ENV \[ "assert" \]) ((errmessage == "error message"))))
         end)
             
       ((~ENV \[ "assert" \]) (true "error message"))
       ((~ENV \[ "assert" \]) (1 2 3 4 5))
          
          
          
          
       ;                                          
       ;                                          
       ;                                          
       ;                                          
       ;                                          
       ;    ;;;;    ;;;;    ;;;;    ;;;;    ;;;;  
       ;   ;;  ;;   ;;  ;   ;;  ;  ;;  ;;   ;;  ; 
       ;   ;    ;   ;       ;      ;    ;   ;     
       ;   ;;;;;;   ;       ;      ;    ;   ;     
       ;   ;        ;       ;      ;    ;   ;     
       ;   ;;   ;   ;       ;      ;;  ;;   ;     
       ;    ;;;;    ;       ;       ;;;;    ;     
       ;                                          
       ;                                          
       ;                                          
        
       (local (status errmessage) = (((~ENV \[ "pcall" \])
                                      ((~ENV \[ "error" \])
                                       "error message")))
         in
         (((~ENV \[ "assert" \]) ((status == false)))
          ((~ENV \[ "assert" \]) ((errmessage == "error message"))))
         end)
      
        
        
       ;                                                                                                  
       ;                                                                           ;       ;;;            
       ;                                                                           ;         ;            
       ;                     ;                       ;               ;             ;         ;            
       ;                     ;                       ;               ;             ;         ;            
       ;    ;;;;;   ;;;;   ;;;;;;  ;;;;;;;  ;;;;   ;;;;;;    ;;;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
       ;   ;;  ;;  ;;  ;;    ;     ;  ;  ; ;;  ;;    ;      ;   ;    ;      ;   ;  ;;  ;;    ;     ;;  ;; 
       ;   ;    ;  ;    ;    ;     ;  ;  ; ;    ;    ;          ;    ;          ;  ;    ;    ;     ;    ; 
       ;   ;    ;  ;;;;;;    ;     ;  ;  ; ;;;;;;    ;      ;;;;;    ;      ;;;;;  ;    ;    ;     ;;;;;; 
       ;   ;    ;  ;         ;     ;  ;  ; ;         ;     ;    ;    ;     ;    ;  ;    ;    ;     ;      
       ;   ;;  ;;  ;;   ;    ;     ;  ;  ; ;;   ;    ;     ;   ;;    ;     ;   ;;  ;;  ;;    ;     ;;   ; 
       ;    ;;; ;   ;;;;      ;;;  ;  ;  ;  ;;;;      ;;;   ;;; ;     ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
       ;        ;                                                                                         
       ;    ;   ;                                                                                         
       ;     ;;;                                                                                          
        
       (local (a b) = ((\{ \}) (\{ \})) in
         (((~ENV \[ "setmetatable" \]) (a b))
          ((~ENV \[ "assert" \]) ((((~ENV \[ "getmetatable" \]) (a)) == b))))
         end)
        
        
       ;                                                  
       ;     ;                       ;                    
       ;                                                  
       ;                                                  
       ;                                                  
       ;   ;;;     ;;;;;     ;;;   ;;;      ;;;;    ;;;;  
       ;     ;     ;;  ;;   ;   ;    ;      ;;  ;  ;    ; 
       ;     ;     ;    ;       ;    ;      ;      ;      
       ;     ;     ;    ;   ;;;;;    ;      ;       ;;;;  
       ;     ;     ;    ;  ;    ;    ;      ;           ; 
       ;     ;     ;;  ;;  ;   ;;    ;      ;      ;    ; 
       ;   ;;;;;   ;;;;;    ;;; ;  ;;;;;    ;       ;;;;  
       ;           ;                                      
       ;           ;                                      
       ;           ;                                      
       (local (a) = ((\{ (\[ 1 \] = "a") (\[ 2 \] = "b") \})) in
         (local (it table index) = (((~ENV \[ "ipairs" \]) (a)))
           in
           (((~ENV \[ "assert" \]) ((table == a)))
            ((~ENV \[ "assert" \]) ((index == 0)))
            (local (nextIndex value) = ((it (a 0)))
              in
              (((~ENV \[ "assert" \]) ((nextIndex == 1)))
                                                             
               ((~ENV \[ "assert" \]) ((value == "a")))
                                                             
               ((nextIndex value) = ((it (a 1))))
                                                             
               ((~ENV \[ "assert" \]) ((nextIndex == 2)))
                                                             
               ((~ENV \[ "assert" \]) ((value == "b")))
                                                             
               ; The iterator returned in successive calls
               ; is the same
               (local (it2 table2 index2) = (((~ENV \[ "ipairs" \]) (a)))
                 in
                                                               
                 ((~ENV \[ "assert" \]) ((it2 == it)))
                                                               
                 end)
               )
                                                            
              end))
           end)
         end)
      
       (local (a b) = ((\{ \}) (\{ (\[ "__ipairs" \] = (function $func1 ()
                                                                 (return (< 1 2 3 >))
                                                                 end)) \})) in
         (((~ENV \[ "setmetatable" \]) (a b))
          (local (it table index) = (((~ENV \[ "ipairs" \]) (a)))
            in
            (((~ENV \[ "assert" \]) ((it == 1)))
             ((~ENV \[ "assert" \]) ((table == 2)))
             ((~ENV \[ "assert" \]) ((index == 3)))
                                                           
             ; The iterator returned in successive calls
             ; is the same
             (local (it2 table2 index2) = (((~ENV \[ "ipairs" \]) (a)))
               in
                                                             
               ((~ENV \[ "assert" \]) ((it2 == it)))
                                                             
               end))
            end))
         end)
        
        
         
       ;                                  
       ;   ;;;                          ; 
       ;     ;                          ; 
       ;     ;                          ; 
       ;     ;                          ; 
       ;     ;      ;;;;     ;;;    ;;;;; 
       ;     ;     ;;  ;;   ;   ;  ;;  ;; 
       ;     ;     ;    ;       ;  ;    ; 
       ;     ;     ;    ;   ;;;;;  ;    ; 
       ;     ;     ;    ;  ;    ;  ;    ; 
       ;     ;     ;;  ;;  ;   ;;  ;;  ;; 
       ;      ;;;   ;;;;    ;;; ;   ;;;;; 
       ;                                  
       ;                                  
       ;                                  

                                         
       
       (((~ENV |[| "fat" |]|))
        =
        ((function
          $2
          (x)
          (if (x <= 1) then (return (< 1 >))
              else
              (return (x * (((~ENV |[| "load" |]|) (("return fat(" .. ((x - 1) .. ")")))) ())))
              end)
          end)))
       
       ((~ENV \[ "assert" \]) (((~ENV \[ "load" \]) ("assert(fat(6)==720)"))))

       (local (a) = (true) in
         (local (b) = (((~ENV |[| "load" |]|)
                        ((function $1 ()
                                   (if a then
                                       (((a) = (false))
                                        (return "return true"))
                                       else
                                       (return nil)
                                       end)
                                   end))))
           in
           ((~ENV \[ "assert" \]) ((b ())))
           end)
         end)

       
                                                                         
       ;;;                          ;     ;;     ;     ;;;            
       ;     ;                          ;    ;               ;            
       ;     ;                          ;    ;               ;            
       ;     ;                          ;    ;               ;            
       ;     ;      ;;;;     ;;;    ;;;;;  ;;;;;   ;;;       ;      ;;;;  
       ;     ;     ;;  ;;   ;   ;  ;;  ;;    ;       ;       ;     ;;  ;; 
       ;     ;     ;    ;       ;  ;    ;    ;       ;       ;     ;    ; 
       ;     ;     ;    ;   ;;;;;  ;    ;    ;       ;       ;     ;;;;;; 
       ;     ;     ;    ;  ;    ;  ;    ;    ;       ;       ;     ;      
       ;     ;     ;;  ;;  ;   ;;  ;;  ;;    ;       ;       ;     ;;   ; 
       ;      ;;;   ;;;;    ;;; ;   ;;;;;    ;     ;;;;;      ;;;   ;;;;  
       ;                                                                  
       ;                                                                  
       ;
       (((~ENV \[ "fat" \])) = (((~ENV \[ "loadfile" \]) ("loadfile_test.lua"))))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "fat" \]) (1)) == 1)))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "fat" \]) (6)) == 720)))

       ;                                  
       ;                                  
       ;                                  
       ;                             ;    
       ;                             ;    
       ;   ; ;;;    ;;;;   ;;  ;;  ;;;;;; 
       ;   ;;   ;  ;;  ;;   ;  ;     ;    
       ;   ;    ;  ;    ;    ;;      ;    
       ;   ;    ;  ;;;;;;    ;;      ;    
       ;   ;    ;  ;         ;;      ;    
       ;   ;    ;  ;;   ;   ;  ;     ;    
       ;   ;    ;   ;;;;   ;;  ;;     ;;; 
       ;                                  
       ;                                  
       ;
       (local (a) = ((\{ (\[ 1 \] = 2) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "next" \]) (a)) == 1)))
         end)
      
       (local (a) = ((\{ (\[ 1 \] = 2) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "next" \]) (a nil)) == 1)))
         end)
      
       (local (a) = ((\{ (\[ 1 \] = 2) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "next" \]) (a 1)) == nil)))
         end)
      
       (local (a) = ((\{ (\[ 1 \] = 2) (\[ 3 \] = 4) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "next" \]) (a 1)) == 3)))
         end)
       ;  
       ;                                          
       ;                     ;                    
       ;                                          
       ;                                          
       ;                                          
       ;   ;;;;;     ;;;   ;;;      ;;;;    ;;;;  
       ;   ;;  ;;   ;   ;    ;      ;;  ;  ;    ; 
       ;   ;    ;       ;    ;      ;      ;      
       ;   ;    ;   ;;;;;    ;      ;       ;;;;  
       ;   ;    ;  ;    ;    ;      ;           ; 
       ;   ;;  ;;  ;   ;;    ;      ;      ;    ; 
       ;   ;;;;;    ;;; ;  ;;;;;    ;       ;;;;  
       ;   ;                                      
       ;   ;                                      
       ;   ;                                      
        
       (local (a) = ((\{ (\[ "a" \] = 1) (\[ "b" \] = 2) \})) in
         (local (it table index) = (((~ENV \[ "pairs" \]) (a)))
           in
           (((~ENV \[ "assert" \]) ((it == (~ENV \[ "next" \]))))
            ((~ENV \[ "assert" \]) ((table == a)))
            ((~ENV \[ "assert" \]) ((index == nil)))
                                                          
            (((~ENV \[ "next" \])) = (1))
            ((it) = (((~ENV \[ "pairs" \]) (a))))
            ; Now, (~ENV \[ "next" \]) has 1 as value.
            ((~ENV \[ "assert" \]) ((not (it == (~ENV \[ "next" \])))))
                                                          
            ; However, "it" points to the "next" built-in service
            ((~ENV \[ "assert" \]) (((it (a)) == "a")))
            ((~ENV \[ "assert" \]) (((it (a "a")) == "b")))
            ((~ENV \[ "assert" \]) (((it (a "b")) == nil)))
                                                          
            ; The iterator returned in successive calls
            ; is the same
            (local (it2 table2 index2) = (((~ENV \[ "pairs" \]) (a)))
              in
              ((~ENV \[ "assert" \]) ((it2 == it)))
                                                            
              end)
            )
           end)
         end)
       (local (a b) = ((\{ \}) (\{ (\[ "__pairs" \] = (function $func1 ()
                                                                (return (< 1 2 3 >))
                                                                end)) \})) in
         (((~ENV \[ "setmetatable" \]) (a b))
          (local (it table index) = (((~ENV \[ "pairs" \]) (a)))
            in
            (((~ENV \[ "assert" \]) ((it == 1)))
             ((~ENV \[ "assert" \]) ((table == 2)))
             ((~ENV \[ "assert" \]) ((index == 3)))
                                                           
             ; The iterator returned in successive calls
             ; is the same
             (local (it2 table2 index2) = (((~ENV \[ "pairs" \]) (a)))
               in
                                                             
               ((~ENV \[ "assert" \]) ((it2 == it)))
                                                             
               end)
             )
            end))
         end)
       ;                                          
       ;                           ;;;     ;;;    
       ;                             ;       ;    
       ;                             ;       ;    
       ;                             ;       ;    
       ;   ;;;;;     ;;;     ;;;     ;       ;    
       ;   ;;  ;;   ;   ;   ;   ;    ;       ;    
       ;   ;    ;  ;            ;    ;       ;    
       ;   ;    ;  ;        ;;;;;    ;       ;    
       ;   ;    ;  ;       ;    ;    ;       ;    
       ;   ;;  ;;   ;   ;  ;   ;;    ;       ;    
       ;   ;;;;;     ;;;    ;;; ;     ;;;     ;;; 
       ;   ;                                      
       ;   ;                                      
       ;   ;
       (local (f) = ((function $func () ((~ENV \[ "assert" \]) (false)) end))
         in ((~ENV \[ "pcall" \]) (f))
         end)
          
       ;                                                  
       ;                                                  
       ;                                                  
       ;                                             ;    
       ;                                             ;    
       ;    ;;;;     ;;;  ;      ;  ;;;;;   ;;;;   ;;;;;; 
       ;    ;;  ;   ;   ; ;      ; ;;  ;;  ;;  ;;    ;    
       ;    ;           ;  ; ;; ;  ;    ;  ;    ;    ;    
       ;    ;       ;;;;;  ; ;; ;  ;    ;  ;;;;;;    ;    
       ;    ;      ;    ;  ; ;; ;  ;    ;  ;         ;    
       ;    ;      ;   ;;   ;  ;   ;;  ;;  ;;   ;    ;    
       ;    ;       ;;; ;   ;  ;    ;;; ;   ;;;;      ;;; 
       ;                                ;                 
       ;                            ;   ;                 
       ;                             ;;;                  
       (local (a) = ((\{ (\[ 1 \] = 2) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "rawget" \]) (a 1)) == 2)))
         end)
       (local (a) = ((\{ (\[ 1 \] = 2) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "rawget" \]) (a 3)) == nil)))
         end)
      
       (local (status errmessage) = (((~ENV \[ "pcall" \])
                                      ((~ENV \[ "rawget" \]) 1 2)))
         in
         (((~ENV \[ "assert" \]) ((status == false)))
          ((~ENV \[ "assert" \]) ((errmessage == "bad argument #1 (table expected, got number)"))))
         end)
          
          
       ;                                                  
       ;                           ;;;                    
       ;                             ;                    
       ;                             ;                    
       ;                             ;                    
       ;    ;;;;     ;;;  ;      ;   ;      ;;;;   ; ;;;  
       ;    ;;  ;   ;   ; ;      ;   ;     ;;  ;;  ;;   ; 
       ;    ;           ;  ; ;; ;    ;     ;    ;  ;    ; 
       ;    ;       ;;;;;  ; ;; ;    ;     ;;;;;;  ;    ; 
       ;    ;      ;    ;  ; ;; ;    ;     ;       ;    ; 
       ;    ;      ;   ;;   ;  ;     ;     ;;   ;  ;    ; 
       ;    ;       ;;; ;   ;  ;      ;;;   ;;;;   ;    ; 
       ;                                                  
       ;                                                  
       ;
        
       (local (a) = ((\{ (\[ 1 \] = 3) (\[ 2 \] = 4) \})) in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "rawlen" \]) (a)) == 2)))
         end)
       (local (a) = ("añ") in
         ((~ENV \[ "assert" \]) ((((~ENV \[ "rawlen" \]) (a)) == 3)))
         end)
          
          
          
       ;                                                  
       ;                                                  
       ;                                                  
       ;                                             ;    
       ;                                             ;    
       ;    ;;;;     ;;;  ;      ;  ;;;;    ;;;;   ;;;;;; 
       ;    ;;  ;   ;   ; ;      ; ;    ;  ;;  ;;    ;    
       ;    ;           ;  ; ;; ;  ;       ;    ;    ;    
       ;    ;       ;;;;;  ; ;; ;   ;;;;   ;;;;;;    ;    
       ;    ;      ;    ;  ; ;; ;       ;  ;         ;    
       ;    ;      ;   ;;   ;  ;   ;    ;  ;;   ;    ;    
       ;    ;       ;;; ;   ;  ;    ;;;;    ;;;;      ;;; 
       ;                                                  
       ;                                                  
       ;                                                  
       (local (a) = ((\{ \})) in
         (((~ENV \[ "rawset" \]) (a 1 2))
          ((~ENV \[ "assert" \]) ((((~ENV \[ "rawget" \]) (a 1)) == 2))))
         end)
      
       (local (a) = (1) in
         (local (status errmessage) = (((~ENV \[ "pcall" \])
                                        ((~ENV \[ "rawset" \]) a 1 2)))
           in
           (((~ENV \[ "assert" \]) ((status == false)))
            ((~ENV \[ "assert" \]) ((errmessage == "bad argument #1 (table expected, got number)"))))
           end)
         end)
        
          
          
           
       ;                                                  
       ;                   ;;;                            
       ;                     ;                            
       ;                     ;                       ;    
       ;                     ;                       ;    
       ;    ;;;;    ;;;;     ;      ;;;;     ;;;   ;;;;;; 
       ;   ;    ;  ;;  ;;    ;     ;;  ;;   ;   ;    ;    
       ;   ;       ;    ;    ;     ;    ;  ;         ;    
       ;    ;;;;   ;;;;;;    ;     ;;;;;;  ;         ;    
       ;        ;  ;         ;     ;       ;         ;    
       ;   ;    ;  ;;   ;    ;     ;;   ;   ;   ;    ;    
       ;    ;;;;    ;;;;      ;;;   ;;;;     ;;;      ;;; 
       ;                                                  
       ;                                                  
       ;                                                  
        
       ((~ENV \[ "assert" \]) ((((~ENV \[ "select" \]) (1 2)) == 2)))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "select" \]) ("#" 1 2 3)) == 3)))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "select" \]) ("#")) == 0)))
          
    

       ;                                                                                                  
       ;                                                                           ;       ;;;            
       ;                                                                           ;         ;            
       ;                     ;                       ;               ;             ;         ;            
       ;                     ;                       ;               ;             ;         ;            
       ;    ;;;;    ;;;;   ;;;;;;  ;;;;;;;  ;;;;   ;;;;;;    ;;;   ;;;;;;    ;;;   ;;;;;     ;      ;;;;  
       ;   ;    ;  ;;  ;;    ;     ;  ;  ; ;;  ;;    ;      ;   ;    ;      ;   ;  ;;  ;;    ;     ;;  ;; 
       ;   ;       ;    ;    ;     ;  ;  ; ;    ;    ;          ;    ;          ;  ;    ;    ;     ;    ; 
       ;    ;;;;   ;;;;;;    ;     ;  ;  ; ;;;;;;    ;      ;;;;;    ;      ;;;;;  ;    ;    ;     ;;;;;; 
       ;        ;  ;         ;     ;  ;  ; ;         ;     ;    ;    ;     ;    ;  ;    ;    ;     ;      
       ;   ;    ;  ;;   ;    ;     ;  ;  ; ;;   ;    ;     ;   ;;    ;     ;   ;;  ;;  ;;    ;     ;;   ; 
       ;    ;;;;    ;;;;      ;;;  ;  ;  ;  ;;;;      ;;;   ;;; ;     ;;;   ;;; ;  ;;;;;      ;;;   ;;;;  
       ;                                                                                                  
       ;                                                                                                  
       ;                                                                                                  
                                                                                                      
                                                                                                        
                                                                                                        
                                                                                                        
       (local (a b c) = ((\{ \}) (\{ (\[ "__metatable" \] = 1) \}) (\{ \})) in
         (((~ENV \[ "setmetatable" \]) (a b))
          (local (status errmessage) = (((~ENV \[ "pcall" \])
                                         ((~ENV \[ "setmetatable" \]) a c)))
            in
            (((~ENV \[ "assert" \]) ((status == false)))
             ((~ENV \[ "assert" \]) ((errmessage == "cannot change a protected metatable"))))
            end))
         end)

                                                                               
       ;                                           ;                      
       ;                                           ;                      
       ;     ;                                     ;                      
       ;     ;                                     ;                      
       ;   ;;;;;;   ;;;;   ; ;;;   ;    ;  ;;;;;;; ;;;;;    ;;;;    ;;;;  
       ;     ;     ;;  ;;  ;;   ;  ;    ;  ;  ;  ; ;;  ;;  ;;  ;;   ;;  ; 
       ;     ;     ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;    ;   ;     
       ;     ;     ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;;;;;;   ;     
       ;     ;     ;    ;  ;    ;  ;    ;  ;  ;  ; ;    ;  ;        ;     
       ;     ;     ;;  ;;  ;    ;  ;   ;;  ;  ;  ; ;;  ;;  ;;   ;   ;     
       ;      ;;;   ;;;;   ;    ;   ;;; ;  ;  ;  ; ;;;;;    ;;;;    ;     
       ;                                                                  
       ;                                                                  
       ;                                                                  
        
       ((~ENV \[ "assert" \]) ((((~ENV \[ "tonumber" \]) ("1")) == 1)))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "tonumber" \]) ("1" 10)) == 1)))
          
          
       ;                                                                  
       ;                                             ;                    
       ;                                                                  
       ;     ;                       ;                                    
       ;     ;                       ;                                    
       ;   ;;;;;;   ;;;;    ;;;;   ;;;;;;   ;;;;   ;;;     ; ;;;    ;;;;; 
       ;     ;     ;;  ;;  ;    ;    ;      ;;  ;    ;     ;;   ;  ;;  ;; 
       ;     ;     ;    ;  ;         ;      ;        ;     ;    ;  ;    ; 
       ;     ;     ;    ;   ;;;;     ;      ;        ;     ;    ;  ;    ; 
       ;     ;     ;    ;       ;    ;      ;        ;     ;    ;  ;    ; 
       ;     ;     ;;  ;;  ;    ;    ;      ;        ;     ;    ;  ;;  ;; 
       ;      ;;;   ;;;;    ;;;;      ;;;   ;      ;;;;;   ;    ;   ;;; ; 
       ;                                                                ; 
       ;                                                            ;   ; 
       ;                                                             ;;;
          
       ((~ENV \[ "assert" \]) ((((~ENV \[ "tostring" \]) (1.0)) == "1")))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "tostring" \]) (1.1)) == "1.1")))
          
          
          
          
          
       ;     ;                            
       ;     ;                            
       ;   ;;;;;;  ;    ;  ;;;;;    ;;;;  
       ;     ;      ;   ;  ;;  ;;  ;;  ;; 
       ;     ;      ;  ;   ;    ;  ;    ; 
       ;     ;      ;  ;   ;    ;  ;;;;;; 
       ;     ;       ; ;   ;    ;  ;      
       ;     ;       ;;    ;;  ;;  ;;   ; 
       ;      ;;;     ;    ;;;;;    ;;;;  
       ;              ;    ;              
       ;             ;     ;              
       ;            ;;     ;              
      
       ((~ENV \[ "assert" \]) ((((~ENV \[ "type" \]) (true)) == "boolean")))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "type" \]) (1)) == "number")))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "type" \]) ("true")) == "string")))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "type" \]) ((function $func () \; end))) == "function")))
       ((~ENV \[ "assert" \]) ((((~ENV \[ "type" \]) ((\{ \}))) == "table")))
          
          
          
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
          
          
       ;                          
       ;                          
       ;                          
       ;                          
       ;                          
       ;    ;;;;    ;;;;   ;;;;;  
       ;    ;;  ;  ;;  ;;  ;;  ;; 
       ;    ;      ;    ;  ;    ; 
       ;    ;      ;;;;;;  ;    ; 
       ;    ;      ;       ;    ; 
       ;    ;      ;;   ;  ;;  ;; 
       ;    ;       ;;;;   ;;;;;  
       ;                   ;      
       ;                   ;      
       ;                   ;      
       ((~ENV \[ "assert" \])(((((~ENV \[ "string" \]) \[ "rep" \]) ("a" 4)) == "aaaa")))
       ((~ENV \[ "assert" \])(((((~ENV \[ "string" \]) \[ "rep" \]) ("a" 4 "b")) == "abababa")))
          
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
          
          
          
       ;                                  
       ;                           ;      
       ;                           ;      
       ;                           ;      
       ;                           ;      
       ;   ;;;;;     ;;;     ;;;   ;   ;  
       ;   ;;  ;;   ;   ;   ;   ;  ;  ;   
       ;   ;    ;       ;  ;       ; ;    
       ;   ;    ;   ;;;;;  ;       ;;;    
       ;   ;    ;  ;    ;  ;       ;  ;   
       ;   ;;  ;;  ;   ;;   ;   ;  ;   ;  
       ;   ;;;;;    ;;; ;    ;;;   ;    ; 
       ;   ;                              
       ;   ;                              
       ;   ;                              
          
       (local (table) = ((((~ENV \[ "table" \]) \[ "pack" \]) ("a" "b" "c"))) in
         (((~ENV \[ "assert" \]) (((table \[ 1 \]) == "a")))
          ((~ENV \[ "assert" \]) (((table \[ 2 \]) == "b")))
          ((~ENV \[ "assert" \]) (((table \[ 3 \]) == "c")))
          ((~ENV \[ "assert" \]) (((table \[ "n" \]) == 3))))
         end)
          
          
       ;                                                  
       ;                                           ;      
       ;                                           ;      
       ;                                           ;      
       ;                                           ;      
       ;   ;    ;  ; ;;;   ;;;;;     ;;;     ;;;   ;   ;  
       ;   ;    ;  ;;   ;  ;;  ;;   ;   ;   ;   ;  ;  ;   
       ;   ;    ;  ;    ;  ;    ;       ;  ;       ; ;    
       ;   ;    ;  ;    ;  ;    ;   ;;;;;  ;       ;;;    
       ;   ;    ;  ;    ;  ;    ;  ;    ;  ;       ;  ;   
       ;   ;   ;;  ;    ;  ;;  ;;  ;   ;;   ;   ;  ;   ;  
       ;    ;;; ;  ;    ;  ;;;;;    ;;; ;    ;;;   ;    ; 
       ;                   ;                              
       ;                   ;                              
       ;                   ;                              
          
       (local (table) = ((\{ "a" "b" "c" \})) in
         (local (v1 v2 v3) = ((((~ENV \[ "table" \]) \[ "unpack" \]) (table))) in
           (((~ENV \[ "assert" \]) ((v1 == "a")))
            ((~ENV \[ "assert" \]) ((v2 == "b")))
            ((~ENV \[ "assert" \]) ((v3 == "c"))))
           end)
         end)


       )))))
    
    
  
  (test-results))

(define (execution-environment-test-suite)
  (basic-functions-test-suite))

(provide execution-environment-test-suite)
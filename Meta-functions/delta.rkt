#lang racket
(require redex
         "../grammar.rkt"
         "./tablesMetafunctions.rkt"
         "./objStoreMetafunctions.rkt"
         "./grammarMetafunctions.rkt"
         "../Desugar/parser.rkt"
         "../Desugar/lexer.rkt"
         "../Desugar/phrases_constructors.rkt")


; We define the semantics of the binary and unary operators of our language
; in terms of operations of PLT Racket. The "," symbol is treated as an escape
; to PLT Racket code. So, in general, the semantics of an expression
; (◇ op_1 op_2) is defined as the PLT Racket code (◇ (term op_1) (term op_2))
; when ◇ is also an operator of PLT Racket.
(define-metafunction core-lang
  ; Arithmetic operations
  ; From https://www.lua.org/manual/5.2/manual.html#2.1:
  ; "Number represents real (double-precision floating-point) numbers"
  [(δ (+ Number_1 Number_2))
   ,(+ (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ (- Number_1 Number_2))
   ,(- (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ (* Number_1 Number_2))
   ,(* (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ (/ 0 0))
   +nan.0]
  
  [(δ (/ Number 0))
   +inf.0]

  
  
  ; Number_1 != 0
  [(δ (/ Number_1 Number_2))
   ,(/ (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ (^ Number_1 Number_2))
   ,(expt (term Number_3) (term Number_4))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ (% Number 0))
   $nan]
  
  [(δ (% Number_1 Number_2))
   ;a - math.floor(a/b)*b
   (δ (- Number_3 (δ (* (δ (math.floor (δ (/ Number_3 Number_4)))) Number_4))))
   
   (where Number_3 ,(real->double-flonum (term Number_1)))
   (where Number_4 ,(real->double-flonum (term Number_2)))]
  
  [(δ (- Number))
   ,(- (term Number))
   
   (where Number_2 ,(real->double-flonum (term Number)))]
  
  ; Number comparison
  [(δ (< Number_1 Number_2)) 
   (toBool ,(< (term Number_1) (term Number_2)))]
  
  [(δ (<= Number_1 Number_2)) 
   (toBool ,(<= (term Number_1) (term Number_2)))]
  
  ; String comparison)
  [(δ (< String_1 String_2)) 
   (toBool ,(string<? (term String_1) (term String_2)))]
  
  [(δ (<= String_1 String_2)) 
   (toBool ,(string<=? (term String_1) (term String_2)))]
  
  ; String concatenation
  [(δ (.. String_1 String_2))
   ,(string-append (term String_1) (term String_2))]
  
  ; String length
  ; Racket's bytes-string, to simulate what Lua's # operator does.
  [(δ (\# String))
   ,(bytes-length (string->bytes/utf-8 (term String)))]
  
  ; Table length
  [(δ (\# (\{ field ... \})))
   any
   
   (where (Number_1 Number_2 ...) 
          ,(map (λ (field)
                  ; Extract the expression associated with the binding...
                  (bind-exp
                   ; number 2...
                   (list-ref
                    ; of the list of match-bindings...
                    (match-bindings
                     ; which should be of length 1.
                     (list-ref
                      
                      (redex-match core-lang
                                   (\[ Number \] = any)
                                   field) 
                      0)) 
                    1)))
                ; Obtain the fields of the form (\[ Number \] = any)
                (filter (λ (field)
                          (redex-match core-lang
                                       (\[ Number \] = any)
                                       field))
                        (term (field ...)))))
   
   (where any ,(argmax (λ (number) number) (term (Number_1 Number_2 ...))))]
  
  ; Table doesn't have numeric keys
  [(δ (\# evaluatedtable))
   0]
  
  ; Equality comparison
  ; Numbers needs special treatment
  [(δ (== Number_1 Number_2))
   (toBool ,(= (term Number_1) (term Number_2)))]
  
  [(δ (== v_1 v_2))
   (toBool ,(equal? (term v_1) (term v_2)))]
  
  ; Logical connectives
  [(δ (and v e)) 
   v
   (side-condition (or (equal? (term v) (term false))
                       (equal? (term v) (term nil))))]
  
  ; Try: a,b = true and g(), with g being a function that returns 2 or more 
  ; values
  [(δ (and v e)) 
   (\( e \))
   (side-condition (and (not (equal? (term v) (term false)))
                        (not (equal? (term v) (term nil)))))]
  
  [(δ (or v e)) 
   v
   (side-condition (and (not (equal? (term v) (term false)))
                        (not (equal? (term v) (term nil)))))]
  
  ; Try: a,b = false or g(), with g being a function that returns 2 or more 
  ; values
  [(δ (or v e)) 
   (\( e \))
   (side-condition (or (equal? (term v) (term false))
                       (equal? (term v) (term nil))))]
  
  [(δ (not v)) 
   true
   (side-condition (or (equal? (term v) (term nil))
                       (equal? (term v) (term false))))]
  
  [(δ (not v)) 
   false]
  
  ; Built-in services
  
  
  ;                                                                                                                          
  ;                             ;                        ;;                                     ;                            
  ;                                                     ;                                                                    
  ;   ;;;;;;                                            ;                               ;                                    
  ;   ;     ;                                           ;                               ;                                    
  ;   ;     ;   ;;;    ;;;;   ;;;       ;;;           ;;;;;   ;    ;  ; ;;;     ;;;   ;;;;;;  ;;;      ;;;;   ; ;;;    ;;;;  
  ;   ;     ;  ;   ;  ;    ;    ;      ;   ;            ;     ;    ;  ;;   ;   ;   ;    ;       ;     ;;  ;;  ;;   ;  ;    ; 
  ;   ;;;;;;       ;  ;         ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;  ;      
  ;   ;     ;  ;;;;;   ;;;;     ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;   ;;;;  
  ;   ;     ; ;    ;       ;    ;     ;                 ;     ;    ;  ;    ;  ;         ;       ;     ;    ;  ;    ;       ; 
  ;   ;     ; ;   ;;  ;    ;    ;      ;   ;            ;     ;   ;;  ;    ;   ;   ;    ;       ;     ;;  ;;  ;    ;  ;    ; 
  ;   ;;;;;;   ;;; ;   ;;;;   ;;;;;     ;;;             ;      ;;; ;  ;    ;    ;;;      ;;;  ;;;;;    ;;;;   ;    ;   ;;;;  
  ;                                                                                                                          
  ;                                                                                                                          
  ;                                                                                                                          
  
  
  
  ;                                                  
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
  
  ; Assert: condition evaluates to false or nil
  [(δ (assert v_1 v_2 v_3 ...))
   (δ (error any))
   
   (side-condition (or (equal? (term v_1) (term false))
                       (equal? (term v_1) (term nil))))
   
   (where any ,(if (equal? (term v_2) (term nil))
                   ; v_2 is nil. Return default error message.
                   (term "assertion failed!")
                   (term v_2)))]
  
  ; Assert: condition evaluates to true
  [(δ (assert v ...))
   (< v ... >)]
  
  
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
  
  [(δ (error v))
   ($err v)]
  
  [(δ (error v_1 v_2))
   ($err v_1)]
  
  
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
  
  ; Table value has a meta-table, which hasn't a "__metatable" key
  [(δ (getmetatable objref_1 θ))
   objref_2
   
   (side-condition (equal? (term (δ (type objref_1 θ)))
                           (term "table")))
   
   (where (tableconstructor_1 objref_2) (derefTheta θ objref_1))
   
   (where (tableconstructor_2 any) (derefTheta θ objref_2))
   
   (side-condition (not (term (keyBelongsTo? tableconstructor_2
                                             "__metatable"))))]
  
  ; Table value has a meta-table, which has a "__metatable" key.
  [(δ (getmetatable objref_1 θ))
   (δ (rawget objref_2 "__metatable" θ))
   
   (side-condition (equal? (term (δ (type objref_1 θ)))
                           (term "table")))
   
   (where (tableconstructor objref_2) (derefTheta θ objref_1))]
  
  ; Table value has not a meta-table: this case holds when the previous don't.
  [(δ (getmetatable objref θ))
   nil
   
   (side-condition (equal? (term (δ (type objref θ)))
                           (term "table")))]
  
  ; The value isn't a table. It has a meta-table,
  ; which has not a "__metatable" key
  [(δ (getmetatable any_1 θ))
   objref
   
   ; Obtain the prefixed reference to the value's type's meta-table.
   (where objref (getMetaTableRef any_1))
   ; Is the meta-table set?
   (side-condition (term (refBelongsToTheta? objref θ)))
   
   (where (tableconstructor any_2) (derefTheta θ objref))
   
   (side-condition (not (term (keyBelongsTo? tableconstructor
                                             "__metatable"))))]
  
  ; The value isn't a table. It has a meta-table, which has
  ; a "__metatable" key,
  [(δ (getmetatable any θ))
   (δ (rawget objref "__metatable" θ))
   
   (where objref (getMetaTableRef any))
   
   (side-condition (term (refBelongsToTheta? objref θ)))]
  
  ; Type has not a meta-table
  [(δ (getmetatable any θ))
   nil]
  
  
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
  
  ; Custom iterator, provided by the metatable
  [(δ (ipairs objref θ))
   ((function $IpairsCustomIter ()
              (local (v1 v2 v3) = ((any (objref)))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "table"))
   
   (where any (indexMetaTable objref "__ipairs" θ))
   
   (side-condition (not (equal? (term any)
                                (term nil))))]

  ; Default iterator
  [(δ (ipairs objref θ))
   (< (function $iPairsDefaultIter (t var)
                (local (result ttype) = (nil ($builtIn type (t)))
                  in
                  ((if (ttype == "table")
                       then
                       
                       \;
                       
                       else
                       
                       ($builtIn error ((("bad argument #1 (table expected, got" .. ttype) .. ")")))
                       
                       end)
                   
                   ((var) = ((var + 1)))
                   
                   ((result) = (($builtIn rawget (t var))))
                   
                   (if (result == nil)
                       then
                       
                       (return (< nil >))
                       
                       else
                       
                       (return (< var result >))
                       
                       end))
                  end)
                end) objref 0 >)
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "table"))
   
   (where nil (indexMetaTable objref "__ipairs" θ))]
  
  [(δ (ipairs v θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v θ)))
   
   (where String_2 ,(string-append "bad argument #1 (table expected, got "
                                   (term String_1)
                                   ")"))]
  
  
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
  [(δ (load String v_1 "b" v_3 θ))
   any_2
   
   ; Reuse Racket's reader to parse our string representation of functions.
   (where any_1 ,(read (open-input-string (term String))))
   
   (where any_2 ,(if (redex-match core-lang
                                  functiondef
                                  (term any_1))
                     (term any_1)
                     "attempt to load a text chunk (mode is 'b')"))]
  
  ; Lua program expressed into a string, either syntactically correct or not 
  [(δ (load String v_1 v_2 v_3 θ))
   any_2
   
   (where any_1 ,(with-handlers ([exn:fail?
                                  (λ (e) (append (term (< nil ))
                                                 (list (if (equal? (term v_1) (term nil))
                                                           
                                                           (string-append "[string "
                                                                          (term String)
                                                                          "]")
                                                           
                                                           (term v_1)))
                                                 (term ( >))))])
                   
                   (parse-this (term String)
                               #t
                               ; Reference to _ENV hardcoded here...
                               (term (to-abstract (ref 1)))
                               )
                   
                   ))

   
   
   (where any_2 ,(if (not (redex-match core-lang
                                       (< e ... >)
                                       (term any_1)))
                     ; If the parsing succeeded, return to code obtained wrapped into a
                     ; vararg function.
                     (append (term (function $loaded (<<<)))
                             (if (not (equal? (term v_3) (term nil)))
                                 ; Received new value for the global environment
                                 (list (term (local ($old_env) = ((ref 1))
                                               in
                                               ((((ref 1)) = (v_3))
                                                any_1
                                                (((ref 1)) = ($old_env)))
                                               end)))

                                 (list (term any_1)))
                             
                             (term (end)))
                     ; Otherwise, return the error message. 
                     (term any_1)))]
  
  ; Received a function from which we can obtain the string that represents the
  ; program
  [(δ (load objref v_1 v_2 v_3 θ))
   any_2
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "function"))
   
   ; Set the appropriate value to the "source" argument (second argument)
   (where any ,(if (equal? (term v_1) (term nil))
                   
                   "=(load)"
                   
                   (term v_1)))


   (where any_2 ((function $loaded ()
                           (local (program nextPiece) = ("" "")
                             in
                             (((nextPiece) = ((objref ())))
                              
                              (while (not ((nextPiece == "") or (nextPiece == nil)))
                                     do
                                     
                                     (if (($builtIn type (nextPiece)) == "string")
                                         then
                                         (((program) = ((program .. nextPiece)))
                                          ((nextPiece) = ((objref ()))))
                                         else
                                         (return (< nil "reader function must return a string" >))
                                         end)
                                     
                                     end)
                              
                              (return ($builtIn load (program any v_2 v_3))))
                             
                             end)
                           end) ()))
   ]

  ; Default case
  [(δ (load v_1 v_2 v_3 v_4 θ))
   (δ (error String))
   
   (where String ,(string-append "bad argument #1 (function expected, got "
                                 (term v_1)
                                 ")"))]

  
  ;                                                                  
  ;   ;;;                          ;     ;;     ;     ;;;            
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
  [(δ (loadfile String_1 v_2 v_3 θ))
   (δ (load String_2 nil nil nil θ))

   (where String_2 ,(file->string (term String_1)))]

  [(δ (loadfile v_1 v_2 v_3 θ))
   (δ (error String_1))

   (where String_1 (δ (type v_1 θ)))

   (where String_2 ,(string-append "bad argument #1 (string expected, got "
                                   (term String_1)
                                   ")"))]
  
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
  
  ; Bad argument #1
  [(δ (next v_1 v_2 θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v_1 θ)))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 to 'next' (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {the first argument is a pointer to a table}
  
  ; nil index, non empty table
  [(δ (next objref nil θ))
   (< v_1 v_2 >)
   
   (where ((\{ (\[ v_1 \] = v_2) field ... \}) any) (derefTheta θ objref))]
  
  ; nil index, empty table
  [(δ (next objref nil θ))
   (< nil >)]
  
  ; Not the last index
  [(δ (next objref v_1 θ))
   (< v_3 v_4 >)
   
   (where ((\{ field ... \}) any) (derefTheta θ objref))
   
   (where ((\[ any_1 \] = any_2)) (extractField (field ...) v_1))
   
   (where (\{ field_1 ... (\[ any_1 \] = any_2) (\[ v_3 \] = v_4) field_2 ... \}) (\{ field ... \}))
   ]
  
  ; Last index
  [(δ (next objref v_1 θ))
   (< nil >)
   
   (where ((\{ field ... \}) any) (derefTheta θ objref))
   
   (where ((\[ any_1 \] = any_2)) (extractField (field ...) v_1))
   
   (where (\{ field_1 ... (\[ any_1 \] = any_2) \}) (\{ field ... \}))]
  
  ; Invalid key.
  [(δ (next objref v θ))
   (δ (error "invalid key to 'next'"))]
  
  
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

  ; Custom iterator, provided by the metatable
  [(δ (pairs objref θ))
   ((function $pairsCustomIter ()
              (local (v1 v2 v3) = ((any (objref)))
                in
                (return (< v1 v2 v3 >))
                end)
              end) ())
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "table"))
   
   (where any (indexMetaTable objref "__pairs" θ))
   
   (side-condition (not (equal? (term any)
                                (term nil))))]

  ; Default iterator: next
  [(δ (pairs objref θ))
   (< (function $next (table index)
                (return ($builtIn next (table index)))
                end) objref nil >)
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "table"))
   
   (where nil (indexMetaTable objref "__pairs" θ))]
  
  [(δ (pairs v θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v θ)))
   
   (where String_2 ,(string-append "bad argument #1 (table expected, got "
                                   (term String_1)
                                   ")"))]
  
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
  
  [(δ (pcall v v_1 ...))
   (((v (v_1 ...)))ProtectedMode)]

  
  ;                                          
  ;                     ;                    
  ;                                          
  ;                                     ;    
  ;                                     ;    
  ;   ;;;;;    ;;;;   ;;;     ; ;;;   ;;;;;; 
  ;   ;;  ;;   ;;  ;    ;     ;;   ;    ;    
  ;   ;    ;   ;        ;     ;    ;    ;    
  ;   ;    ;   ;        ;     ;    ;    ;    
  ;   ;    ;   ;        ;     ;    ;    ;    
  ;   ;;  ;;   ;        ;     ;    ;    ;    
  ;   ;;;;;    ;      ;;;;;   ;    ;     ;;; 
  ;   ;                                      
  ;   ;                                      
  ;   ;                                      
  [(δ (print v ...))
   ,(print (term (< v ... >)))]
  
  ;                                                                  
  ;                                                           ;;;    
  ;                                                             ;    
  ;                                                             ;    
  ;                                                             ;    
  ;    ;;;;     ;;;  ;      ;  ;;;;    ;;;;;  ;    ;    ;;;     ;    
  ;    ;;  ;   ;   ; ;      ; ;;  ;;  ;;  ;;  ;    ;   ;   ;    ;    
  ;    ;           ;  ; ;; ;  ;    ;  ;    ;  ;    ;       ;    ;    
  ;    ;       ;;;;;  ; ;; ;  ;;;;;;  ;    ;  ;    ;   ;;;;;    ;    
  ;    ;      ;    ;  ; ;; ;  ;       ;    ;  ;    ;  ;    ;    ;    
  ;    ;      ;   ;;   ;  ;   ;;   ;  ;;  ;;  ;   ;;  ;   ;;    ;    
  ;    ;       ;;; ;   ;  ;    ;;;;    ;;; ;   ;;; ;   ;;; ;     ;;; 
  ;                                        ;                         
  ;                                        ;                         
  ;                                        ;                         
  
  [(δ (rawequal v_1 v_2))
   (δ (== v_1 v_2))]
  
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
  
  ; Bad argument #1
  [(δ (rawget v_1 v_2 θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v_1 θ)))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append 
                     "bad argument #1 (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {objref points to a table}
  [(δ (rawget objref v_1 θ))
   any_3
   
   ; Extract the fields
   (where ((\{ field ... \}) any_1) (derefTheta θ objref))
   
   (where ((\[ any_2 \] = any_3)) (extractField (field ...) v_1))]
  
  ; {v isn't a key of the table pointed by objref}
  [(δ (rawget objref v θ))
   nil]
  
  
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
  ; Bad argument #1 to 'rawlen'
  [(δ (rawlen v θ))
   (δ (error "bad argument #1 to 'rawlen'(table or string expected)"))
   
   (where string (δ (type v θ)))
   
   (side-condition (not (or (equal? (term string)
                                    "table")
                            
                            (equal? (term string)
                                    "string"))))]
  
  [(δ (rawlen String θ))
   (δ (\# String))]
  
  [(δ (rawlen objref θ))
   (δ (\# tableconstructor))
   
   (where tableconstructor (getTable (derefTheta θ objref)))]
  
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
  
  ; Bad argument #1 to 'rawset'
  [(δ (rawset v_1 v_2 v_3 θ))
   (θ (δ (error String_2)))
   
   (where String_1 (δ (type v_1 θ)))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; Bad argument #2 to 'rawset'
  [(δ (rawset v_1 nil v_3 θ))
   (θ (δ (error String_2)))
   
   (where String_2 "table index is nil")]
  
  [(δ (rawset v_1 +nan.0 v_3 θ))
   (θ (δ (error String_2)))
   
   (where String_2 "table index is NaN")]
  
  ; Delete field
  [(δ (rawset objref v_1 nil θ_1))
   (θ_2 objref)
   
   (where ((\{ field ... \}) any) (derefTheta θ_1 objref))
   
   (where ((\[ any_2 \] = any_3)) (extractField (field ...) v_1))
   
   (where (\{ field_1 ... (\[ any_2 \] = any_3) field_2 ... \}) (\{ field ... \}))
   
   (where θ_2 (thetaAlter θ_1
                          objref
                          (\{ field_1 ... field_2 ... \})))]
  
  ; v_2 != nil
  [(δ (rawset objref v_1 v_2 θ_1))
   (θ_2 objref)
   
   (where ((\{ field ... \}) any_1) (derefTheta θ_1 objref))
   
   (where ((\[ any_2 \] = any_3)) (extractField (field ...) v_1))
   
   (where (\{ field_1 ... (\[ any_2 \] = any_3) field_2 ... \}) (\{ field ... \}))
   
   (where θ_2 (thetaAlter θ_1
                          objref
                          (\{ field_1 ... (\[ any_2 \] = v_2) field_2 ... \})))]
  
  ; Add a new field 
  [(δ (rawset objref v_1 v_2 θ_1))
   (θ_2 objref)
   
   (where ((\{ field ... \}) any) (derefTheta θ_1 objref))
   
   (where θ_2 (thetaAlter θ_1
                          objref
                          (\{ (\[ v_1 \] = v_2) field ... \})))]
  
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
  ; Index out of range
  [(δ (select Number v ...))
   (δ (error "bad argument #1 to 'select' (index out of range)"))
   
   (where Number_2 ,(* -1 (length (term (v ...)))))
   
   (side-condition (or (< (term Number) (term Number_2))
                       (= (term Number) 0)))]
  
  ; Positive index, in the range [1;(length (sv ...))] 
  [(δ (select Number v ...))
   ,(append (term (< ))
            (list-tail (term (v ...)) (term Number_2))
            (term ( >)))
   
   (side-condition (and (<= (term Number) (length (term (v ...))))
                        (<= 1 (term Number))))
   
   (where Number_2 ,(- (exact-floor (term Number)) 1))]
  
  ; Positive index > (length (sv ...))
  [(δ (select Number v ...))
   (< >)
   
   (side-condition (> (term Number) (length (term (v ...)))))]
  
  ; Negative index
  [(δ (select Number v ...))
   ,(append (term (< ))
            (list-tail (term (v ...)) (term Number_3))
            (term ( >)))
   
   (where Number_2 ,(* -1 (length (term (v ...)))))
   
   (side-condition (and (<= (term Number_2) (term Number))
                        (<= (term Number) -1)))
   
   (where Number_3 ,(+ (length (term (v ...)))
                       (exact-floor (term Number))))]
  
  ; Obtain the total number of actual arguments received.
  [(δ (select "#" v ...))
   (< any >)
   
   (where any ,(length (term (v ...))))]
  
  [(δ (select '#' v ...))
   (< any >)
   
   (where any ,(length (term (v ...))))]
  
  ; Default case
  [(δ (select v_1 v ...))
   (δ (error "bad argument #1 to 'select' (number expected)"))]
  
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
  
  ; Bad argument #1.
  [(δ (setmetatable v_1 v_2 θ))
   (θ (δ (error String_2)))
   
   (where String_1 (δ (type v_1 θ)))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append
                     "bad argument #1 to 'setmetatable' (table expected, got "
                     (term String_1)
                     ")"))]
  
  ; {objref is a pointer to a table}
  ; Bad argument #2.
  [(δ (setmetatable objref v θ))
   (θ (δ (error "bad argument #2 to 'setmetatable' (nil or table expected)")))
   
   (where String_1 (δ (type v θ)))
   
   (side-condition (not (or (equal? (term String_1)
                                    "table")
                            
                            (equal? (term String_1)
                                    "nil"))))]
  
  ; {v points to a table or is nil}
  
  ; Protected meta-table
  [(δ (setmetatable objref v θ))
   (θ (δ (error "cannot change a protected metatable")))
   
   (side-condition (term (protectedMetaTable? objref θ)))]
  
  ; Non protected meta-table
  [(δ (setmetatable objref v θ_1))
   (θ_2 objref)
   
   (where (any_1 any_2) (derefTheta θ_1 objref))
   
   (where θ_2 (thetaAlter θ_1 objref (any_1 v)))]
  
  
  ;                                                                  
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
  ; Decimal number
  [(δ (tonumber String nil))
   e
   
   ; Convert string following the rules of the lexer, as said by the semantics.
   ; However, lexer alone will not suffice: for example, in case of malformed strings beginning
   ; with a correct string representation of numbers.
   (where ((~ENV) = (e)) ,(with-handlers ([exn:fail?
                                           (λ (e) #f)])
                            ((λ ()
                               ; To use the parser, we need to feed it with an statement...
                               ; NOTE: we append String directly. Then, the conversion to a number
                               ; is done by the lexer/parser.
                               (parse-this (string-append "_ENV = " (term String))
                                           #f
                                           (void))))))

   
   (side-condition (or
                    ; Positive or negative decimal number
                    (redex-match core-lang
                                 Number
                                 (term e))

                    (redex-match core-lang
                                 (- Number)
                                 (term e))

                    ; Hexadecimal number with binary exponent
                    (redex-match core-lang
                                 (e_1 binop e_2)
                                 (term e))

                    (redex-match core-lang
                                 (- (e_1 binop e_2))
                                 (term e))))
   ]
  
  ; When called with a base (Number), then the first argument should be a string
  ; to be interpreted as an integer numeral in that base
  [(δ (tonumber String Number))
   any_2

   (where any_1 ,(string->number (term String) (term Number)))

   (where any_2 ,(if (equal? (term any_1) #f)
                     (term nil)
                     (term any_1)))]

  ; NOTE: Lua's reference manual said that if the argument is already a number,
  ; then tonumber return that number. But when given an hexadecimal number,
  ; it converts it into a decimal. In our mechanization that isn't a problem,
  ; because all numbers are converted to decimal.
  [(δ (tonumber Number_1 Number_2))
   Number_1]

  [(δ (tonumber Number_1 nil))
   Number_1]

  [(δ (tonumber v_1 v_2))
   (δ (error v_1 v_2))

   (where Sring ,(string-append "bad argument #1 (string expected)"))]
  
  ; Default case
  [(δ (tonumber v_1 v_2))
   nil]
  
  
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
  ; table objref doesn't have an associated metatable or its meta-table doesn't have a
  ; field ("__tostring" = any)
  [(δ (tostring objref θ))
   String
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "table"))
   
   (where nil (indexMetaTable objref "__tostring" θ))
   
   (where String ,(string-append "table: "
                                 (~a (term objref))))]
  
  ; table objref has an associated metatable, with field ("__tostring" = any)
  [(δ (tostring objref θ))
   (any (objref))
   
   (side-condition (equal? (term (δ (type objref θ)))
                           "table"))
   
   (where any (indexMetaTable objref "__tostring" θ))]
  
  ; To implement the behaviour of Lua's coercion:
  ; 1.0 .. 1.0 = "11"
  [(δ (tostring Number θ))
   ,(~a (inexact->exact (term Number)))
   
   (where objref (getMetaTableRef Number))
   
   (side-condition (term (refBelongsToTheta? objref θ)))
   
   (where nil (indexMetaTable objref "__tostring" θ))
   
   (side-condition (equal? (remainder (* (term Number) 10) 10)
                           0.0))]
  
  [(δ (tostring Number θ))
   ,(~a (inexact->exact (term Number)))
   
   (side-condition (equal? (remainder (* (term Number) 10) 10)
                           0.0))]
  
  ; v doesn't have an associated metatable or its meta-table doesn't have a
  ; field ("__tostring" = any)
  [(δ (tostring v θ))
   String_2
   
   (where objref (getMetaTableRef v))
   
   (side-condition (term (refBelongsToTheta? objref θ)))
   
   (where nil (indexMetaTable objref "__tostring" θ))
   
   (where String_1 (δ (type v θ)))
   
   (where String_2 ,(string-append (term String_1)
                                   (~a (term v))))]
  
  ; v has an associated metatable, with field ("__tostring" = any)
  [(δ (tostring v θ))
   (any (v))
   
   (where objref (getMetaTableRef v))
   
   (side-condition (term (refBelongsToTheta? objref θ)))
   
   (where any (indexMetaTable objref "__tostring" θ))]
  
  ; Default case: racket/format conversion
  [(δ (tostring v θ))
   ; From racket/format
   ,(~a (term v))]
  
  
  ;                                  
  ;                                  
  ;                                  
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
  
  [(δ (type Number θ))
   ,(term "number")]
  
  [(δ (type nil θ))
   ,(term "nil")]
  
  [(δ (type Boolean θ))
   ,(term "boolean")]
  
  [(δ (type String θ))
   ,(term "string")]
  
  [(δ (type objref θ))
   ,(term "function")
   
   (where any (derefTheta θ objref))
   
   (side-condition (redex-match core-lang
                                functiondef
                                (term any)))]
  
  ; Default case
  [(δ (type objref θ))
   ,(term "table")]

  
  ;                                                  
  ;                                   ;;;     ;;;    
  ;                                     ;       ;    
  ;                                     ;       ;    
  ;                                     ;       ;    
  ;   ;;  ;;  ;;;;;     ;;;     ;;;     ;       ;    
  ;    ;  ;   ;;  ;;   ;   ;   ;   ;    ;       ;    
  ;     ;;    ;    ;  ;            ;    ;       ;    
  ;     ;;    ;    ;  ;        ;;;;;    ;       ;    
  ;     ;;    ;    ;  ;       ;    ;    ;       ;    
  ;    ;  ;   ;;  ;;   ;   ;  ;   ;;    ;       ;    
  ;   ;;  ;;  ;;;;;     ;;;    ;;; ;     ;;;     ;;; 
  ;           ;                                      
  ;           ;                                      
  ;           ;                                      
  [(δ (xpcall v_1 v_2 v_3 ...))
   (((v_1 (v_3 ...)))ProtectedMode v_2)]

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

  
  ;                          
  ;           ;              
  ;           ;              
  ;           ;              
  ;           ;              
  ;     ;;;   ;;;;;    ;;;;  
  ;    ;   ;  ;;  ;;  ;    ; 
  ;        ;  ;    ;  ;      
  ;    ;;;;;  ;    ;   ;;;;  
  ;   ;    ;  ;    ;       ; 
  ;   ;   ;;  ;;  ;;  ;    ; 
  ;    ;;; ;  ;;;;;    ;;;;  
  ;                          
  ;                          
  ;                          
  [(δ (math.abs Number))
   ,(abs (term Number))]

  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;     ;;;    ;;;;    ;;;;  
  ;    ;   ;   ;   ;  ;;  ;;  ;    ; 
  ;        ;  ;       ;    ;  ;      
  ;    ;;;;;  ;       ;    ;   ;;;;  
  ;   ;    ;  ;       ;    ;       ; 
  ;   ;   ;;   ;   ;  ;;  ;;  ;    ; 
  ;    ;;; ;    ;;;    ;;;;    ;;;;  
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.acos Number))
   ,(acos (term Number))]
  
  [(δ (math.acos v))
   (δ (error "bad argument #1 (number expected)"))]
  
  ;                                  
  ;                     ;            
  ;                                  
  ;                                  
  ;                                  
  ;     ;;;    ;;;;   ;;;     ; ;;;  
  ;    ;   ;  ;    ;    ;     ;;   ; 
  ;        ;  ;         ;     ;    ; 
  ;    ;;;;;   ;;;;     ;     ;    ; 
  ;   ;    ;       ;    ;     ;    ; 
  ;   ;   ;;  ;    ;    ;     ;    ; 
  ;    ;;; ;   ;;;;   ;;;;;   ;    ; 
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.asin Number))
   ,(asin (term Number))]
  
  [(δ (math.asin v))
   (δ (error "bad argument #1 (number expected)"))]
  
  ;                                  
  ;                                  
  ;                                  
  ;             ;                    
  ;             ;                    
  ;     ;;;   ;;;;;;    ;;;   ; ;;;  
  ;    ;   ;    ;      ;   ;  ;;   ; 
  ;        ;    ;          ;  ;    ; 
  ;    ;;;;;    ;      ;;;;;  ;    ; 
  ;   ;    ;    ;     ;    ;  ;    ; 
  ;   ;   ;;    ;     ;   ;;  ;    ; 
  ;    ;;; ;     ;;;   ;;; ;  ;    ; 
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.atan Number))
   ,(atan (term Number))]
  
  [(δ (math.atan v))
   (δ (error "bad argument #1 (number expected)"))]

  
  ;                                  
  ;                     ;     ;;;    
  ;                             ;    
  ;                             ;    
  ;                             ;    
  ;     ;;;    ;;;;   ;;;       ;    
  ;    ;   ;  ;;  ;;    ;       ;    
  ;   ;       ;    ;    ;       ;    
  ;   ;       ;;;;;;    ;       ;    
  ;   ;       ;         ;       ;    
  ;    ;   ;  ;;   ;    ;       ;    
  ;     ;;;    ;;;;   ;;;;;      ;;; 
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.ceil Number))
   ,(ceiling (term Number))]
  
  [(δ (math.ceil v))
   (δ (error "bad argument #1 (number expected)"))]
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;     ;;;    ;;;;    ;;;;  
  ;    ;   ;  ;;  ;;  ;    ; 
  ;   ;       ;    ;  ;      
  ;   ;       ;    ;   ;;;;  
  ;   ;       ;    ;       ; 
  ;    ;   ;  ;;  ;;  ;    ; 
  ;     ;;;    ;;;;    ;;;;  
  ;                          
  ;                          
  ;                          
  [(δ (math.cos Number))
   ,(cos (term Number))]
  
  [(δ (math.cos v))
   (δ (error "bad argument #1 (number expected)"))]

  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;     ;;;    ;;;;    ;;;;   ; ;;;  
  ;    ;   ;  ;;  ;;  ;    ;  ;;   ; 
  ;   ;       ;    ;  ;       ;    ; 
  ;   ;       ;    ;   ;;;;   ;    ; 
  ;   ;       ;    ;       ;  ;    ; 
  ;    ;   ;  ;;  ;;  ;    ;  ;    ; 
  ;     ;;;    ;;;;    ;;;;   ;    ; 
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.cosh Number))
   ,(cosh (term Number))]
  
  [(δ (math.cosh v))
   (δ (error "bad argument #1 (number expected)"))]
  
  ;                          
  ;        ;                 
  ;        ;                 
  ;        ;                 
  ;        ;                 
  ;    ;;;;;   ;;;;    ;;;;; 
  ;   ;;  ;;  ;;  ;;  ;;  ;; 
  ;   ;    ;  ;    ;  ;    ; 
  ;   ;    ;  ;;;;;;  ;    ; 
  ;   ;    ;  ;       ;    ; 
  ;   ;;  ;;  ;;   ;  ;;  ;; 
  ;    ;;;;;   ;;;;    ;;; ; 
  ;                        ; 
  ;                    ;   ; 
  ;                     ;;;  
  [(δ (math.deg Number))
   ,(radians->degrees (term Number))]
  
  [(δ (math.deg v))
   (δ (error "bad argument #1 (number expected)"))]

  
  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;   ;;  ;;   ;  ;   ;;  ;; 
  ;   ;    ;    ;;    ;    ; 
  ;   ;;;;;;    ;;    ;    ; 
  ;   ;         ;;    ;    ; 
  ;   ;;   ;   ;  ;   ;;  ;; 
  ;    ;;;;   ;;  ;;  ;;;;;  
  ;                   ;      
  ;                   ;      
  ;                   ;      
  [(δ (math.exp Number))
   ,(exp (term Number))]
  
  [(δ (math.exp v))
   (δ (error "bad argument #1 (number expected)"))]

  ;                                          
  ;      ;;   ;;;                            
  ;     ;       ;                            
  ;     ;       ;                            
  ;     ;       ;                            
  ;   ;;;;;     ;      ;;;;    ;;;;    ;;;;  
  ;     ;       ;     ;;  ;;  ;;  ;;   ;;  ; 
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;    ;  ;    ;   ;     
  ;     ;       ;     ;;  ;;  ;;  ;;   ;     
  ;     ;        ;;;   ;;;;    ;;;;    ;     
  ;                                          
  ;                                          
  ;                                          
  
  [(δ (math.floor Number))
   ,(floor (term Number))]
  
  
  ;                                  
  ;      ;;                        ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;     ;                          ; 
  ;   ;;;;;   ;;;;;;;  ;;;;    ;;;;; 
  ;     ;     ;  ;  ; ;;  ;;  ;;  ;; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;    ;  ;    ; 
  ;     ;     ;  ;  ; ;;  ;;  ;;  ;; 
  ;     ;     ;  ;  ;  ;;;;    ;;;;; 
  ;                                  
  ;                                  
  ;                                  
  
  [(δ (math.fmod Number_1 Number_2))
   ,(exact-floor (remainder (term Number_1) (term Number_2)))]
  

  
  ;                          
  ;   ;;;                    
  ;     ;                    
  ;     ;                    
  ;     ;                    
  ;     ;      ;;;;    ;;;;; 
  ;     ;     ;;  ;;  ;;  ;; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;;  ;;  ;;  ;; 
  ;      ;;;   ;;;;    ;;; ; 
  ;                        ; 
  ;                    ;   ; 
  ;                     ;;;
  [(δ (math.log Number nil))
   ,(log (term Number))]

  [(δ (math.log Number_1 Number_2))
   (δ (/ (δ (math.log Number_1 nil))
         (δ (math.log Number_2 nil))))]

  ; default case
  [(δ (math.log v_1 v_2))
   (δ (error "bad arguments (numbers expected)"))]

  ;                          
  ;                          
  ;                          
  ;                          
  ;                          
  ;   ;;;;;;;   ;;;   ;;  ;; 
  ;   ;  ;  ;  ;   ;   ;  ;  
  ;   ;  ;  ;      ;    ;;   
  ;   ;  ;  ;  ;;;;;    ;;   
  ;   ;  ;  ; ;    ;    ;;   
  ;   ;  ;  ; ;   ;;   ;  ;  
  ;   ;  ;  ;  ;;; ;  ;;  ;; 
  ;                          
  ;                          
  ;                          
  [(δ (math.max Number ...))
   ,(foldr (λ (nmbr accum) (max nmbr accum))
           -inf.0
           (term (Number ...)))]

  
  ;                                  
  ;                        ;     ;;  
  ;                        ;    ;    
  ;                        ;    ;    
  ;                        ;    ;    
  ;   ;;;;;;;  ;;;;    ;;;;;  ;;;;;  
  ;   ;  ;  ; ;;  ;;  ;;  ;;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;    ;  ;    ;    ;    
  ;   ;  ;  ; ;;  ;;  ;;  ;;    ;    
  ;   ;  ;  ;  ;;;;    ;;;;;    ;    
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.modf Number))
   ,(- (term Number) (exact-truncate (term Number)))]

  
  ;                          
  ;                        ; 
  ;                        ; 
  ;                        ; 
  ;                        ; 
  ;    ;;;;     ;;;    ;;;;; 
  ;    ;;  ;   ;   ;  ;;  ;; 
  ;    ;           ;  ;    ; 
  ;    ;       ;;;;;  ;    ; 
  ;    ;      ;    ;  ;    ; 
  ;    ;      ;   ;;  ;;  ;; 
  ;    ;       ;;; ;   ;;;;; 
  ;                          
  ;                          
  ;                          

  [(δ (math.rad Number))
   ,(degrees->radians (term Number))]
  
  [(δ (math.rad v))
   (δ (error "bad argument #1 (number expected)"))]
  
  ;                          
  ;             ;            
  ;                          
  ;                          
  ;                          
  ;    ;;;;   ;;;     ; ;;;  
  ;   ;    ;    ;     ;;   ; 
  ;   ;         ;     ;    ; 
  ;    ;;;;     ;     ;    ; 
  ;        ;    ;     ;    ; 
  ;   ;    ;    ;     ;    ; 
  ;    ;;;;   ;;;;;   ;    ; 
  ;                          
  ;                          
  ;                          
  
  [(δ (math.sin Number))
   ,(sin (term Number))]
  
  [(δ (math.sin v))
   (δ (error "bad argument #1 (number expected)"))]

  
  ;                                  
  ;             ;             ;      
  ;                           ;      
  ;                           ;      
  ;                           ;      
  ;    ;;;;   ;;;     ; ;;;   ; ;;;  
  ;   ;    ;    ;     ;;   ;  ;;   ; 
  ;   ;         ;     ;    ;  ;    ; 
  ;    ;;;;     ;     ;    ;  ;    ; 
  ;        ;    ;     ;    ;  ;    ; 
  ;   ;    ;    ;     ;    ;  ;    ; 
  ;    ;;;;   ;;;;;   ;    ;  ;    ; 
  ;                                  
  ;                                  
  ;
  [(δ (math.sinh Number))
   ,(sinh (term Number))]
  
  [(δ (math.sinh v))
   (δ (error "bad argument #1 (number expected)"))]


  
  ;                                  
  ;                                  
  ;                                  
  ;                             ;    
  ;                             ;    
  ;    ;;;;    ;;;;;   ;;;;   ;;;;;; 
  ;   ;    ;  ;;  ;;   ;;  ;    ;    
  ;   ;       ;    ;   ;        ;    
  ;    ;;;;   ;    ;   ;        ;    
  ;        ;  ;    ;   ;        ;    
  ;   ;    ;  ;;  ;;   ;        ;    
  ;    ;;;;    ;;; ;   ;         ;;; 
  ;                ;                 
  ;                ;                 
  ;                ;                 
  [(δ (math.sqrt Number))
   ,(sqrt (term Number))]
  
  [(δ (math.sqrt v))
   (δ (error "bad argument #1 (number expected)"))]

  
  ;                          
  ;                          
  ;                          
  ;     ;                    
  ;     ;                    
  ;   ;;;;;;    ;;;   ; ;;;  
  ;     ;      ;   ;  ;;   ; 
  ;     ;          ;  ;    ; 
  ;     ;      ;;;;;  ;    ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;   ;;  ;    ; 
  ;      ;;;   ;;; ;  ;    ; 
  ;                          
  ;                          
  ;                          
  [(δ (math.tan Number))
   ,(tan (term Number))]
  
  [(δ (math.tan v))
   (δ (error "bad argument #1 (number expected)"))]

  
  ;                                  
  ;                           ;      
  ;                           ;      
  ;     ;                     ;      
  ;     ;                     ;      
  ;   ;;;;;;    ;;;   ; ;;;   ; ;;;  
  ;     ;      ;   ;  ;;   ;  ;;   ; 
  ;     ;          ;  ;    ;  ;    ; 
  ;     ;      ;;;;;  ;    ;  ;    ; 
  ;     ;     ;    ;  ;    ;  ;    ; 
  ;     ;     ;   ;;  ;    ;  ;    ; 
  ;      ;;;   ;;; ;  ;    ;  ;    ; 
  ;                                  
  ;                                  
  ;                                  
  [(δ (math.tanh Number))
   ,(tanh (term Number))]
  
  [(δ (math.tanh v))
   (δ (error "bad argument #1 (number expected)"))]
  
  
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
  ;        ;                         
  ;        ;                         
  ;        ;                         
  ;        ;                         
  ;    ;;;;;  ;    ;  ;;;;;;; ;;;;;  
  ;   ;;  ;;  ;    ;  ;  ;  ; ;;  ;; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;    ;  ;    ;  ;  ;  ; ;    ; 
  ;   ;;  ;;  ;   ;;  ;  ;  ; ;;  ;; 
  ;    ;;;;;   ;;; ;  ;  ;  ; ;;;;;  
  ;                           ;      
  ;                           ;      
  ;                           ;
  [(δ (string.dump v θ))
   any
   
   (where String (δ (type v θ)))
   
   (where any ,(if (equal? (term String)
                           "function")
                   
                   (str-flatten (term (derefTheta θ v)))
                   
                   (term (δ (error ,(string-append "bad argument #1 (function expected, got "
                                                   (term String)
                                                   ")"))))))
   ]

  
  ;                          
  ;   ;;;                    
  ;     ;                    
  ;     ;                    
  ;     ;                    
  ;     ;      ;;;;   ; ;;;  
  ;     ;     ;;  ;;  ;;   ; 
  ;     ;     ;    ;  ;    ; 
  ;     ;     ;;;;;;  ;    ; 
  ;     ;     ;       ;    ; 
  ;     ;     ;;   ;  ;    ; 
  ;      ;;;   ;;;;   ;    ; 
  ;                          
  ;                          
  ;                          
  [(δ (string.len String))
   (δ (\# String))
   ]
  
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
  [(δ (string.rep String Number nil))
   any
   
   (where any ,(foldr (λ (str accum) (term (δ (.. ,str ,accum))))
                      (term String)
                      (build-list (- (term Number) 1)
                                  (λ (nmbr) (term String)))))
   ]

  [(δ (string.rep String_1 Number String_2))
   (δ (.. any String_1))
   
   (where any ,(foldr (λ (str accum) (term (δ (.. (δ (.. String_1 String_2)) ,accum))))
                      (term (δ (.. String_1 String_2)))
                      (build-list (- (term Number) 2)
                                  (λ (nmbr) (term String_1)))))
   ]

  
  ;                                                          
  ;                                                          
  ;                                                          
  ;                                                          
  ;                                                          
  ;    ;;;;    ;;;;   ;    ;   ;;;;    ;;;;    ;;;;    ;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;;  ;;  ;;   ;;  ;  ;    ;  ;;  ;; 
  ;    ;      ;    ;   ;  ;   ;    ;   ;      ;       ;    ; 
  ;    ;      ;;;;;;   ;  ;   ;;;;;;   ;       ;;;;   ;;;;;; 
  ;    ;      ;        ;;;;   ;        ;           ;  ;      
  ;    ;      ;;   ;    ;;    ;;   ;   ;      ;    ;  ;;   ; 
  ;    ;       ;;;;     ;;     ;;;;    ;       ;;;;    ;;;;  
  ;                                                          
  ;                                                          
  ;                                                          

  [(δ (string.reverse String))
   ,(list->string (reverse (string->list (term String))))]
  
  ;                          
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;                   ;      
  ;    ;;;;   ;    ;  ;;;;;  
  ;   ;    ;  ;    ;  ;;  ;; 
  ;   ;       ;    ;  ;    ; 
  ;    ;;;;   ;    ;  ;    ; 
  ;        ;  ;    ;  ;    ; 
  ;   ;    ;  ;   ;;  ;;  ;; 
  ;    ;;;;    ;;; ;  ;;;;;  
  ;                          
  ;
  ; Correction of indices
  ; Number_1 < 0
  [(δ (string.sub String Number_1 Number_2))
   (δ (string.sub String 1 Number_2))

   (side-condition (< (term Number_1)
                      0))]
  
  ; If Number_2 is greater than the string length, it is corrected to that length
  [(δ (string.sub String Number_1 Number_2))
   (δ (string.sub String Number_1 (δ (\# String))))

   (side-condition (< (term (δ (\# String)))
                      (exact-floor (term Number_2))))]

  ; If, after these corrections, Number_1 is greater than Number_2, the function
  ; returns the empty string. 
  [(δ (string.sub String Number_1 Number_2))
   ""

   (side-condition (< (term Number_2)
                      (term Number_1)))]

  ; Normal case
  [(δ (string.sub String Number_1 Number_2))
   (substring (term String)
              (- (exact-floor (term Number_1)) 1)
              (exact-floor (term Number_2)))]
  
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
  ;                                                  
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;     ;;;    ;;;;   ; ;;;     ;;;     ;;;   ;;;;;; 
  ;    ;   ;  ;;  ;;  ;;   ;   ;   ;   ;   ;    ;    
  ;   ;       ;    ;  ;    ;  ;            ;    ;    
  ;   ;       ;    ;  ;    ;  ;        ;;;;;    ;    
  ;   ;       ;    ;  ;    ;  ;       ;    ;    ;    
  ;    ;   ;  ;;  ;;  ;    ;   ;   ;  ;   ;;    ;    
  ;     ;;;    ;;;;   ;    ;    ;;;    ;;; ;     ;;; 
  ;                                                  
  ;                                                  
  ; Missing parameters
  [(δ (table.concat objref nil v_1 v_2 θ))
   (δ (table.concat objref "" v_1 v_2 θ))]

  [(δ (table.concat objref String nil v θ))
   (δ (table.concat objref String 1 v θ))]

  [(δ (table.concat objref String v nil θ))
   (δ (table.concat objref
                    String
                    v
                    (δ (\# evaluatedtable))
                    θ))

   (where (evaluatedtable any) (derefTheta θ objref))]
  
  ; Simpler case
  [(δ (table.concat objref String Number_1 Number_2 θ))
   any_3

   (where String_1 (δ (type objref θ)))
   
   (side-condition (equal? (term String_1)
                           "table"))

   ; Quantity of fields to be accessed
   (where Number_3 ,(+ (- (term Number_2) (term Number_1))
                       1))

   (side-condition (> (term Number_3)
                      0))

   ; Construct a list of indexing operations, with numeric indices from
   ; Number_1 to Number_2
   (where (any_1 any_2 ...) ,(build-list (inexact->exact (term Number_3))
                                         (λ (nmbr) (term (objref \[ ,(+ nmbr (term Number_1)) \])))))

   ; Apply string concatenation between each field, separated by String
   (where any_3 ,(foldl (λ (field accum) (term (,accum .. (String .. ,field))))
                        (term any_1)
                        (term (any_2 ...))))]

  ; Default case
  [(δ (table.concat objref String Number_1 Number_2 θ))
   ""]
  
  ;                                                  
  ;     ;                                            
  ;                                                  
  ;                                             ;    
  ;                                             ;    
  ;   ;;;     ; ;;;    ;;;;    ;;;;    ;;;;   ;;;;;; 
  ;     ;     ;;   ;  ;    ;  ;;  ;;   ;;  ;    ;    
  ;     ;     ;    ;  ;       ;    ;   ;        ;    
  ;     ;     ;    ;   ;;;;   ;;;;;;   ;        ;    
  ;     ;     ;    ;       ;  ;        ;        ;    
  ;     ;     ;    ;  ;    ;  ;;   ;   ;        ;    
  ;   ;;;;;   ;    ;   ;;;;    ;;;;    ;         ;;; 
  ;                                                  
  ;                                                  
  ;
  [(δ (table.insert objref nil v θ_1))
   (θ_2 (< >))
   
   (where ((\{ field ... \}) any) (derefTheta θ_1 objref))

   (where Number (δ (\# (\{ field ... \}))))

   (where θ_2 (thetaAlter θ_1
                          objref
                          (\{ field ... (\[  Number \] = v) \})))]
  
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
  
  
  [(δ (table.pack v ...))
   any_3
   
   (where Number ,(length (term (v ...))))
   
   ; Take the list of keys (naturals starting from 1),
   ; the list of values received, and construct
   ; table fields taking 2 elements, one from each list.
   (where any ,(map (λ (number value)
                      (append (term (\[ ))
                              (list number)
                              (term (\] = ))
                              (list value)))
                    ; Build the list of keys
                    (build-list (term Number) (λ (nmbr) (+ nmbr 1)))
                    ; and pass the values
                    (term (v ...))))
   
   ; Filter nil-valued fields
   (where any_2 ,(filter (λ (field)
                           (not (redex-match core-lang
                                             (\[ v \] = nil)
                                             field)))
                         (term any)))
   
   ; Add the parenthesis and the field "n"
   ; NOTE: it seems that the implementation counts even the nil-valued
   ; fields.
   (where any_3 ,(append (term (\{ ))
                         (term any_2)
                         (term ((\[ "n" \] = Number)))
                         (term ( \}))))]
  
  
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
  [(δ (table.unpack objref v_1 v_2 θ))
   any_2
   
   (where (evaluatedtable any_1) (derefTheta θ objref))

   ; Set range of indexes. v_1 and v_2 should be nil or a number 
   (where Number_1 ,(if (not (equal? (term v_1)
                                     (term nil)))
                        (term v_1)
                        1) ; Default first index
          )
   
   (where Number_2 ,(if (not (equal? (term v_2)
                                     (term nil)))
                        (term v_2)
                        (term (δ (\# evaluatedtable))) ; Default last index
                        ))

   ; Construct a tuple of table indexing expressions
   (where any_2 ,(append (term (< ))

                         (map (λ (index)
                                (append (term (objref \[ ))
                                        (term (,index))
                                        (term (\]))))
                              
                              (range (exact-floor (term Number_1))
                                     (+ (exact-floor (term Number_2)) 1)))

                         (term ( >))))]

  ; erroneous cases
  [(δ (table.unpack v_1 v_2 v_3 θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v_1 θ)))
   
   (side-condition (not (equal? (term String_1)
                                "table")))
   
   (where String_2 ,(string-append "bad argument #1 (table expected, got "
                                   (term String_1)
                                   ")"))]
  
  [(δ (table.unpack v_1 v_2 v_3 θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v_2 θ)))
   
   (side-condition (not (equal? (term String_1)
                                "number")))
   
   (where String_2 ,(string-append "bad argument #2 (number expected, got "
                                   (term String_1)
                                   ")"))]
  
  ; Default case
  [(δ (table.unpack v_1 v_2 v_3 θ))
   (δ (error String_2))
   
   (where String_1 (δ (type v_3 θ)))
   
   (where String_2 ,(string-append "bad argument #3 (number expected, got "
                                   (term String_1)
                                   ")"))]
  
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
  
  
  ;                                                          
  ;                                     ;                    
  ;                                                          
  ;                                                          
  ;                                                          
  ;    ;;;;    ;;;;    ;;;;;  ;    ;  ;;;      ;;;;    ;;;;  
  ;    ;;  ;  ;;  ;;  ;;  ;;  ;    ;    ;      ;;  ;  ;;  ;; 
  ;    ;      ;    ;  ;    ;  ;    ;    ;      ;      ;    ; 
  ;    ;      ;;;;;;  ;    ;  ;    ;    ;      ;      ;;;;;; 
  ;    ;      ;       ;    ;  ;    ;    ;      ;      ;      
  ;    ;      ;;   ;  ;;  ;;  ;   ;;    ;      ;      ;;   ; 
  ;    ;       ;;;;    ;;; ;   ;;; ;  ;;;;;    ;       ;;;;  
  ;                        ;                                 
  ;                        ;                                 
  ;                        ;
  ; Basic implementation of the require service
  [(δ (package.require String_1 θ))
   (δ (load String_2 nil nil nil θ))

   (where String_2 ,(file->string (term String_1)))]
  
  )


; To convert booleans values in racket to boolean values in our language 
(define-metafunction core-lang
  
  [(toBool #t)
   true]
  
  [(toBool any)
   false])

; To export the delta function
(provide δ)


;                                                                                          
;                                                           ;       ;;;                    
;                                                           ;         ;                    
;                     ;                       ;             ;         ;                    
;                     ;                       ;             ;         ;                    
;   ;;;;;;;  ;;;;   ;;;;;;    ;;;           ;;;;;;    ;;;   ;;;;;     ;      ;;;;    ;;;;  
;   ;  ;  ; ;;  ;;    ;      ;   ;            ;      ;   ;  ;;  ;;    ;     ;;  ;;  ;    ; 
;   ;  ;  ; ;    ;    ;          ;            ;          ;  ;    ;    ;     ;    ;  ;      
;   ;  ;  ; ;;;;;;    ;      ;;;;;   ;;;      ;      ;;;;;  ;    ;    ;     ;;;;;;   ;;;;  
;   ;  ;  ; ;         ;     ;    ;            ;     ;    ;  ;    ;    ;     ;            ; 
;   ;  ;  ; ;;   ;    ;     ;   ;;            ;     ;   ;;  ;;  ;;    ;     ;;   ;  ;    ; 
;   ;  ;  ;  ;;;;      ;;;   ;;; ;             ;;;   ;;; ;  ;;;;;      ;;;   ;;;;    ;;;;  
;                                                                                          
;                                                                                          
;                                                                                          
(define (eventHasHandler handler)
  (and (not (equal? handler (term nil)))
       (not (equal? handler (term false)))))

; Chooses a handler for a binary operation
; PRE : {v_1, v_2 are the operands and String is the string that serves as
;       key to index the meta-table
; ret = (getBinHandler v_1 v_2 String θ)
; POS : {returns the value of v_1's meta-table indexed with key String (if
;        it belongs to the meta-table and the value is not nil or false) or
;        it returns the value of v_2's meta-table indexed with key String}
(define-metafunction core-lang
  getBinHandler : v v String θ -> v
  
  [(getBinHandler v_1 v_2 String θ)
   any
   
   ; Determine if v_1 has meta-table
   (where any (indexMetaTable v_1 String θ))
   (side-condition (eventHasHandler (term any)))]
  
  [(getBinHandler v_1 v_2 String θ)
   any_2
   
   ; Determine if v_1 has meta-table
   (where any (indexMetaTable v_1 String θ))
   
   (side-condition (not (eventHasHandler (term any))))
   
   (where any_2 (indexMetaTable v_2 String θ))
   
   (side-condition (eventHasHandler (term any_2)))]
  
  ; Otherwise...
  [(getBinHandler v_1 v_2 String θ)
   nil])

(provide getBinHandler)

; Chooses a handler for an unary operation
; PRE : {sv is the operand and String is the string that serves as
;       key to index the meta-table
; ret = (getUnaryHandler sv String θ)
; POS : {returns the value of sv's meta-table indexed with key String (if
;        it belongs to the meta-table and the value is not nil or false) or
;        nil}
(define-metafunction core-lang
  getUnaryHandler : v String θ -> v
  
  [(getUnaryHandler v String θ)
   any
   ; Determine if sv has meta-table
   (where any (indexMetaTable v String θ))
   (side-condition (eventHasHandler (term any)))]
  
  ; Otherwise...
  [(getUnaryHandler v String θ)
   nil])

(provide getUnaryHandler)

; Returns the predefined location where a meta-table for an indicated type,
; different from type "table", must be stored
(define-metafunction core-lang
  [(getMetaTableRef Number)
   (objr 1)]
  
  [(getMetaTableRef nil)
   (objr 2)]
  
  [(getMetaTableRef Boolean)
   (objr 3)]
  
  [(getMetaTableRef String)
   (objr 4)]
  
  [(getMetaTableRef objref)
   (objr 5)])

(provide getMetaTableRef)


; Meta-function that tries to get the meta-table of a given value and index it
; with a given key. If it doesn't succeed, it returns nil.
; PRE : {sv_1 is the value whose meta-table we want to index and sv_2 is the
;        key}
; ret = (indexMetaTable sv_1 sv_2 θ)

(define-metafunction core-lang
  ; objref_1 points to a table, which has a metatable with key v_1
  [(indexMetaTable objref_1 v_1 θ)
   v_2
   
   (side-condition (equal? (term (δ (type objref_1 θ)))
                           "table"))
   
   (where (tableconstructor objref_2) (derefTheta θ objref_1))
   
   (where v_2 (δ (rawget objref_2 v_1 θ)))]
  
  ; v_1 isn't a table and doesn't have a metatable.
  [(indexMetaTable v_1 v_2 θ)
   nil
   
   (where objref (getMetaTableRef v_1))
   
   (side-condition (not (term (refBelongsToTheta? objref θ))))]
  
  ; v_1 isn't a table and has a metatable.
  [(indexMetaTable v_1 v_2 θ)
   v_3
   
   (where objref (getMetaTableRef v_1))
   
   (where v_3 (δ (rawget objref v_2 θ)))])

(provide indexMetaTable)


(define-metafunction core-lang
  ; Protected meta-table
  [(protectedMetaTable? objref θ)
   #t
   
   (side-condition (not (equal? (term (indexMetaTable objref "__metatable" θ))
                                (term nil))))]
  
  ; Default case
  [(protectedMetaTable? objref θ)
   #f])

(provide protectedMetaTable?)


; Obtain a handler for an equality comparison, following the criterion defined
; in the procedure of the same name, in Lua's reference manual
(define-metafunction core-lang 
  
  ; The values compared are tables, with the same handler for the equality
  ; comparison
  [(getEqualHandler v_1 v_2 θ)
   any_1
   
   (side-condition (equal? (term (δ (type v_1 θ)))
                           (term (δ (type v_2 θ)))))
   
   (side-condition (equal? (term (δ (type v_1 θ)))
                           "table"))
   
   (where any_1 (indexMetaTable v_1 "__eq" θ))
   
   (where any_2 (indexMetaTable v_2 "__eq" θ))
   
   (side-condition (equal? (term any_1)
                           (term any_2)))]
  
  ; The values compared are tables, with the different handlers for the equality
  ; comparison
  [(getEqualHandler v_1 v_2 θ)
   nil
   
   (side-condition (equal? (term (δ (type v_1 θ)))
                           (term (δ (type v_2 θ)))))
   
   (side-condition (equal? (term (δ (type v_1 θ)))
                           "table"))]
  
  ; The types of the values compared are different, or they are not tables
  [(getEqualHandler v_1 v_2 θ)
   nil])

(provide getEqualHandler)

(define-metafunction core-lang
  
  [(errmessage ArithWrongOps String_1 String_2)
   ,(string-append "attempt to perform arithmetic on a "
                   (term String_1)
                   " value.")
   
   (side-condition (not (equal? (term String_1)
                                "number")))]
  
  [(errmessage ArithWrongOps "number" String_2)
   ,(string-append "attempt to perform arithmetic on a "
                   (term String_2)
                   " value.")]
  
  [(errmessage StrConcatWrongOps "string" String_2)
   ,(string-append "attempt to concatenate a "
                   (term String_2)
                   " value.")]
  
  [(errmessage StrConcatWrongOps String_1 String_2)
   ,(string-append "attempt to concatenate a "
                   (term String_1)
                   " value.")]
  
  [(errmessage OrdCompWrongOps String_1 String_2)
   ,(string-append "attempt to compare "
                   (term String_1)
                   " with "
                   (term String_2))]
  
  [(errmessage NegWrongOp String)
   ,(string-append "attempt to perform arithmetic on a "
                   (term String)
                   " value.")]
  
  [(errmessage StrLenWrongOp String)
   ,(string-append "attempt to get length of a "
                   (term String)
                   " value.")]
  
  [(errmessage WrongFunCall String)
   ,(string-append "attempt to call a "
                   (term String)
                   " value.")]
  
  [(errmessage specCondLabel String)
   ,(string-append "attempt to index a "
                   (term String)
                   " value.")
   
   (side-condition (or (equal? (term specCondLabel)
                               (term NonTableIndexed))
                       
                       (equal? (term specCondLabel)
                               (term FieldAssignOverNonTable))))]
  )

(provide errmessage)

(define-metafunction core-lang
  
  [(binopeventkey +)
   "__add"]
  
  [(binopeventkey -)
   "__sub"]
  
  [(binopeventkey *)
   "__mul"]
  
  [(binopeventkey /)
   "__div"]
  
  [(binopeventkey %)
   "__mod"]
  
  [(binopeventkey ^)
   "__pow"]
  
  [(binopeventkey ..)
   "__concat"]
  
  [(binopeventkey <)
   "__lt"]
  
  [(binopeventkey <=)
   "__le"])

(provide binopeventkey)

(define-metafunction core-lang
  
  [(unopeventkey \#)
   "__len"]
  
  [(unopeventkey -)
   "__unm"])

(provide unopeventkey)

(define-metafunction core-lang
  
  [(misceventkey WrongFunCall)
   "__call"]
  
  [(misceventkey specCondLabel)
   "__index"
   
   (side-condition (or (equal? (term specCondLabel)
                               (term KeyNotFound))
                       
                       (equal? (term specCondLabel)
                               (term NonTableIndexed))))]
  
  [(misceventkey specCondLabel)
   "__newindex"
   
   (side-condition (or (equal? (term specCondLabel)
                               (term FieldAssignWrongKey))
                       
                       (equal? (term specCondLabel)
                               (term FieldAssignOverNonTable))))])

(provide misceventkey)

; To ease the comparation of the keys of a table, reusing δ (== ...)  
(define-metafunction core-lang
  extractField : (field ...) v -> any
  
  [(extractField (field ...) v)
   ,(filter (λ (field)
              ; Take each field that equals to
              ; (\[ v \] = any), according to delta
              ((λ (match)
                 (if match
                     
                     ; There must be just one match structure
                     ((λ (bindings)
                        ; Compare the keys
                        (if (equal? (term (δ (== ,(bind-exp (list-ref bindings 0)) v)))
                                    (term true))
                            #t
                            #f))
                      ; Pass the only match structure obtained
                      (match-bindings (list-ref match 0)))
                     
                     #f))
               ; Extract each component of the field
               (redex-match core-lang
                            (\[ any_1 \] = any_2)
                            field)))
            (term (field ...)))])

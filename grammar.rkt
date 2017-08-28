#lang racket

(require redex)
; Core language grammar definition
(define-language core-lang                                                          
  [s \;
     break
     (return e)
     functioncall
     ($builtIn Name (e ...))
     ((var ...) = (e ...))
     (do s end)
     (if e then s else s end)
     (while e do s end)
     (local (Name ...) = (e ...) in s end)
     (s_1 s_2 ...)

     ; Run-time statements
     (< e ... >)
     ((((v \[ v \])) = (v))specCondLabel)
     ($err v)
     ((s)ProtectedMode)
     ((s)ProtectedMode v) ; xpcall's protected mode
     ((s)Return)
     ((s)Break)
     ((v (v ...))WrongFunCall)
     ($nextItWhile e do s end)]

  [specCondLabel FieldAssignWrongKey
                 FieldAssignOverNonTable
                 KeyNotFound
                 NonTableIndexed
                 ArithWrongOps
                 StrConcatWrongOps
                 EqFail
                 OrdCompWrongOps
                 NegWrongOp
                 StrLenWrongOp]

  [Boolean true false]
  
  [v nil Boolean Number String objref]
  
  [vlist (v ...)]

  ; Variables' Identifiers' syntactic category, to ease the definition of the
  ; substitution function.
  [id Name
      <<<]

  [parameters (Name ...)
              (Name ... <<<)]

  ; This syntactic category is added to ease meta-functions' definitions. 
  [functiondef (function Name parameters s end)]
  
  [e v
     <<<
     var
     ; To allow function calls in protected mode, in place of expressions.
     functioncall
     ($builtIn Name (e ...))
     (\( e \))
     tableconstructor
     functiondef
     (e binop e)
     (unop e)

     ; Run-time expressions
     r
     (< e ... >)
     ($err v)
     ((s)FunCall)
     ; To allow expressions like ((((e)FunctionCall))FunctionCall)
     ((e)FunCall)
     ((e)ProtectedMode)
     ((v \[ v \])specCondLabel)
     ((v binop v)specCondLabel)
     ((unop v)specCondLabel)
     ((v (v ...))WrongFunCall)]
  
  [functioncall (e (e ...))
                (e : Name (e ...))]
  
  [tableconstructor (\{ field ... \})]
  
  [evaluatedtable (\{ efield ... \})]
  
  [arithop + - * / ^ %]
  
  [relop < <= > >=]

  ; Not short-circuit binop
  [strictbinop arithop relop == ..]
  
  [binop strictbinop and or]
  
  [unop - not \#]
  
  ; Name can be anything except a keyword of the language
  [Name variable-not-otherwise-mentioned]
  
  ;[prefixexp var functioncall (\( exp \))]

  [var Name 
       (e \[ e \])
       ; run-time expression
       evar]

  [evar r
        (v \[ v \])]
  
  [field (\[ e \] = e)
         ; We need to allow fields like this
         e]
  
  [efield (\[ v \] = v)
          v]
  
  ; Number represents real (double-precision floating-point) numbers
  [Number real]
  
  [String string]


  
  ;                                                  
  ;                                                  
  ;                                                  
  ;    ;;;;;    ;                                    
  ;   ;;    ;   ;                                    
  ;   ;       ;;;;;;   ;;;;    ;;;;    ;;;;    ;;;;  
  ;   ;;        ;     ;;  ;;   ;;  ;  ;;  ;;  ;    ; 
  ;    ;;;;;    ;     ;    ;   ;      ;    ;  ;      
  ;        ;;   ;     ;    ;   ;      ;;;;;;   ;;;;  
  ;         ;   ;     ;    ;   ;      ;            ; 
  ;   ;    ;;   ;     ;;  ;;   ;      ;;   ;  ;    ; 
  ;    ;;;;;     ;;;   ;;;;    ;       ;;;;    ;;;;  
  ;                                                  
  ;                                                  
  ;                                                  

  
  ; This syntactic category is added to ease meta-functions' definitions.
  [intreptable (tableconstructor objref)
               (tableconstructor nil)]
  
  ; Values that we can store in an object store
  [object functiondef 
          intreptable]

  [r (ref number)]
  
  [objref (objr number)]
  
  [rst (r v)]
  
  [σ ((r v) ...)]

  [objrefst (objref object)]
  
  [θ ((objref object) ...)]
  
  
  
  ;                                                                          
  ;                                                                          
  ;                                                                          
  ;   ;;;;;;;                           ;;;;                    ;            
  ;   ;                                ;    ;                   ;            
  ;   ;       ;    ;                  ;        ;;;;   ; ;;;   ;;;;;;  ;;  ;; 
  ;   ;       ;;  ;;                  ;       ;;  ;;  ;;   ;    ;      ;  ;  
  ;   ;;;;;;;  ;  ;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;        ;  ;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;        ;;;;                   ;       ;    ;  ;    ;    ;       ;;   
  ;   ;         ;;      ;;             ;    ; ;;  ;;  ;    ;    ;      ;  ;  
  ;   ;;;;;;;   ;;      ;;              ;;;;   ;;;;   ;    ;     ;;;  ;;  ;; 
  ;                                                                          
  ;                                                                          
  ;                                                                          

  ; No labelled-blocks, no protected mode
  [Elenlnp (v ... Enlnp e ...)]

  [Enlnp hole
         ; Statements
         (do Enlnp end)
         (if Enlnp then s else s end)
         (local (Name ...) = Elenlnp in s end)
         ((evar ... (Enlnp \[ e \]) var ...) = (e ...))
         ((evar ... (v \[ Enlnp \]) var ...) = (e ...))
         ((evar ...) = Elenlnp)
         (break Name Enlnp)
         (return Enlnp)
         (Enlnp s ...)

         ; Function call, method call, built-in services
         (Enlnp (e ...))
         (v Elenlnp)
         ($builtIn Name Elenlnp)
         (Enlnp : Name (e ...))

         ; Expressions
         ((Enlnp)FunCall)
         (\( Enlnp \))
         (Enlnp binop e)
         (v strictbinop Enlnp)
         (unop Enlnp)
         (< v ... Enlnp e ... >)
         (\{ efield ... (\[ Enlnp \] = e) field ... \})
         (\{ efield ... (\[ v \] = Enlnp) field ... \})
         (\{ efield ... Enlnp field ... \})
         (Enlnp \[ e \])
         (v \[ Enlnp \])]

  ; No labelled-blocks
  ; Simple induction on the structure helps to prove that an Elf is the
  ; desired evaluation context (for the inductive case: think that any evaluation
  ; context of the desired category, that has one or more ((...)ProtectedMode) phrases,
  ; begins being a Enlnp (even it could be a hole), then the first ((...)ProtectedMode)
  ; appears, and what's left is, by i.h., an Elf context. That's what the second production
  ; describes.
  [Elf Enlnp
       (in-hole Enlnp ((Elf)ProtectedMode))]

  ; No Protected Mode
  [Enp Enlnp
       (in-hole Enlnp ((Enp)Break))
       (in-hole Enlnp ((Enp)Return))]

  ; All possible evaluation contexts
  [E Enlnp
     (in-hole Enlnp ((E)Return))
     (in-hole Enlnp ((Enp)Break))
     (in-hole Enlnp ((E)ProtectedMode))]
  
   
  ; List of expressions where a tuple is truncated
  [Etel (v ... hole e_1 e_2 ...)]
  
  ; Immediate evaluation contexts where a tuple is truncated
  ; Propertie: it must occur that there is no sub-phrase of this evaluation
  ; contexts that is also an evaluation context: for example,
  ; (break Name (< Etel >)) is not a valid member of Et, as
  ; (< Etel >) is a member of Et.
  [Et (if hole then s else s_2 end)
      (local (Name ...) = Etel in s end)
      ((evar ... (hole \[ e \]) var ...) = (e ...))
      ((evar ... (v \[ hole \]) var ...) = (e ...))
      ((evar ...) = Etel)
      (hole (e ...))
      (v Etel)
      ($builtIn Name Etel)
      (hole : Name (e ...))
      (hole binop e)
      (v strictbinop hole)
      (unop hole)
      (< v ... hole e_1 e_2 ... >)
      (\{ efield ... (\[ hole \] = e) field ... \})
      (\{ efield ... (\[ v \] = hole) field ... \})
      (\{ efield ... hole field_1 field ... \})
      (hole \[ e \])
      (v \[ hole \])]
  
  ; List of expressions where a tuple is unwrapped
  [Euel (v ... hole)]
  ; Immediate evaluation contexts where a tuple is unwrapped
  [Eu (local (Name ...) = Euel in s end)
      ((evar ...) = Euel)
      (v Euel)
      ($builtIn Name Euel)
      (< v ... hole >)
      (\{ efield ... hole \})]

  ; Evaluation contexts where tuples are discarded
  [Ed hole 
      (do Ed end)
      (Ed s_1 s_2 ...)
      ((Ed)Break)]
  ) 

; Export core-lang grammar definition
(provide core-lang)

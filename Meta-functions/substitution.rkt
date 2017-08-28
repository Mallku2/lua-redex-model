#lang racket
(require redex
         "../grammar.rkt")

; Substitution function over expressions
; PARAMS:
; exp : the expression to which the substitution is applied
; parameters : a list of identifiers to be subtituted
; explist : a list of expressions to substitute the identifiers in
; parameters. The identifier in the position i of parameters will
; be replaced by the expression in the position i of expressions
(define-metafunction core-lang
  substExp : e ((id e) ... ) -> e

  ; Empty mapping
  [(substExp e ())
   e]
  
  ; Variable identifier or vararg expression
  [(substExp id_1 ((id_2 e) ...))
   (applySubst id_1 ((id_2 e) ...))]
  
  ; Function call
  [(substExp (e_1 (e_2 ...)) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...))
    (substexplist (e_2 ...) ((id e_3) ...)))]
  
  [(substExp (e_1 : Name (e_2 ...)) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...))
    : Name (substexplist (e_2 ...) ((id e_3) ...)))]
  
  ; Built-in procedure call
  [(substExp ($builtIn Name (e_1 ...)) ((id e_2) ...))
   ($builtIn Name (substexplist (e_1 ...) ((id e_2) ...)))]
  
  ; Operator '( ')'
  [(substExp (\( e_1 \)) ((id e_2) ...))
   (\( (substExp e_1 ((id e_2) ...)) \))]
  
  ; Table indexing
  [(substExp (e_1 \[ e_2 \]) ((id e_3) ...))
   ((substExp e_1 ((id e_3) ...)) \[ (substExp e_2 ((id e_3) ...)) \])]
  
  ; Tuple
  ; Non empty tuple
  ; expresiones?
  [(substExp (< e_1 ... >) ((id e_2) ...))
   ,(append (term (< ))
            (term (substexplist (e_1 ...) ((id e_2) ...)))
            (term ( >)))]
  
  ; Function definition
  ; We are assuming that the identifiers parlist occur in the 
  ; same order as in namelist.
  ; When the substitution defines a substitute to a vararg expression, it is
  ; discarded
  [(substExp (function Name_1 (id ...) s end) ((Name e_1) ... (<<< e_2)))
   (substExp (function Name_1 (id ...) s end) ((Name e_1) ...))]
  
  [(substExp (function Name_1 (id ...) s end) ((Name_2 e_1) ...))
   (function Name_1 (id ...) 
                 (substBlock s ((Name_3 e_3) ...))
                 end)
   (where ((Name_3 e_3) ...) ,(remove* (term (id ...)) (term ((Name_2 e_1) ...))
                                 (λ (identifier pair)
                                   (equal? identifier
                                           (list-ref pair 0)))))]
  
  
  ; Table constructor
  ; substfield receives and returns a list of the form (field ...)
  ; so in this case, to reconstruct the original expression that has the form
  ; of a list of symbols, we must escape to scheme code an use the append
  ; function put the result of substfield with the others symbols in one list
  [(substExp (\{ field ... \}) ((id e) ...))
   ,(append (term (\{))
            (append (term (substfield (field ...) ((id e) ...)))
                    (term (\}))))]
  
  ; Binary operators
  [(substExp (e_1 binop e_2) ((id e) ...))
   ((substExp e_1 ((id e) ...))
    binop
    (substExp e_2 ((id e) ...)))]
  
  ; Unary operators
  [(substExp (unop e_1) ((id e_2) ...))
   (unop (substExp e_1 ((id e_2) ...)))]

  
  ; These case holds for every expression without an structure, different than
  ; a variable or a vararg exp: nil, empty, boolean, number, string, 
  ; simpvalref, objref
  [(substExp any ((id e_2) ...))
   any])

; Substitution function for blocks
(define-metafunction core-lang
  substBlock : s ((id e) ...) -> s

  ; Empty mapping
  [(substBlock s ())
   s]
  
  ; Empty statement
  [(substBlock \; ((id e) ...))
   \;]

  ; Function call
  [(substBlock functioncall ((id e) ...))
   (substExp functioncall ((id e) ...))]

  ; Built-in procedure call
  [(substBlock ($builtIn Name (e_1 ...)) ((id e_2) ...))
   (substExp ($builtIn Name (e_1 ...)) ((id e_2) ...))]

  ; Concatenation of statements
  [(substBlock (s_1 s_2 ...) ((id e) ...))
   (substslist (s_1 s_2 ...) ((id e) ...))]

  ; Block Do...End
  [(substBlock (do s end) ((id e) ...))
   (do (substBlock s ((id e) ...)) end)]

  ; Break statement
  [(substBlock break ((id e_2) ...))
   break]

  ; Return statement
  [(substBlock (return e_1) ((id e_2) ...))
   (return (substExp e_1 ((id e_2) ...)))]

  ; Conditional
  [(substBlock (if e_1 then s_1 else s_2 end) ((id e_2) ...))
   (if (substExp e_1 ((id e_2) ...)) then
       (substBlock s_1 ((id e_2) ...))
       else (substBlock s_2 ((id e_2) ...)) end)]
  
  ; While loop
  [(substBlock (while e_1 do s end) ((id e_2) ...))
   (while (substExp e_1 ((id e_2) ...)) do 
          (substBlock s ((id e_2) ...)) 
          end)]
  
  ; Local statement
  [(substBlock (local (Name_1 ...) = (e_1 ...) in s end) ((id_1 e_2) ...))
   (local (Name_1 ...) = (substexplist (e_1 ...) ((id_1 e_2) ...)) in 
     (substBlock s ((id_2 e_3) ...))
     end)
   
   (where ((id_2 e_3) ...) ,(remove* (term (Name_1 ...)) (term ((id_1 e_2) ...))
                                     (λ (identifier pair)
                                   (equal? identifier
                                           (list-ref pair 0)))))]
   
  ; Variable assignment
  [(substBlock ((var ...) = (e_1 ...)) ((id e_2) ...))
   ((substexplist (var ...) ((id e_2) ...))
    =
    (substexplist (e_1 ...) ((id e_2) ...)))])


;                                                                  
;                                                                  
;                                                                  
;                              ;     ;;;       ;                   
;     ;;                               ;                           
;     ;;                               ;                           
;     ;;    ;    ;  ;;   ;   ;;;       ;     ;;;     ;;;;    ; ;;; 
;    ;  ;   ;    ;   ;  ;      ;       ;       ;         ;   ;;    
;    ;  ;   ;    ;    ;;       ;       ;       ;         ;   ;     
;   ;;;;;;  ;    ;    ;;       ;       ;       ;     ;;;;;   ;     
;   ;    ;  ;    ;    ;;       ;       ;       ;    ;    ;   ;     
;   ;    ;  ;   ;;   ;  ;      ;       ;       ;    ;   ;;   ;     
;  ;      ;  ;;; ;  ;    ;   ;;;;;   ;;;;;   ;;;;;   ;;; ;   ;     
;                                                                  
;                                                                  
;                                                                  
;                                                                  

(define-metafunction core-lang
  applySubst : id ((id e) ...) -> e
  
  [(applySubst id ())
   id]
  
  [(applySubst id_1 ((id_1 e_1) (id e) ...))
   e_1]
  
  [(applySubst id_1 ((id_2 e_1) (id e) ...))
   (applySubst id_1 ((id e) ...))

   (side-condition (not (equal? (term id_1)
                                (term id_2))))]
  )

; PRE : {lenght(id ...) == length(e ...)}
(define-metafunction core-lang
  createSubstMap : (id ...) (e ...) -> ((id e) ...)
  
  [(createSubstMap () ())
   ()]
  
  [(createSubstMap (id_1 id ...) (e_1 e ...))
   ,(append (term ((id_1 e_1)))
            (term (createSubstMap (id ...) (e ...))))]
  )

(provide createSubstMap)

; Auxiliar meta-function to perform a substitution over list
; of exp constructions.
(define-metafunction core-lang
  substexplist : (e ...) ((id e) ...) -> (e ...)
  
  [(substexplist () ((id e) ...))
   ()]
  
  [(substexplist (e_1) ((id e) ...))
   ((substExp e_1 ((id e) ...)))]
  
  [(substexplist (e_1 e ...) ((id e_2) ...))
   ,(append (term ((substExp e_1 ((id e_2) ...)))) 
            (term (substexplist (e ...) ((id e_2) ...))))])

; Auxiliar meta-function to perform a substitution over list
; of exp constructions.
(define-metafunction core-lang
  substslist : (s ...) ((id e) ...) -> (s ...)
  
  [(substslist () ((id e) ...))
   ()]
  
  [(substslist (s_1) ((id e) ...))
   ((substBlock s_1 ((id e) ...)))]
  
  [(substslist (s_1 s ...) ((id e_2) ...))
   ,(append (term ((substBlock s_1 ((id e_2) ...)))) 
            (term (substslist (s ...) ((id e_2) ...))))])

; Auxiliar meta-function to perform a substitution over list
; of table fields.
(define-metafunction core-lang
  substfield : (field ...) ((id e) ...) -> (field ...)
  
  [(substfield () ((id e_2) ...))
   ()]
  
  [(substfield ((\[ e_1 \] = e_2)) ((id e_3) ...))
   ((\[ (substExp e_1 ((id e_3) ...)) \]
        =
        (substExp e_2 ((id e_3) ...))))]
  
  [(substfield (e_1) ((id e_2) ...))
   ((substExp e_1 ((id e_2) ...)))]
  
  [(substfield (field_1 field ...) ((id e) ...))
   ,(append (term (substfield (field_1) ((id e) ...))) 
            (term (substfield (field ...) ((id e) ...))))])

; Export subst meta-function
(provide substBlock
         substExp)

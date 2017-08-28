#lang racket
(require redex
         "../grammar.rkt")

(provide (all-defined-out))


;                                                                          
;           ;                                                              
;           ;                                                              
;      ;    ;                 ;                               ;            
;     ; ;   ;                 ;                               ;            
;     ; ;   ;;;;;    ;;;;   ;;;;;;   ;;;;     ;;;     ;;;   ;;;;;;         
;     ; ;   ;;  ;;  ;    ;    ;      ;;  ;   ;   ;   ;   ;    ;            
;    ;   ;  ;    ;  ;         ;      ;           ;  ;         ;            
;    ;   ;  ;    ;   ;;;;     ;      ;       ;;;;;  ;         ;            
;    ;;;;;  ;    ;       ;    ;      ;      ;    ;  ;         ;            
;   ;;   ;; ;;  ;;  ;    ;    ;      ;      ;   ;;   ;   ;    ;            
;   ;     ; ;;;;;    ;;;;      ;;;   ;       ;;; ;    ;;;      ;;;         
;                                                                          
;                                                                          
;                                                                          

;                                                                          
;                                                                          
;                                                                          
;                                                             ;            
;                                                             ;            
;    ;;;;;   ;;;;     ;;;   ;;;;;;; ;;;;;;;   ;;;    ;;;;     ;      ;;;;  
;   ;;  ;;   ;;  ;   ;   ;  ;  ;  ; ;  ;  ;  ;   ;   ;;  ;    ;     ;    ; 
;   ;    ;   ;           ;  ;  ;  ; ;  ;  ;      ;   ;              ;      
;   ;    ;   ;       ;;;;;  ;  ;  ; ;  ;  ;  ;;;;;   ;               ;;;;  
;   ;    ;   ;      ;    ;  ;  ;  ; ;  ;  ; ;    ;   ;                   ; 
;   ;;  ;;   ;      ;   ;;  ;  ;  ; ;  ;  ; ;   ;;   ;              ;    ; 
;    ;;; ;   ;       ;;; ;  ;  ;  ; ;  ;  ;  ;;; ;   ;               ;;;;  
;        ;                                                                 
;    ;   ;                                                                 
;     ;;;                                                                  

;                                                                                                  
;                                                                                                  
;                                                                                                  
;                                     ;                               ;                            
;                                     ;                               ;                            
;     ;;;    ;;;;   ; ;;;    ;;;;   ;;;;;;   ;;;;   ;    ;    ;;;   ;;;;;;   ;;;;    ;;;;    ;;;;  
;    ;   ;  ;;  ;;  ;;   ;  ;    ;    ;      ;;  ;  ;    ;   ;   ;    ;     ;;  ;;   ;;  ;  ;    ; 
;   ;       ;    ;  ;    ;  ;         ;      ;      ;    ;  ;         ;     ;    ;   ;      ;      
;   ;       ;    ;  ;    ;   ;;;;     ;      ;      ;    ;  ;         ;     ;    ;   ;       ;;;;  
;   ;       ;    ;  ;    ;       ;    ;      ;      ;    ;  ;         ;     ;    ;   ;           ; 
;    ;   ;  ;;  ;;  ;    ;  ;    ;    ;      ;      ;   ;;   ;   ;    ;     ;;  ;;   ;      ;    ; 
;     ;;;    ;;;;   ;    ;   ;;;;      ;;;   ;       ;;; ;    ;;;      ;;;   ;;;;    ;       ;;;;  
;                                                                                                  
;                                                                                                  
;                                                                                                  



;                                                                                  
;                                                                                  
;                                                                                  
;    ;;;;;    ;               ;                                       ;            
;   ;;    ;   ;               ;                                       ;            
;   ;       ;;;;;;    ;;;   ;;;;;;   ;;;;   ;;;;;;;  ;;;;   ; ;;;   ;;;;;;   ;;;;  
;   ;;        ;      ;   ;    ;     ;;  ;;  ;  ;  ; ;;  ;;  ;;   ;    ;     ;    ; 
;    ;;;;;    ;          ;    ;     ;    ;  ;  ;  ; ;    ;  ;    ;    ;     ;      
;        ;;   ;      ;;;;;    ;     ;;;;;;  ;  ;  ; ;;;;;;  ;    ;    ;      ;;;;  
;         ;   ;     ;    ;    ;     ;       ;  ;  ; ;       ;    ;    ;          ; 
;   ;    ;;   ;     ;   ;;    ;     ;;   ;  ;  ;  ; ;;   ;  ;    ;    ;     ;    ; 
;    ;;;;;     ;;;   ;;; ;     ;;;   ;;;;   ;  ;  ;  ;;;;   ;    ;     ;;;   ;;;;  
;                                                                                  
;                                                                                  
;                                                                                  

(struct skip ())

(struct break ())

(struct return (e))

(struct fun-call (prefixexp args))

(struct method-call (prefixexp method-name args))

(struct var-assign (lvalues rvalues))

(struct conditional (guard if else))

(struct while (guard body))

(struct local-vars (ids exps scope))

(struct conc-stats (stats))

(struct do-end (stats))


; run-time constructions (used in compile time)
(struct built-in-call (service args))

(struct tuple (exps))


;                                                                                          
;                                                             ;                            
;                                                                                          
;   ;;;;;;;                                                                                
;   ;                                                                                      
;   ;       ;;  ;;  ;;;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;      ;;;;   ; ;;;    ;;;;  
;   ;        ;  ;   ;;  ;;   ;;  ;  ;;  ;;  ;    ;  ;    ;    ;     ;;  ;;  ;;   ;  ;    ; 
;   ;;;;;;;   ;;    ;    ;   ;      ;    ;  ;       ;         ;     ;    ;  ;    ;  ;      
;   ;         ;;    ;    ;   ;      ;;;;;;   ;;;;    ;;;;     ;     ;    ;  ;    ;   ;;;;  
;   ;         ;;    ;    ;   ;      ;            ;       ;    ;     ;    ;  ;    ;       ; 
;   ;        ;  ;   ;;  ;;   ;      ;;   ;  ;    ;  ;    ;    ;     ;;  ;;  ;    ;  ;    ; 
;   ;;;;;;; ;;  ;;  ;;;;;    ;       ;;;;    ;;;;    ;;;;   ;;;;;    ;;;;   ;    ;   ;;;;  
;                   ;                                                                      
;                   ;                                                                      
;                   ;                                                                      


; values
(struct nil ())

(struct true ())

(struct false ())

(struct nmbr (value))

(struct str (value))

; id
(struct id-name (name))

(struct id-vararg ())

(struct var-table-field (t k))

; function definition
(struct params (ids))

(struct vararg-params (ids))

(struct func-def (label formal-params body))

; parenthesized expressions
(struct parent-e (e))

; table constructors
; fieldss
(struct kv-table-field (k v))

(struct v-table-field (v))

; list of table fields
(struct fields (flds))

(struct tableconstructor (fields))

; binops
(struct binop (op e1 e2))

; unops
(struct unop (op e))

; arithmetic
(struct add ())

(struct sub ())

(struct mul ())

(struct div ())

(struct pow ())

(struct mod ())

(struct unm ())

; strings
(struct str-concat ())

(struct len ())

; boolean
(struct \\and ())

(struct \\or ())

(struct \\not ())

; references
(struct val-ref (nmbr))

(struct objr-ref (nmbr))

; relational
(struct lt ())

(struct le ())

(struct gt ())

(struct ge ())

(struct eq ())

; list of expressions
(struct exps (el))


; Concrete-to-abstract grammar translation...
(define-metafunction core-lang
  [(to-abstract (ref Number))
   ,(val-ref (nmbr (term Number)))]

  [(to-abstract (objr Number))
   ,(objr-ref (nmbr (term Number)))]

  [(to-abstract Number)
   ,(nmbr (term Number))])

(provide to-abstract)
;                                                                  
;                                                                  
;                                                                  
;     ;;;;                                            ;            
;    ;    ;                                           ;            
;   ;        ;;;;   ; ;;;     ;;;    ;;;;    ;;;;   ;;;;;;   ;;;;  
;   ;       ;;  ;;  ;;   ;   ;   ;   ;;  ;  ;;  ;;    ;     ;;  ;; 
;   ;       ;    ;  ;    ;  ;        ;      ;    ;    ;     ;    ; 
;   ;       ;    ;  ;    ;  ;        ;      ;;;;;;    ;     ;;;;;; 
;   ;       ;    ;  ;    ;  ;        ;      ;         ;     ;      
;    ;    ; ;;  ;;  ;    ;   ;   ;   ;      ;;   ;    ;     ;;   ; 
;     ;;;;   ;;;;   ;    ;    ;;;    ;       ;;;;      ;;;   ;;;;  
;                                                                  
;                                                                  
;                                                                  


;                                                          
;                                                          
;                                                          
;                                                          
;                                                          
;    ;;;;;   ;;;;     ;;;   ;;;;;;; ;;;;;;;   ;;;    ;;;;  
;   ;;  ;;   ;;  ;   ;   ;  ;  ;  ; ;  ;  ;  ;   ;   ;;  ; 
;   ;    ;   ;           ;  ;  ;  ; ;  ;  ;      ;   ;     
;   ;    ;   ;       ;;;;;  ;  ;  ; ;  ;  ;  ;;;;;   ;     
;   ;    ;   ;      ;    ;  ;  ;  ; ;  ;  ; ;    ;   ;     
;   ;;  ;;   ;      ;   ;;  ;  ;  ; ;  ;  ; ;   ;;   ;     
;    ;;; ;   ;       ;;; ;  ;  ;  ; ;  ;  ;  ;;; ;   ;     
;        ;                                                 
;    ;   ;                                                 
;     ;;;                                                  

; concrete grammar for statements
(define (concrete-grammar-s phrase)
  (match phrase
    ((break)  (term break))
    
    ((skip)   (term \;))
    
    ((return e) (list (term return)
                      (concrete-grammar-e e)))

    ((do-end s) (list (term do)
                      (concrete-grammar-s s)
                      (term end)))

    ((while guard body) (list (term while)
                              (concrete-grammar-e guard)
                              (term do)
                              (concrete-grammar-s body)
                              (term end)))

    ((local-vars ids rvalues scope) (list (term local)
                                          (concrete-grammar-exps ids)
                                          (term =)
                                          (concrete-grammar-exps rvalues)
                                          (term in)
                                          (concrete-grammar-s scope)
                                          (term end)))

    
    ((conc-stats stats) (concrete-grammar-conc-stats (conc-stats stats)))
    
    ((var-assign lvalues rvalues) (list (concrete-grammar-exps lvalues)
                                        (term =)
                                        (concrete-grammar-exps rvalues)))
    
    ((conditional guard if else)  (list (term if) (concrete-grammar-e guard)
                                        (term then)
                                        (concrete-grammar-s if)
                                        (term else)
                                        (concrete-grammar-s else)
                                        (term end)))

    ; maybe it is a functioncall
    (_ (concrete-grammar-functioncall phrase))))

; concrete grammar for expressions
(define (concrete-grammar-e phrase)
  (match phrase
    ((nil) (term nil))

    ((true) (term true))

    ((false) (term false))

    ((nmbr value) (term ,value))

    ((str value) (term ,value))

    ((val-ref nmbr) (term (ref ,(concrete-grammar-e nmbr))))

    ((objr-ref nmbr) (term (objr ,(concrete-grammar-e nmbr))))
    
    ((id-name name) (term ,name))

    ((id-vararg) (term <<<))

    ((var-table-field t k) (list (concrete-grammar-e t)
                                 (term \[)
                                 (concrete-grammar-e k)
                                 (term \])))

    ((parent-e e) (list (term \()
                        (concrete-grammar-e e)
                        (term \))))

    ((binop op e1 e2) (append (list (concrete-grammar-e e1))
                              (list (concrete-grammar-op op))
                              (list (concrete-grammar-e e2))))

    ((unop op e) (append (list (concrete-grammar-op op))
                         (list (concrete-grammar-e e))))

    ; tuples
    ((tuple exps) (append (term (< ))
                          (concrete-grammar-exps exps)
                          (term ( >))))

    ; table constructor
    ((tableconstructor fields) (append (term (\{))
                                       (concrete-grammar-tablefields fields)
                                       (term (\}))))

    ; function definition
    ((func-def label formal-params body) (append (term (function ))
                                                 (list (concrete-grammar-e label))
                                                 (concrete-grammar-form-params formal-params)
                                                 (list (concrete-grammar-s body))
                                                 (term ( end))))

    ; maybe it is a functioncall
    (_ (concrete-grammar-functioncall phrase))))



;                                                                          
;                             ;     ;;;       ;                            
;                                     ;                                    
;      ;                              ;                                    
;     ; ;                             ;                                    
;     ; ;   ;    ;  ;;  ;;  ;;;       ;     ;;;       ;;;    ;;;;   ;    ; 
;     ; ;   ;    ;   ;  ;     ;       ;       ;      ;   ;   ;;  ;   ;   ; 
;    ;   ;  ;    ;    ;;      ;       ;       ;          ;   ;       ;  ;  
;    ;   ;  ;    ;    ;;      ;       ;       ;      ;;;;;   ;       ;  ;  
;    ;;;;;  ;    ;    ;;      ;       ;       ;     ;    ;   ;        ; ;  
;   ;;   ;; ;   ;;   ;  ;     ;       ;       ;     ;   ;;   ;        ;;   
;   ;     ;  ;;; ;  ;;  ;;  ;;;;;      ;;;  ;;;;;    ;;; ;   ;         ;   
;                                                                      ;   
;                                                                     ;    
;                                                                    ;;    
                                 ;;                                                                                    

(define (concrete-grammar-functioncall phrase)
  (match phrase
    ((fun-call prefixexp args) (list (concrete-grammar-e prefixexp)
                                     (concrete-grammar-exps args)
                                     ))

    ((method-call prefixexp method-name args) (list (concrete-grammar-e prefixexp)
                                                    (term :)
                                                    (concrete-grammar-e method-name)
                                                    (concrete-grammar-exps args)))

    ((built-in-call service args) (list (term \$builtIn)
                                        (concrete-grammar-e service)
                                        (concrete-grammar-exps args)))

    (_ (error "concrete-grammar-functioncall: received" phrase))))

(define (concrete-grammar-form-params form-params)
  (match form-params
    ((params ids) (list (concrete-grammar-exps ids)))

    ((vararg-params ids) (list (append (concrete-grammar-exps ids)
                                       (list (concrete-grammar-e (id-vararg))))))))

(define (concrete-grammar-op op)
  (match op
    ((add) (term +))

    ((sub) (term -))

    ((mul) (term *))

    ((div) (term /))

    ((pow) (term ^))

    ((mod) (term %))

    ((unm) (term -))

    ((str-concat) (term ..))

    ((len) (term \#))

    ((lt) (term <))

    ((le) (term <=))

    ((gt) (term >))

    ((ge) (term >=))

    ((eq) (term ==))

    ((\\and) (term and))

    ((\\or) (term or))

    ((\\not) (term not))

    (_     (error "concrete-grammar-op: received unkwon op " op))))


; list of expressions
(define (concrete-grammar-exps es)
  (match es
    ((exps '()) '())

    ((exps el) (append (list (concrete-grammar-e (list-ref el 0)))
                       (concrete-grammar-exps (exps (list-tail el 1)))))
    ))

; list of statementes
(define (concrete-grammar-conc-stats phrase)
  (match phrase
    ((conc-stats '()) '())

    ((conc-stats stats) (append (list (concrete-grammar-s (list-ref stats 0)))
                                (concrete-grammar-conc-stats (conc-stats (list-tail stats 1)))))
    ))

; table fields
; list of fields
(define (concrete-grammar-tablefields flds)
  (match flds
    ((fields '()) '())

    ((fields l) (append (list (concrete-grammar-tablefield (list-ref l 0)))
                        (concrete-grammar-tablefields (fields (list-tail l 1)))))))

; single field
(define (concrete-grammar-tablefield field)
  (match field
    ((kv-table-field k v) (append (term  (\[ ))
                                  (list (concrete-grammar-e k))
                                  (term  (\] = ))
                                  (list (concrete-grammar-e v))))

    ((v-table-field v) (concrete-grammar-e v))))

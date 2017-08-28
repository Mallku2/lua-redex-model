#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/metaTableMechanismMetafunctions.rkt")

; "black-box testing"


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
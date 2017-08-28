#lang racket
(require redex
         "../Meta-functions/objStoreMetafunctions.rkt")

; "black-box testing"


;                                                                                  
;                                                                                  
;                                                                                  
;                                      ;;;          ;                              
;   ;;;;                              ;    ;;;;;;;  ;                 ;            
;   ;   ;                             ;       ;     ;                 ;            
;   ;   ;;   ;;;;    ; ;;;   ;;;;   ;;;;;;    ;     ; ;;;    ;;;;   ;;;;;;   ;;;;  
;   ;    ;  ;;  ;;   ;;     ;;  ;;    ;       ;     ;;   ;  ;;  ;;    ;          ; 
;   ;    ;  ;    ;   ;      ;    ;    ;       ;     ;    ;  ;    ;    ;          ; 
;   ;    ;  ;;;;;;   ;      ;;;;;;    ;       ;     ;    ;  ;;;;;;    ;      ;;;;; 
;   ;   ;;  ;        ;      ;         ;       ;     ;    ;  ;         ;     ;    ; 
;   ;   ;   ;;       ;      ;;        ;       ;     ;    ;  ;;        ;     ;   ;; 
;   ;;;;     ;;;;;   ;       ;;;;;    ;       ;     ;    ;   ;;;;;     ;;;   ;;; ; 
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
(define (derefTheta-test-suite)
  (test-equal (term (derefTheta (((objr 1) ((\{ \}) nil))) (objr 1)))
              (term ((\{ \}) nil)))
  
  (test-equal (term (derefTheta (((objr 1) (function X () ((ref 1) ()) end)))
                                (objr 1)))
              (term (function X () ((ref 1) ()) end)))
  
  (test-results))



;                                                                                                                                          
;                                                                                                                                          
;                                                                                                                                          
;                      ;;;                   ;;;                                                            ;                              
;                     ;     ;;;;;              ;                                   ;;;;;;;         ;;;;;;;  ;                 ;            
;                     ;     ;    ;             ;                                      ;               ;     ;                 ;            
;    ; ;;;   ;;;;   ;;;;;;  ;    ;   ;;;;      ;     ;;;;   ; ;;;    ;;;;;;  ;;;;;    ;      ;;;;     ;     ; ;;;    ;;;;   ;;;;;;   ;;;;  
;    ;;     ;;  ;;    ;     ;   ;;  ;;  ;;     ;    ;;  ;;  ;;   ;  ;    ;  ;         ;     ;;  ;;    ;     ;;   ;  ;;  ;;    ;          ; 
;    ;      ;    ;    ;     ;;;;    ;    ;     ;    ;    ;  ;    ;  ;    ;  ;;        ;     ;    ;    ;     ;    ;  ;    ;    ;          ; 
;    ;      ;;;;;;    ;     ;   ;;  ;;;;;;     ;    ;    ;  ;    ;  ;    ;    ;;;     ;     ;    ;    ;     ;    ;  ;;;;;;    ;      ;;;;; 
;    ;      ;         ;     ;    ;  ;          ;    ;    ;  ;    ;   ;;;;        ;    ;     ;    ;    ;     ;    ;  ;         ;     ;    ; 
;    ;      ;;        ;     ;   ;;  ;;         ;    ;;  ;;  ;    ;  ;            ;    ;     ;;  ;;    ;     ;    ;  ;;        ;     ;   ;; 
;    ;       ;;;;;    ;     ;;;;;    ;;;;;   ;;;;;   ;;;;   ;    ;   ;;;;;  ;;;;;     ;      ;;;;     ;     ;    ;   ;;;;;     ;;;   ;;; ; 
;                                                                   ;     ;                                                                
;                                                                   ;     ;                                                                
;                                                                    ;;;;;                                                                 
;                                                                                                                                          

(define (refBelongsToTheta-test-suite)
  (test-equal (term (refBelongsToTheta? (objr 1) (((objr 1) ((\{ \}) nil)))))
              (term #t))
  
  (test-equal (term (refBelongsToTheta? (objr 2) (((objr 1) ((\{ \}) nil)))))
              (term #f))
  
  (test-results))


;;                                                                                          
;;                                                                                          
;;                                                                                          
;;      ;;;                          ;               ;           ;                      ;;; 
;;     ;                             ;        ;;;;   ;               ;;;;;             ;    
;;     ;                             ;        ;  ;   ;               ;   ;;            ;    
;;   ;;;;;;   ; ;;;   ;;;;    ;;;;;  ; ;;;   ;    ;  ; ;;;    ;;;;   ;    ;   ;;;;   ;;;;;; 
;;     ;      ;;     ;;  ;;  ;       ;;   ;  ;    ;  ;;  ;;      ;   ;   ;;  ;;  ;;    ;    
;;     ;      ;      ;    ;  ;;      ;    ;  ;    ;  ;    ;      ;   ;;;;    ;    ;    ;    
;;     ;      ;      ;;;;;;    ;;;   ;    ;  ;    ;  ;    ;      ;   ;  ;    ;;;;;;    ;    
;;     ;      ;      ;            ;  ;    ;  ;    ;  ;    ;      ;   ;   ;   ;         ;    
;;     ;      ;      ;;           ;  ;    ;   ;  ;   ;;  ;;      ;   ;    ;  ;;        ;    
;;     ;      ;       ;;;;;  ;;;;;   ;    ;   ;;;;   ; ;;;       ;   ;    ;;  ;;;;;    ;    
;;                                                               ;                          
;;                                                               ;                          
;;                                                           ;;;;                           
;;                                                                                          

(define (freshObjRef-test-suite)
  (test-equal (term (freshObjRef ()))
              (term (objr 6)))
              
  (test-equal (term (freshObjRef (((objr 6) ((\{ \}) nil)))))
              (term (objr 7)))
  
  (test-results))


;;                                                                                                                                          
;;                                                                                                                                          
;;                                                                                                                                          
;;      ;;;                                     ;                                                                                 ;         
;;     ;                               ;                             ;;;;;            ;;;;;    ;                                  ;  ;;;;;  
;;     ;                               ;                               ;             ;    ;    ;                                  ;  ;    ; 
;;   ;;;;;;  ;    ;  ; ;;;     ;;;;  ;;;;;;   ;;;     ;;;;   ; ;;;     ;      ;;;;;  ;       ;;;;;;   ;;;;    ; ;;;   ;;;;    ;;; ;       ; 
;;     ;     ;    ;  ;;   ;  ;;        ;        ;    ;;  ;;  ;;   ;    ;     ;       ;;        ;     ;;  ;;   ;;     ;;  ;;  ;;  ;;      ;  
;;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;    ;     ;;        ;;;     ;     ;    ;   ;      ;    ;  ;    ;     ;   
;;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;    ;       ;;;       ;;    ;     ;    ;   ;      ;;;;;;  ;    ;    ;    
;;     ;     ;    ;  ;    ;  ;         ;        ;    ;    ;  ;    ;    ;          ;       ;    ;     ;    ;   ;      ;       ;    ;         
;;     ;     ;   ;;  ;    ;  ;;        ;        ;    ;;  ;;  ;    ;    ;          ;      ;;    ;     ;;  ;;   ;      ;;      ;;  ;;    ;;   
;;     ;      ;;; ;  ;    ;    ;;;;     ;;;   ;;;;;   ;;;;   ;    ;  ;;;;;   ;;;;;   ;;;;;      ;;;   ;;;;    ;       ;;;;;   ;;; ;    ;;   
;;                                                                                                                                          
;;                                                                                                                                          
;;                                                                                                                                          
;;                                                                                                                                          

(define (functionIsStored?-test-suite)
  (test-equal (term (functionIsStored? (((objr 1)
                                         (function X () ((ref 1) ()) end)))
                                       (function X () ((ref 1) ()) end)))
              (term (objr 1)))
  
  (test-equal (term (functionIsStored? (((objr 1) (function X () ((ref 2) ()) end)))
                                       (function X () ((ref 1) ()) end)))
              (term #f))
  
  (test-equal (term (functionIsStored? (((objr 1) (function X () ((ref 2) ()) end)))
                                       (function Y () ((ref 1) ()) end)))
              (term #f))
  
  (test-results))



(define (test-all-obj-store-metafunctions-suites)
  (derefTheta-test-suite)
  (refBelongsToTheta-test-suite)
  (freshObjRef-test-suite)
  (functionIsStored?-test-suite)
  )
;
(provide test-all-obj-store-metafunctions-suites)
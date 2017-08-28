#lang racket
(require redex
         "../grammar.rkt"
         "./tablesMetafunctions.rkt"
         )

; Access to an object store
; PRE : {the store received satisfy the invariant of representation
;        and the reference dereferenced belongs to the domain of the
;        store}
; ret = (derefTheta θ ref)
; POS : {the correspondent value mapped to the reference}
(define-metafunction core-lang
  derefTheta : θ objref -> object

  [(derefTheta (objrefst_1 ... (objref object) objrefst_2 ...) objref)
   object])

(provide derefTheta)


; Determine if a reference belongs to the domain of a store
; PRE : {the store received satisfy the invariant of representation}
(define-metafunction core-lang
  refBelongsToTheta? : objref θ -> any
  
  ; objref in dom(θ)
  [(refBelongsToTheta? objref (objrefst_1 ... (objref object) objrefst_2 ...))
   #t]
  
  ; Default case
  [(refBelongsToTheta? objref θ)
   #f])

(provide refBelongsToTheta?)


; First location in the object store where values can be stored
; (because the locations since 1 to objStoreFirstLocation-1 are
; reserved to meta-tables of types different than table)
(define objStoreFirstLocation 6)
(provide objStoreFirstLocation)

; Meta-function that generates a fresh objref. Its definition depends
; heavily on the fact that references are implicit generated only by this
; function and that we don't have any kind of garbage collection.
(define-metafunction core-lang
  freshObjRef : θ -> objref
  ; Empty Store
  [(freshObjRef ())
   (objr ,objStoreFirstLocation)]
  
  ; An store with at least one reference
  [(freshObjRef (objrefst_1 ... ((objr Number_1) object)))
   (objr Number_2)
   (where Number_2 ,(+ (term Number_1) 1))])

(provide freshObjRef)

; To add objects to the store
(define-metafunction core-lang
  addObject : θ any -> (θ objref)
  
  [(addObject (objrefst ...) functiondef)
   ((objrefst ... (objref functiondef)) objref)
   
   (where objref (freshObjRef (objrefst ...)))]
  
  [(addObject (objrefst ...) evaluatedtable)
   ((objrefst ... (objref (newTable evaluatedtable))) objref)
   
   (where objref (freshObjRef (objrefst ...)))])

(provide addObject)

; Modify a reference-value mapping on a given store
; PRE : {the store received satisfy the invariant of representation
;        and the reference belongs to the domain of the
;        store}
(define-metafunction core-lang
  thetaAlter : θ objref any -> θ
  [(thetaAlter (objrefst_1 ... (objref intreptable) objrefst_2 ...) objref 
               tableconstructor)
   
   (objrefst_1 ... (objref (modifyTable intreptable tableconstructor)) 
               objrefst_2 ...)]
  
  [(thetaAlter (objrefst_1 ... (objref intreptable) objrefst_2 ...) objref 
               intreptable_2)
   
   (objrefst_1 ... (objref intreptable_2) 
               objrefst_2 ...)]
  )

(provide thetaAlter)

; Determines if there is a closured stored with a given tag and the same body
; PRE : {θ is a proper store && functiondef is the function to be stored}
; ret = (functionIsStored? θ functiondef)
; POS : {if there is a function stored with the same name and the same body as 
;        functiondef => ret is its objref && any other case => ret == #f}
(define-metafunction core-lang
  functionIsStored? : θ functiondef -> any
  ; Base case
  [(functionIsStored? () functiondef)
   ,#f]
  
  ; Inductive cases
  ; A function with the same tag and the same body is already stored 
  [(functionIsStored? ((objref (function Name_1 parameters s end)) objrefst ...)
                      (function Name_1 parameters s end))
   objref]
  
  ; A function with the same tag but different body is stored
  [(functionIsStored? ((objref (function Name_1 parameters s_1 end))
                       objrefst ...) 
                      (function Name_1 parameters s_2 end))
   
   (functionIsStored? (objrefst ...) 
                      (function Name_1 parameters s_2 end))]
  
  ; A function with different tag is stored 
  [(functionIsStored? ((objref (function Name_1 parameters_1 s_1 end))
                       objrefst ...)
                      (function Name_2 parameters_2 s_2 end))
   
   (functionIsStored? (objrefst ...) (function Name_2 parameters_2 s_2 end))]
  
  ; This case is for objects that are not functions
  [(functionIsStored? ((objref object) objrefst ...) functiondef)
   (functionIsStored? (objrefst ...) functiondef)]
  )

(provide functionIsStored?)

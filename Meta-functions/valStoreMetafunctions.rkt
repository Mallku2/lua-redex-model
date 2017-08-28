#lang racket
(require redex
         "../grammar.rkt")

; Determine if a reference belongs to the domain of a store
; PRE : {the store received satisfy the invariant of representation}
(define-metafunction core-lang
  refBelongsTo : r sigma -> any
  ; Base case
  [(refBelongsTo r_1 ())
   #f]
  ; Inductive cases
  [(refBelongsTo r_1 ((r_1 v) rst ...))
   #t]
  [(refBelongsTo r_1 ((r_2 v) rst ...))
   (refBelongsTo r_1 (rst ...))])

; Extension of refBelongsTo, to manage many references
; PRE : {the store received satisfy the invariant of representation}
(define-metafunction core-lang
  ; Base case
  [(refsBelongsTo (r) σ)
   (refBelongsTo r σ)]
  ; Inductive case
  [(refsBelongsTo (r_1 r_2 ...) σ)
   ,(and (term (refBelongsTo r_1 σ)) (term (refsBelongsTo (r_2 ...) σ)))])

(provide refBelongsTo)

; Access to a store (by dereferencing a simpValRef)
; PRE : {the store received satisfy the invariant of representation
;        and the reference dereferenced belongs to the domain of the
;        store}
(define-metafunction core-lang
  derefSigma : σ r -> v
  [(derefSigma (rst_1 ... (r v) rst_2 ...) r)
   v])

(provide derefSigma)

; Modify a reference-value mapping on a given store
; PRE : {the store received satisfy the invariant of representation
;        and the reference belongs to the domain of the
;        store}
(define-metafunction core-lang
  sigmaAlter : σ r v -> σ
  
  [(sigmaAlter (rst_1 ... (r_2 v_2) rst_2 ...) r_2 v_4)
   (rst_1 ... (r_2 v_4) rst_2 ...)])

(provide sigmaAlter)

; Meta-function that generates a fresh reference in sigma. Its definition
; depends heavily on the fact that references are implicit generated only by
; this function and that we don't have any kind of garbage collection.
(define-metafunction core-lang
  freshSigmaRef : σ -> r
  ; Empty Store
  [(freshSigmaRef ())
   (ref 1)]

  ; An store with at least one reference
  [(freshSigmaRef (rst ... ((ref Number_1) v)))
   (ref Number_2)

   (where Number_2 ,(+ (term Number_1) 1))])

; To add simplevalues to the store
(define-metafunction core-lang
  addSimpVal : σ (v ...) -> (σ (r ...))
  ; Base case
  [(addSimpVal (rst ...) ())
   ((rst ...) ())]
  
  ; Inductive case
  [(addSimpVal (rst ...) (v_1 v_2 ...))
   (σ (r_1 r ...))

   (where r_1 (freshSigmaRef (rst ...)))
   (where (σ (r ...)) (addSimpVal (rst ... (r_1 v_1)) (v_2 ...)))])

(provide addSimpVal)
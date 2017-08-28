#lang racket
(require redex
         "../grammar.rkt")


(define-metafunction core-lang
  ; Binary arithmetic operators
  [(\#errmessage ArithWrongOps string_1 string_2)
   ,(string-append "attempt to perform arithmetic on a "
                   (term string_1)
                   " value.")
   
   (side-condition (not (equal? (term string_1)
                                "number")))]
  
  [(\#errmessage ArithWrongOps string_1 string_2)
   ,(string-append "attempt to perform arithmetic on a "
                   (term string_2)
                   " value.")
   
   (side-condition (not (equal? (term string_2)
                                "number")))]
  
  ; String concatenation
  [(\#errmessage StrConcatWrongOps string_1 string_2)
   ,(string-append "attempt to concatenate a "
                   (term string_1)
                   " value.")
   
   (side-condition (not (equal? (term string_1)
                                "string")))]
  
  [(\#errmessage StrConcatWrongOps string_1 string_2)
   ,(string-append "attempt to concatenate a "
                   (term string_2)
                   " value.")
   
   (side-condition (not (equal? (term string_2)
                                "string")))]
  
  ; Order comparison
  [(\#errmessage OrdCompWrongOps string_1 string_2)
   ,(string-append "attempt to compare "
                   (term string_1)
                   " with "
                   (term string_2)
                   ".")]
  
  ; Negation
  [(\#errmessage NegWrongOp string)
   ,(string-append "attempt to perform arithmetic on a "
                   (term string)
                   " value.")]
  
  ; String length
  [(\#errmessage StrLenWrongOp string)
   ,(string-append "attempt to get length of a "
                   (term string)
                   " value.")]
  
  ; Wrong function call
  [(\#errmessage WrongFunCall string)
   ,(string-append "attempt to call a "
                   (term string)
                   " value.")]
  
  ; Table related errors
  [(\#errmessage specCondLabel string)
   ,(string-append "attempt to index a "
                   (term string)
                   " value.")
   
   (side-condition (or (equal? (term specCondLabel)
                               (term NonTableIndexed))
                       
                       (equal? (term specCondLabel)
                               (term FieldAssignOverNonTable))))])

(provide \#errmessage)

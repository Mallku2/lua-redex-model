#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/errorMessagesMetafunctions.rkt")

(define (errmessages-test-suite)
  ; Arithmetic operations
  (test-equal (term (\#errmessage ArithWrongOps "table" "number"))
              "attempt to perform arithmetic on a table value.")

  (test-equal (term (\#errmessage ArithWrongOps "number" "table"))
              "attempt to perform arithmetic on a table value.")

  ; String concatenation
  (test-equal (term (\#errmessage StrConcatWrongOps "table" "string"))
              "attempt to concatenate a table value.")

  (test-equal (term (\#errmessage StrConcatWrongOps "string" "table"))
              "attempt to concatenate a table value.")

  ; Order comparison
  (test-equal (term (\#errmessage OrdCompWrongOps "string" "table"))
              "attempt to compare string with table.")

  ; Negation
  (test-equal (term (\#errmessage NegWrongOp "string"))
              "attempt to perform arithmetic on a string value.")

  ; String length
  (test-equal (term (\#errmessage StrLenWrongOp "boolean"))
              "attempt to get length of a boolean value.")

  ; Wrong function call
  (test-equal (term (\#errmessage WrongFunCall "boolean"))
              "attempt to call a boolean value.")

  ; Table related errors
  (test-equal (term (\#errmessage NonTableIndexed "boolean"))
              "attempt to index a boolean value.")

  (test-equal (term (\#errmessage FieldAssignOverNonTable "boolean"))
              "attempt to index a boolean value.")

  
  (test-results))

(provide errmessages-test-suite)
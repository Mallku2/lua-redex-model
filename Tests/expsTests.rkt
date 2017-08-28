#lang racket
; Black-box testing for expression that don't interact with some store

(require redex
         "../grammar.rkt"
         "../Relations/exps.rkt")

(define (exps-test-suite)
  ; Operator ()
  (test-->> exps
            (term (\( (< 1 2 3 >) \)))
            1)
  (test-->> exps
            (term (\( (< >) \)))
            (term nil))
  (test-->> exps
            (term (\( 1 \)))
            (term 1))
 
  ; Arithmetic Operations
  (test-->> exps
            (term (1 + 1))
            (term 2.0))
  (test-->> exps
            (term (1 - 1))
            (term 0.0))
  (test-->> exps
            (term (1 * 1))
            (term 1.0))
  (test-->> exps
            (term (1 / 1))
            (term 1.0))
  (test-->> exps
            (term (1 ^ 1))
            (term 1.0))
  (test-->> exps
            (term (1 % 1))
            (term 0.0))
  ; Equality comparison
  (test-->> exps
            (term (1 == 1))
            (term true))
  (test-->> exps
            (term ("a" == "a"))
            (term true))
  ; Number order comparison
  (test-->> exps
            (term (1 < 2))
            (term true))
  (test-->> exps
            (term (2 > 1))
            (term true))
  (test-->> exps
            (term (2 < 1))
            (term false))
  (test-->> exps
            (term (2 <= 1))
            (term false))
  (test-->> exps
            (term (1 >= 2))
            (term false))
  (test-->> exps
            (term (2 >= 2))
            (term true))
  (test-->> exps
            (term (1 <= 2))
            (term true))
  ; String order comparison
  (test-->> exps
            (term ("a" < "a"))
            (term false))
  (test-->> exps
            (term ("a" > "a"))
            (term false))
  (test-->> exps
            (term ("a" < "b"))
            (term true))
  (test-->> exps
            (term ("a" <= "a"))
            (term true))
  (test-->> exps
            (term ("a" <= "b"))
            (term true))
  (test-->> exps
            (term ("b" >= "a"))
            (term true))
  ; String concatenation
  (test-->> exps
            (term ("a" .. "b"))
            (term "ab"))
  (test-->> exps
            (term ("" .. "b"))
            (term "b"))

  (test-->> exps
            (term ("1" .. 2.0))
            (term "12"))
  
  (test-->> exps
            (term (1 .. "2.0"))
            (term "12.0"))
  ; String length
  (test-->> exps
            (term (\# "a"))
            (term 1))
  ; Logical conectives
  (test-->> exps
            (term (1 and (X ())))
            (term (\( (X ()) \))))
  (test-->> exps
            (term (nil and 2))
            (term nil))
  (test-->> exps
            (term (true and (ref 2)))
            (term (\( (ref 2) \))))
  (test-->> exps
            (term (false and 2))
            (term false))
  (test-->> exps
            (term (1 or 2))
            (term 1))
  (test-->> exps
            (term (false or 2))
            (term 2))
  (test-->> exps
            (term (nil or 2))
            (term 2))
  (test-->> exps
            (term (not 1))
            (term false))
  (test-->> exps
            (term (not nil))
            (term true))
  (test-->> exps
            (term (not false))
            (term true))
  ; Coercion
  (test-->> exps
            (term ("0x2.0p0" + 1.0))
            (term ((2.0 * (2 ^ 0)) + 1.0)))
  (test-->> exps
            (term ("   0x2.0p0   " + 1.0))
            (term ((2.0 * (2 ^ 0)) + 1.0)))
  (test-->> exps
            (term (1 + "0x1.0p0"))
            (term (1 + (1.0 * (2 ^ 0)))))
  (test-->> exps
            (term ("0x1.0p0" - 1))
            (term ((1.0 * (2 ^ 0)) - 1)))
  (test-->> exps
            (term (1 - "0x1.0p0"))
            (term (1 - (1.0 * (2 ^ 0)))))
  (test-->> exps
            (term ("0x1.0p0" * 1))
            (term ((1.0 * (2 ^ 0)) * 1)))
  (test-->> exps
            (term (1 * "0x1.0p0"))
            (term (1 * (1.0 * (2 ^ 0)))))
  (test-->> exps
            (term ("0x1.0p0" / 1))
            (term ((1.0 * (2 ^ 0)) / 1)))
  (test-->> exps
            (term (1.0 / "0x1.0p0"))
            (term (1.0 / (1.0 * (2 ^ 0)))))
  (test-->> exps
            (term ("0x1.0p0" ^ 1.0))
            (term ((1.0 * (2 ^ 0)) ^ 1.0)))
  (test-->> exps
            (term (1.0 ^ "0x1.0p0"))
            (term (1.0 ^ (1.0 * (2 ^ 0)))))
  (test-->> exps
            (term ("0x1.0p0" % 1.0))
            (term ((1.0 * (2 ^ 0)) % 1.0)))
  (test-->> exps
            (term (1.0 % "0x1.0p0"))
            (term (1.0 % (1.0 * (2 ^ 0)))))
  (test-->> exps
            (term (- "0x1.0p0"))
            (term (- (1.0 * (2 ^ 0)))))
  
  
  ; Abnormal expressions
  (test-->> exps
            (term ("a" + 1))
            (term (("a" + 1)ArithWrongOps)))
  (test-->> exps
            (term (1 + "a"))
            (term ((1 + "a")ArithWrongOps)))
  (test-->> exps
            (term ("0xq" + 1))
            (term (("0xq" + 1)ArithWrongOps)))
  (test-->> exps
            (term (1 + "0xq"))
            (term ((1 + "0xq")ArithWrongOps)))
  (test-->> exps
            (term (1 + "0x1.q"))
            (term ((1 + "0x1.q")ArithWrongOps)))
  (test-->> exps
            (term (1 + "0x1.1pq"))
            (term ((1 + "0x1.1pq")ArithWrongOps)))
  (test-->> exps
            (term ("a" - 1))
            (term (("a" - 1)ArithWrongOps)))
  (test-->> exps
            (term (1 - "a"))
            (term ((1 - "a")ArithWrongOps)))
  (test-->> exps
            (term ("0xq" - 1))
            (term (("0xq" - 1)ArithWrongOps)))
  (test-->> exps
            (term (1 - "0xq"))
            (term ((1 - "0xq")ArithWrongOps)))
  (test-->> exps
            (term (1 - "0x1.q"))
            (term ((1 - "0x1.q")ArithWrongOps)))
  (test-->> exps
            (term (1 - "0x1.1pq"))
            (term ((1 - "0x1.1pq")ArithWrongOps)))
  (test-->> exps
            (term ("a" * 1))
            (term (("a" * 1)ArithWrongOps)))
  (test-->> exps
            (term (1 * "a"))
            (term ((1 * "a")ArithWrongOps)))
  (test-->> exps
            (term ("0xq" * 1))
            (term (("0xq" * 1)ArithWrongOps)))
  (test-->> exps
            (term (1 * "0xq"))
            (term ((1 * "0xq")ArithWrongOps)))
  (test-->> exps
            (term (1 * "0x1.q"))
            (term ((1 * "0x1.q")ArithWrongOps)))
  (test-->> exps
            (term (1 * "0x1.1pq"))
            (term ((1 * "0x1.1pq")ArithWrongOps)))
  (test-->> exps
            (term ("a" ^ 1))
            (term (("a" ^ 1)ArithWrongOps)))
  (test-->> exps
            (term (1 ^ "a"))
            (term ((1 ^ "a")ArithWrongOps)))
  (test-->> exps
            (term ("0xq" ^ 1))
            (term (("0xq" ^ 1)ArithWrongOps)))
  (test-->> exps
            (term (1 ^ "0xq"))
            (term ((1 ^ "0xq")ArithWrongOps)))
  (test-->> exps
            (term (1 ^ "0x1.q"))
            (term ((1 ^ "0x1.q")ArithWrongOps)))
  (test-->> exps
            (term (1 ^ "0x1.1pq"))
            (term ((1 ^ "0x1.1pq")ArithWrongOps)))
  (test-->> exps
            (term ("a" % 1))
            (term (("a" % 1)ArithWrongOps)))
  (test-->> exps
            (term (1 % "a"))
            (term ((1 % "a")ArithWrongOps)))
  (test-->> exps
            (term ("0xq" % 1))
            (term (("0xq" % 1)ArithWrongOps)))
  (test-->> exps
            (term (1 % "0xq"))
            (term ((1 % "0xq")ArithWrongOps)))
  (test-->> exps
            (term (1 % "0x1.q"))
            (term ((1 % "0x1.q")ArithWrongOps)))
  (test-->> exps
            (term (1 % "0x1.1pq"))
            (term ((1 % "0x1.1pq")ArithWrongOps)))
  (test-->> exps
            (term (- "a"))
            (term ((- "a")NegWrongOp)))
  (test-->> exps
            (term (- "0xq"))
            (term ((- "0xq")NegWrongOp)))
  (test-->> exps
            (term (- "0x1.q"))
            (term ((- "0x1.q")NegWrongOp)))
  (test-->> exps
            (term (- "0x1.1pq"))
            (term ((- "0x1.1pq")NegWrongOp)))
  (test-->> exps
            (term ("a" .. (objr 1)))
            (term (("a" .. (objr 1))StrConcatWrongOps)))
  (test-->> exps
            (term (\# (objr 1)))
            (term ((\# (objr 1))StrLenWrongOp)))
  (test-->> exps
            (term ("a" == "b"))
            (term (("a" == "b")EqFail)))
  (test-->> exps
            (term (true == 1))
            (term ((true == 1)EqFail)))
  (test-->> exps
            (term ((objr 1) < (objr 1)))
            (term (((objr 1) < (objr 1))OrdCompWrongOps)))
  (test-->> exps
            (term ((objr 1) <= (objr 1)))
            (term (((objr 1) <= (objr 1))OrdCompWrongOps)))
  
  
  (test-results))

(provide exps-test-suite)

#lang racket
(require redex
         "phrases_constructors.rkt"
         rackunit
         rackunit/text-ui)

(define-test-suite phrases-constructors-test-suite
  ; statements
  (check-equal? (concrete-grammar-s (skip))
             (term \;))
  
  (check-equal? (concrete-grammar-s (break))
             (term break))

  (check-equal? (concrete-grammar-s (return (tuple (exps (list (number 1))))))
             (term (return (< 1 >))))

  (check-equal? (concrete-grammar-s (fun-call (id-name 'x) (exps (list (number 1)))))
             (term (x (1))))

  (check-equal? (concrete-grammar-s (method-call (id-name 'x) (id-name 'y) (exps (list (number 1)))))
             (term (x : y (1))))

  (check-equal? (concrete-grammar-s (built-in-call (id-name 'service) (exps (list (number 1)))))
             (term (\$builtIn service (1))))

  (check-equal? (concrete-grammar-s (conditional (nil) (skip) (break)))
          (term (if nil then \; else break end)))

  (check-equal? (concrete-grammar-s (while (true) (skip)))
          (term (while true do \; end)))

  (check-equal? (concrete-grammar-s (local-vars (exps (list (id-name 'x) (id-name 'y)))
                                                (exps (list (number 1)))
                                                (skip)))
          (term (local (x y) = (1) in \; end)))

  (check-equal? (concrete-grammar-s (var-assign (exps (list (id-name 'x) (id-name 'y)))
                                                (exps (list (number 1) (number 2)))))
          (term ((x y) = (1 2))))

  (check-equal? (concrete-grammar-s (conc-stats (list (skip)
                                                      (var-assign (exps (list (id-name 'x) (id-name 'y)))
                                                                  (exps (list (number 1) (number 2)))))))
                (term (\;
                       ((x y) = (1 2)))))

  ; expressions
  (check-equal? (concrete-grammar-e (nil))
          (term nil))

  (check-equal? (concrete-grammar-e (true))
          (term true))

  (check-equal? (concrete-grammar-e (false))
          (term false))

  (check-equal? (concrete-grammar-e (number 1))
          (term 1))

  (check-equal? (concrete-grammar-e (number 1.1))
          (term 1.1))

  (check-equal? (concrete-grammar-e (str "asd"))
          (term "asd"))

  (check-equal? (concrete-grammar-e (id-name 'asd))
          (term asd))

  (check-equal? (concrete-grammar-e (id-vararg))
          (term <<<))

  (check-equal? (concrete-grammar-e (var-table-field (number 1) (number 2)))
          (term (1 \[ 2 \])))

  (check-equal? (concrete-grammar-e (parent-e (number 1)))
          (term (\( 1 \))))

  (check-equal? (concrete-grammar-e (binop (add)
                                                (number 1)
                                                (number 2)))
                (term (1 + 2)))

  (check-equal? (concrete-grammar-e (binop (sub)
                                                (number 1)
                                                (number 2)))
                (term (1 - 2)))

  (check-equal? (concrete-grammar-e (binop (mul)
                                                (number 1)
                                                (number 2)))
                (term (1 * 2)))

  (check-equal? (concrete-grammar-e (binop (div)
                                                (number 1)
                                                (number 2)))
                (term (1 / 2)))

  (check-equal? (concrete-grammar-e (binop (pow)
                                                (number 1)
                                                (number 2)))
                (term (1 ^ 2)))

  (check-equal? (concrete-grammar-e (binop (mod)
                                                (number 1)
                                                (number 2)))
                (term (1 % 2)))

  (check-equal? (concrete-grammar-e (binop (lt)
                                                (number 1)
                                                (number 2)))
                (term (1 < 2)))

  (check-equal? (concrete-grammar-e (binop (le)
                                                (number 1)
                                                (number 2)))
                (term (1 <= 2)))

  (check-equal? (concrete-grammar-e (binop (gt)
                                                (number 1)
                                                (number 2)))
                (term (1 > 2)))

  (check-equal? (concrete-grammar-e (binop (ge)
                                                (number 1)
                                                (number 2)))
                (term (1 >= 2)))

  (check-equal? (concrete-grammar-e (binop (eq)
                                                (number 1)
                                                (number 2)))
                (term (1 == 2)))

  (check-equal? (concrete-grammar-e (binop (\\and)
                                                (number 1)
                                                (number 2)))
                (term (1 and 2)))

  (check-equal? (concrete-grammar-e (binop (\\or)
                                                (number 1)
                                                (number 2)))
                (term (1 or 2)))

  (check-equal? (concrete-grammar-e (binop (str-concat)
                                                (number "1")
                                                (number "2")))
                (term ("1" .. "2")))

  (check-equal? (concrete-grammar-e (unop (unm)
                                               (number 1)))
                (term (- 1)))

  (check-equal? (concrete-grammar-e (unop (\\not)
                                               (true)))
                (term (not true)))

  (check-equal? (concrete-grammar-e (unop (len)
                                               (number "asd")))
                (term (\# "asd")))

  (check-equal? (concrete-grammar-e (tuple (exps (list (number 1) (number 2)))))
          (term (< 1 2 >)))

  (check-equal? (concrete-grammar-e (tableconstructor
                               (fields (list
                                             (kv-table-field
                                              (number 1)
                                              (number 2))
                                             (v-table-field (number 3))))))
          (term (\{ (\[ 1 \] = 2) 3 \})))

  (check-equal? (concrete-grammar-e (func-def (id-name 'x)
                                             (params (exps (list (id-name 'y))))
                                             (skip)))
          (term (function x (y) \; end)))

  )

(provide phrases-constructors-test-suite)
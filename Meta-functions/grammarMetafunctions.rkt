#lang racket
(require redex
         "../grammar.rkt")
; Predicates over grammar's symbols, to ease the definition of some rules
; from the model
(define-metafunction core-lang
  [(isArithBinOp +)
   #t]
  
  [(isArithBinOp -)
   #t]
  
  [(isArithBinOp *)
   #t]
  
  [(isArithBinOp %)
   #t]
  
  [(isArithBinOp ^)
   #t]
  
  [(isArithBinOp /)
   #t]
  
  [(isArithBinOp any)
   #f])

(provide isArithBinOp)


(define-metafunction core-lang
  [(isRelationalOperator <)
   #t]
  
  [(isRelationalOperator <=)
   #t]
  
  [(isRelationalOperator any)
   #f])

(provide isRelationalOperator)


(define-metafunction core-lang
  [(isNumberBinOp binop)
   (or (isArithBinOp binop)
       (isRelationalOperator binop))])

(provide isNumberBinOp)


(define-metafunction core-lang
  [(translateComparisonOp >)
   <]
  
  [(translateComparisonOp >=)
   <=])

(provide translateComparisonOp)

(define-metafunction core-lang
  [(isBooleanBinOp and)
   #t]
  
  [(isBooleanBinOp or)
   #t]
  
  [(isBooleanBinOp any)
   #f])

(provide isBooleanBinOp)


(define-metafunction core-lang
  [(isStringBinOp binop)
   (or (term (isRelationalOperator binop))
       (equal? (term binop) (term ..)))])

(provide isStringBinOp)


(define-metafunction core-lang
  [(fixUnwrap (v_1 (v_2 ... hole)) (v_3 ...))
   (v_1 (v_2 ... v_3 ...))]
  
  [(fixUnwrap ($builtIn Name  (v_1 ... hole)) (v_2 ...))
   ($builtIn Name  (v_1 ... v_2 ...))]
  
  [(fixUnwrap (< v_1 ... hole >) (v_2 ...))
   (< v_1 ... v_2 ... >)]
  
  [(fixUnwrap (\{ field ... hole \}) (v ...))
   (\{ field ... v ... \})]
  
  [(fixUnwrap (local (Name ...) = (v_1 ... hole) in s end) (v_2 ...))
   (local (Name ...) = (v_1 ... v_2 ...) in s end)]
  
  [(fixUnwrap ((evar ...) = (v_1 ... hole)) (v_2 ...))
   ((evar ...) = (v_1 ... v_2 ...))]
  )

(provide fixUnwrap)

; To flat a list of symbols into an string representing the corresponding
; s-expression (needed for the implementation of string.dump).
(define (str-flatten l)
  (string-append
   "("
   (foldr
    (lambda (elem acum)
      (if (symbol? elem)
          ; Some special treatment for brackets, curly braces, etc
          (cond
            [(equal? elem '\[)
             (string-append "\\[" " " acum)]
            [(equal? elem '\])
             (string-append "\\]" " " acum)]
            [(equal? elem '\{)
             (string-append "\\{" " " acum)]
            [(equal? elem '\})
             (string-append "\\}" " " acum)]
            [(equal? elem '\;)
             (string-append "\\;" " " acum)]
            [(equal? elem '\#)
             (string-append "\\#" " " acum)]
            ; No problem.
            [else (string-append (symbol->string elem) " " acum)])
          ; Other special cases: strings and numbers
          (cond [(string? elem)
                 (string-append (string-append "\"" elem "\"") " " acum)]
                [(number? elem)
                 (string-append (number->string elem) " " acum)]
                ; Then, it should by a nested structure. Recursive call.
                [else (string-append (str-flatten elem) " " acum)]
                )))
    ")"
    l)))

(provide str-flatten)
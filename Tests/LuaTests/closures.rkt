#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/standardReductionRelation.rkt"
         "../../Desugar/parser.rkt")

(define (ok? red)
  (and (eq? (length red) 1)

       (redex-match core-lang
              (σ : θ : \;)
              (first red))))

(define (lua-closures-test-suite)
  (test-predicate ok? (apply-reduction-relation* stand-reduc-rel
                          (term (plugIntoExecutionEnvironment
                                 (unquote (parse-this (file->string "closure.lua") #f (void)))))))
  (test-results))

(provide lua-closures-test-suite)

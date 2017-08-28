#lang racket
(require redex
         "../../grammar.rkt"
         "../../executionEnvironment.rkt"
         "../../Relations/fullProgs.rkt"
         "../../Desugar/parser.rkt")

(define (ok? red)
  (and (eq? (length red) 1)

       (redex-match core-lang
              (σ : θ : \;)
              (first red))))

(define (lua-calls-test-suite)
  (test-predicate ok? (apply-reduction-relation* full-progs-rel
                          (term (plugIntoExecutionEnvironment
                                 (unquote (parse-this (file->string "calls.lua") #f (void)))))))
  (test-results))

(provide lua-calls-test-suite)

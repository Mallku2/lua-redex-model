#lang racket
(require redex
         ; Meta-functions test suites
         "./deltaTests.rkt"
         "./objStoreMetafunctionsTests.rkt"
         "./substitutionTests.rkt"
         ; Notions of reduction test suites
         "./expsTests.rkt"
         "./expsValStoreTests.rkt"
         "./expsObjStoreTests.rkt"
         "./functionCallTests.rkt"
         "./statsTests.rkt"
         "./statsValStoreTests.rkt"
         "./statsObjStoreTests.rkt"
         "./abnormalStatsTests.rkt"
         "./abnormalExpsTests.rkt"
         "./builtInTests.rkt"
         "./fullProgsTests.rkt"
         "./executionEnvironmentTests.rkt"
         )

(define (test-all-metafunctions)
  (print "delta-test-suite :")
  (delta-test-suite)
  (print "obj-store-metafunctions-test-suite :")
  (test-all-obj-store-metafunctions-suites)
  (print "substitution-test-suite :")
  (subs-test-suite)
  (print "test-all-meta-table-mech-metafunctions-test-suites: ")
  (test-all-meta-table-mech-metafunctions-test-suites)
  )

(define (test-all-reductions)
  (print "exps-test-suite: ")
  (exps-test-suite)
  (print "exps-val-store-test-suite:")
  (exps-val-store-test-suite)
  (print "exps-obj-store-red-test-suite:")
  (exps-obj-store-test-suite)
  (print "func-call-test-suite:")
  (func-call-test-suite)
  (print "abnormal-exps-test-suite:")
  (abnormal-exps-test-suite)
  (print "stats-test-suite: ")
  (stats-test-suite)
  (print "stats-val-store-test-suite:")
  (stats-val-store-test-suite)
  (print "stats-obj-store-test-suite:")
  (stats-obj-store-test-suite)
  (print "abnormal-stats-test-suite:")
  (abnormal-stats-test-suite)
  (print "built-in-test-suite:")
  (built-in-test-suite)
  (print "full-progs-rel-test-suite: ")
  (full-progs-rel-test-suite)
  (print "execution-environment-test-suite")
  (execution-environment-test-suite)
  )

(define (test-all)
  (test-all-metafunctions)
  (test-all-reductions))

(test-all)

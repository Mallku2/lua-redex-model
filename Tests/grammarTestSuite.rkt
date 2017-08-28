#lang racket
(require redex
         "../grammar.rkt")

(define (evaluation-contexts-test-suite)

  ; Elf are all the possible evaluation contexts, with no labelled blocks 
  (test-predicate (redex-match core-lang Elf)
                  (term (do hole end)))

  (test-predicate (redex-match core-lang Elf)
                  (term ((hole)ProtectedMode)))

  (test-predicate (redex-match core-lang Elf)
                  (term (((do hole end))ProtectedMode)))

  (test-predicate (redex-match core-lang Elf)
                  (term (do ((hole)ProtectedMode) end)))

  (test-predicate (lambda (t)
                    (not ((redex-match core-lang Elf) t)))
                  (term (:: X :: \{ hole \})))

  ; Enp are all the possible evaluation contexts, with no protected mode 
  (test-predicate (redex-match core-lang Enp)
                  (term (do hole end)))

  (test-predicate (redex-match core-lang Enp)
                  (term (:: X :: \{ hole \})))

  (test-predicate (redex-match core-lang Enp)
                  (term (:: X :: \{ (do hole end) \})))

  (test-predicate (redex-match core-lang Enp)
                  (term (do (:: X :: \{ hole \}) end)))

  (test-predicate (lambda (t)
                    (not ((redex-match core-lang Enp) t)))
                  (term ((hole)ProtectedMode)))

  (test-results)
)

(provide evaluation-contexts-test-suite)

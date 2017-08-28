#lang racket
(require redex
         "scope.rkt"
         rackunit
         rackunit/text-ui)

(define actual-scope
  (void))

(define-test-suite scope-test-suite
  ; Actual scope
  #:before (lambda ()
             (set! actual-scope
                   (new-scope (new-empty-scope) (list 'a 'b))))
  
  (check-true (is-in-scope actual-scope 'a))

  (check-true (is-in-scope actual-scope 'b))

  (check-false (is-in-scope actual-scope 'c))

  ; Variables in external scopes are still visibles
  ; New scope
  (check-true ((lambda ()
                 (set! actual-scope (new-scope actual-scope (list 'c)))
                 (is-in-scope actual-scope 'a))))

  (check-true (is-in-scope actual-scope 'b))
  
  (check-true (is-in-scope actual-scope 'c))

  ; Another scope
  (check-true ((lambda ()
                 (set! actual-scope (new-scope actual-scope (list 'd)))
                 (is-in-scope actual-scope 'a))))

  (check-true (is-in-scope actual-scope 'b))
  
  (check-true (is-in-scope actual-scope 'c))
  
  (check-true (is-in-scope actual-scope 'd))
  
  ; Restore previous scopes
  (check-true ((lambda ()
                 (set! actual-scope (scope-previous-scopes actual-scope))
                 (match actual-scope
                   ((scope (list 'c) prev-scope) #t)
                   (_ #f)))))

  (check-true ((lambda ()
                 (set! actual-scope (scope-previous-scopes actual-scope))
                 (match actual-scope
                   ((scope (list 'a 'b) prev-scope) #t)
                   (_ #f)))))

  ; Blocks
  (check-true ((lambda ()
                 (new-block)
                 (set! actual-scope (new-scope actual-scope (list 'c)))
                 (set! actual-scope (close-scopes-in-block actual-scope))
                 (match actual-scope
                   ((scope (list 'a 'b) prev-scope) #t)
                   (_ #f)))))

  (check-true ((lambda ()
                 (new-block)
                 (set! actual-scope (new-scope actual-scope (list 'c)))
                 (set! actual-scope (new-scope actual-scope (list 'd)))
                 (set! actual-scope (close-scopes-in-block actual-scope))
                 (match actual-scope
                   ((scope (list 'a 'b) prev-scope) #t)
                   (_ #f)))))

  (check-true ((lambda ()
                 (new-block)
                 (set! actual-scope (new-scope actual-scope (list 'c)))
                 (set! actual-scope (new-scope actual-scope (list 'd)))
                 (set! actual-scope (new-scope actual-scope (list 'e)))
                 (set! actual-scope (close-scopes-in-block actual-scope))
                 (match actual-scope
                   ((scope (list 'a 'b) prev-scope) #t)
                   (_ #f)))))
  
)
(provide scope-test-suite)
  

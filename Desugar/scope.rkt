#lang racket
(provide (all-defined-out))

; Scopes as chained symbol tables
(struct scope (symbols previous-scopes))

(struct block (scopes previous-block))

(define func-defs
  0)

(define (new-func-def)
  (set! func-defs (+ func-defs 1)))

(define (new-label)
  (new-func-def)
  (string->symbol (string-append "$"
                                 (number->string func-defs))))

; Each time a new block is being analized, this procedure must be called to
; reset the internal counters of scopes
(define (new-block previous-block)
  (block (new-empty-scope) previous-block))

; An empty scope, to begin with the parsing
(define (new-empty-scope)
  (scope '() (void)))

(define (new-empty-block)
  (block (new-empty-scope) #f))

; Reset internal state
(define (reset-symbol-table)
  (set! func-defs 0))

; Close every local variables' scope, that were opened into a block that is
; being closed. Returns the scope external to the block being closed.
(define (close-scopes-in-block actual-block)
  (match actual-block
    ((block scopes previous-block)
     previous-block)
    (#f
     (new-empty-block))

    (- (error "close-scopes-in-block, unexpected values: " actual-block))))

; Creates a new scope, internal to the given, where the indicated symbols are
; defined
(define (new-scope actual-block symbols)
  
  (match actual-block
    
    ((block scopes previous-block)
     
     (match scopes
       ((scope previous-symbols previous-scopes)
        (block (scope symbols (scope previous-symbols previous-scopes)) previous-block))

       (- (error "new-scope, unexpected scope value: " scopes symbols))))

    (#f
     (block (scope symbols (void)) #f)
     )

    (-
     (error "new-scope, unexpected values: " actual-block))))

; Determine if a given symbol is defined in the actual scope
(define (is-in-scope actual-scope symbol)
  (match actual-scope
    ((scope '() previous-scope) #f)
    
    ((scope symbols previous-scope)
     (if (member symbol symbols)
         #t
         (is-in-scope previous-scope symbol)))
    
    (- (error "is-in-scope, unexpected values: " actual-scope symbol))))

; Determine if a given symbol is defined in the actual block
(define (is-in-block actual-block symbol)
  (match actual-block
    ((block scopes #f)
     (if (is-in-scope scopes symbol)
         #t
         #f
         ))
     
    ((block scopes previous-block)
     (if (is-in-scope scopes symbol)
         #t
         (is-in-block previous-block symbol)
         ))
    
    (#f
     #f)
    
    (- (error "is-in-block, unexpected values: " actual-block symbol))))

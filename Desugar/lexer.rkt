#lang racket

(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre))

(provide (all-defined-out))

(provide token-value)

(provide token-name)

(define-empty-tokens empty-tokens (EOF
                                   ; statements
                                   \;
                                   :
                                   BREAK
                                   RETURN
                                   REPEAT
                                   UNTIL
                                   IF
                                   THEN
                                   ELSE
                                   ELSEIF
                                   END
                                   DO
                                   WHILE
                                   FOR
                                   IN
                                   LOCAL
                                   FUNCTION
                                   =
                                   \[
                                   \]
                                   \(
                                   \)
                                   \{
                                   \}
                                   \.
                                   \,
                                   ; expressions
                                   +
                                   -
                                   *
                                   /
                                   %
                                   ^
                                   CONCAT
                                   LT
                                   LE
                                   GT
                                   GE
                                   EQ
                                   NOTEQ
                                   AND
                                   OR
                                   NOT
                                   \#
                                   NIL
                                   FALSE
                                   TRUE
                                   VARARG
                                   ; tokens for resolving shif-reduce conflicts
                                   BEGINNING_DO_END
                                   BEGINNING_WHILE
                                   UNM ; arith. negation
                                   ))

(define-tokens non-empty-tokens (STRING NUMBER HEX-NMBR-BINPOT NAME))

(define-lex-abbrevs
  (double-quoted-string-lit (re-: "\""
                                  (re-* (char-complement #\"))
                                  "\""))
  
  (mult-lines-string-lit (re-: "[["
                               (re-* (re-or (re-~ #\])
                                            (re-: #\]
                                                  (re-~ #\]))))
                               "]]"))
  
  (single-quoted-string-lit (re-: "'"
                                  (re-* (char-complement #\'))
                                  "'"))
  
  (digits (char-range "0" "9"))
  
  (hex-digits (re-or (char-range "0" "9")
                     (char-range "A" "F")
                     (char-range "a" "f")))
  
  ; Decimal number, with optional fractional part
  (simp-number-lit (re-or (concatenation (re-+ digits)
                                         (re-? ".")
                                         (re-* digits))

                          (concatenation (re-* digits)
                                         (re-? ".")
                                         (re-+ digits))))
  
  ; Hexadecimal number, with optional fractional part
  (simp-hex-number-lit (concatenation (re-or "0x" "0X")
                                      (re-* hex-digits)
                                      (re-? ".")
                                      (re-* hex-digits)))
  
  ; Decimal number, with optional fractional part and decimal exponent
  (scient-number-lit (concatenation simp-number-lit
                                    (re-or "e" "E")
                                    (re-or (concatenation (re-or "-" "+")
                                                          (re-+ digits))
                                           (re-+ digits))))
  
  ; Hexadecimal number, with optional fractional part and binary exponent
  (hex-number-bin-exp-lit (concatenation simp-hex-number-lit
                                         (re-or "p" "P")
                                         (re-or (concatenation (re-or "-" "+")
                                                               (re-+ digits))
                                                (re-+ digits))))
  
  (number-lit (re-or simp-number-lit
                     scient-number-lit))
  
  (hex-number-lit (re-or simp-hex-number-lit
                         hex-number-bin-exp-lit))
  
  (id-beg-chars (re-or (char-range "a" "z")
                       (char-range "A" "Z")
                       "_"))
  
  (id (concatenation id-beg-chars
                     (re-* (re-or id-beg-chars
                                  "_"
                                  digits)))))

; Lexer for Lua
(define lua-lexer
  (lexer ; literals
   ("nil" (token-NIL))
   ("false" (token-FALSE))
   ("true" (token-TRUE))
   ; exact->inexact, to use IEEE floating-point representation of a number,
   ; same as Lua
   (number-lit (token-NUMBER (exact->inexact (string->number lexeme))))
   
   ; Translate to Racket's hexadecimal numbers' notation
   (simp-hex-number-lit (token-NUMBER (string->number (string-replace lexeme
                                                                      (regexp "0x|0X") "#x"))))
   
   (hex-number-bin-exp-lit (token-HEX-NMBR-BINPOT
                            ; Extract hexadecimal number and binary exponent
                            ((lambda ()
                               (define hex-split (string-split lexeme (regexp "p|P")))
                               
                               (list (string->number (string-replace (list-ref hex-split 0)
                                                                     (regexp "0x|0X") "#x"))
                                     (string->number (list-ref hex-split 1)))))))
   
   (double-quoted-string-lit (token-STRING
                              ; Remove unprintable characters
                              ; (embedded zeros)
                              (clean (substring lexeme
                                                1
                                                (- (string-length lexeme) 1)))))
   
   (single-quoted-string-lit (token-STRING
                              ; Remove unprintable characters
                              ; (embedded zeros)
                              (clean (substring lexeme
                                                1
                                                (- (string-length lexeme) 1)))))
   
   (mult-lines-string-lit (token-STRING (clean
                                         ; Delete [[ and ]]
                                         (substring lexeme
                                                    2
                                                    (- (string-length lexeme) 2)))))
   
   
   ("..." (token-VARARG))
   (".." (token-CONCAT))
   (";" (token-\;))
   (":" (token-:))
   ("," (token-\,))
   ("." (token-\.))
   ; Delete comments
   ("--[[" (lua-mult-line-comment-lexer input-port))
   ("--" (lua-sing-line-comment-lexer input-port))
   
   ("-" (token--))
   ("+" (token-+))
   ("*" (token-*))
   ("/" (token-/))
   ("^" (token-^))
   ("%" (token-%))
   ("<" (token-LT))
   ("<=" (token-LE))
   (">" (token-GT))
   (">=" (token-GE))
   ("==" (token-EQ))
   ("~=" (token-NOTEQ))
   ("and" (token-AND))
   ("or" (token-OR))
   ("not" (token-NOT))
   ("#" (token-\#))
   ("=" (token-=))
   ("[" (token-\[))
   ("]" (token-\]))
   ("(" (token-\())
   (")" (token-\)))
   ("{" (token-\{))
   ("}" (token-\}))
   ("break" (token-BREAK))
   ("return" (token-RETURN))
   ("if" (token-IF))
   ("then" (token-THEN))
   ("else" (token-ELSE))
   ("elseif" (token-ELSEIF))
   ("do" (token-DO))
   ("end" (token-END))
   ("while" (token-WHILE))
   ("for" (token-FOR))
   ("repeat" (token-REPEAT))
   ("until" (token-UNTIL))
   ("in" (token-IN))
   ("local" (token-LOCAL))
   ("function" (token-FUNCTION))
   
   ; identifiers
   (id (token-NAME (string->symbol (string-replace lexeme "_" "~"))))
   
   ; skip whitespaces
   (whitespace (lua-lexer input-port))
   
   ((eof) (token-EOF))))

; Another lexer, specific for skipping comments' content
; Single-line comments
(define lua-sing-line-comment-lexer 
  (lexer
   ["\n" (lua-lexer input-port)]
   
   [any-char (lua-sing-line-comment-lexer input-port)]))

; Multiple-lines comments
(define lua-mult-line-comment-lexer 
  (lexer
   ["]]--" (lua-lexer input-port)]
   
   [any-char
    (lua-mult-line-comment-lexer input-port)]))


; Racket's strings "prints using doublequotes, where doublequote and backslash
; characters within the string are escaped with backslashes"
; (from https://docs.racket-lang.org/guide/strings.html). Which is not the case
; with Lua's string. We implement this procedure to eliminate redundant escape
; characters.
(define (clean string)
  (define aux string)
  
  ; Unescape backslashs from \n
  (set! aux (string-replace aux "\\n" "\n"))
  (set! aux (string-replace aux "\\\n" "\n"))
  ; Unescape backslashs from \0
  (set! aux (string-replace aux "\\0" "\0"))
  (set! aux (string-replace aux "\\\0" "\0"))
    
  (if (equal? aux string)
      string ; Nothing left to be cleaned
      (clean aux))
  )

; Translate to Racket's hexadecimal numbers' notation
(define (2-racket-hex number)
  (string-replace number (regexp "0x|0X") "#x"))


(provide lua-lexer)

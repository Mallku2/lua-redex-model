#lang racket
(require parser-tools/lex
         rackunit
         rackunit/text-ui
         "./lexer.rkt")

(define (lex-this lexer input) (lambda () (lexer input)))

; Test
(define-test-suite lexer-test-suite
  (check-equal? (lua-lexer (open-input-string "nil"))
                'NIL)

  (check-equal? (lua-lexer (open-input-string "false"))
                'FALSE)

  (check-equal? (lua-lexer (open-input-string "true"))
                'TRUE)

  ; numbers
  (check-equal? (lua-lexer (open-input-string "1"))
                (token-NUMBER 1))

  (check-equal? (lua-lexer (open-input-string "1.3"))
                (token-NUMBER 1.3))

  (check-equal? (lua-lexer (open-input-string ".1"))
                (token-NUMBER 0.1))

  (check-equal? (lua-lexer (open-input-string "1."))
                (token-NUMBER 1.0))

  (check-equal? (lua-lexer (open-input-string "314.16e-2"))
                (token-NUMBER 3.1416))

  (check-equal? (lua-lexer (open-input-string "314.16e+2"))
                (token-NUMBER 31416.0))

  (check-equal? (lua-lexer (open-input-string "314.16E-2"))
                (token-NUMBER 3.1416))

  (check-equal? (lua-lexer (open-input-string "314.16E+2"))
                (token-NUMBER 31416.0))

  (check-equal? (lua-lexer (open-input-string "0xff"))
                (token-NUMBER 255))

  (check-equal? (lua-lexer (open-input-string "0xff."))
                (token-NUMBER 255.0))

  (check-equal? (lua-lexer (open-input-string "0x.1"))
                (token-NUMBER 0.0625))

  (check-equal? (lua-lexer (open-input-string "0x0.1E"))
                (token-NUMBER 0.1171875))

  (check-equal? (lua-lexer (open-input-string "0xA23p-4"))
                (token-HEX-NMBR-BINPOT '(2595 -4)))
  
  (check-equal? (lua-lexer (open-input-string "0X1.921FB54442D18P+1"))
                (token-HEX-NMBR-BINPOT '(1.5707963267948966 1)))

  (check-equal? (lua-lexer (open-input-string "\"string\""))
                (token-STRING "string"))

  ; NOTE: single-quoted strings to double-quoted, because in Racket we don't
  ; have the first ones.
  (check-equal? (lua-lexer (open-input-string "'string'"))
                (token-STRING "string"))
  
  (check-equal? (lua-lexer (open-input-string "..."))
                'VARARG)

  (check-equal? (lua-lexer (open-input-string ".."))
                'CONCAT)

  (check-equal? (lua-lexer (open-input-string "."))
                (token-\.))

  (check-equal? (lua-lexer (open-input-string ";"))
                (token-\;))

  (check-equal? (lua-lexer (open-input-string ":"))
                (token-:))

  (check-equal? (lua-lexer (open-input-string "-"))
                (token--))

  (check-equal? (lua-lexer (open-input-string "+"))
                (token-\+))

  (check-equal? (lua-lexer (open-input-string "*"))
                (token-\*))

  (check-equal? (lua-lexer (open-input-string "/"))
                (token-/))

  (check-equal? (lua-lexer (open-input-string "^"))
                (token-^))

  (check-equal? (lua-lexer (open-input-string "%"))
                (token-%))

  (check-equal? (lua-lexer (open-input-string "<"))
                (token-LT))

  (check-equal? (lua-lexer (open-input-string "<="))
                (token-LE))

  (check-equal? (lua-lexer (open-input-string ">"))
                (token-GT))

  (check-equal? (lua-lexer (open-input-string ">="))
                (token-GE))

  (check-equal? (lua-lexer (open-input-string "=="))
                (token-EQ))

  (check-equal? (lua-lexer (open-input-string "~="))
                (token-NOTEQ))

  (check-equal? (lua-lexer (open-input-string "and"))
                (token-AND))

  (check-equal? (lua-lexer (open-input-string "or"))
                (token-OR))

  (check-equal? (lua-lexer (open-input-string "not"))
                (token-NOT))

  (check-equal? (lua-lexer (open-input-string "#"))
                (token-\#))

  (check-equal? (lua-lexer (open-input-string "="))
                (token-=))

  (check-equal? (lua-lexer (open-input-string "["))
                (token-\[))

  (check-equal? (lua-lexer (open-input-string "]"))
                (token-\]))

  (check-equal? (lua-lexer (open-input-string "("))
                (token-\())

  (check-equal? (lua-lexer (open-input-string ")"))
                (token-\)))

  (check-equal? (lua-lexer (open-input-string "{"))
                (token-\{))

  (check-equal? (lua-lexer (open-input-string "}"))
                (token-\}))

  (check-equal? (lua-lexer (open-input-string "break"))
                (token-BREAK))

  (check-equal? (lua-lexer (open-input-string "return"))
                (token-RETURN))

  (check-equal? (lua-lexer (open-input-string "if"))
                (token-IF))

  (check-equal? (lua-lexer (open-input-string "then"))
                (token-THEN))

  (check-equal? (lua-lexer (open-input-string "else"))
                (token-ELSE))

  (check-equal? (lua-lexer (open-input-string "elseif"))
                (token-ELSEIF))

  (check-equal? (lua-lexer (open-input-string "do"))
                (token-DO))

  (check-equal? (lua-lexer (open-input-string "end"))
                (token-END))

  (check-equal? (lua-lexer (open-input-string "while"))
                (token-WHILE))

  (check-equal? (lua-lexer (open-input-string "for"))
                (token-FOR))

  (check-equal? (lua-lexer (open-input-string "repeat"))
                (token-REPEAT))

  (check-equal? (lua-lexer (open-input-string "until"))
                (token-UNTIL))

  (check-equal? (lua-lexer (open-input-string "local"))
                (token-LOCAL))

  (check-equal? (lua-lexer (open-input-string "function"))
                (token-FUNCTION))

  (check-equal? (lua-lexer (open-input-string "\"string\""))
                (token-STRING "string"))

  (check-equal? (lua-lexer (open-input-string "x"))
                (token-NAME 'x))

  )

(provide lexer-test-suite)

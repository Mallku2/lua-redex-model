#lang racket
(require "./lexer.rkt"
         "./parser.rkt"
         "./phrases_constructors.rkt"
         rackunit
         rackunit/text-ui
         racket/match
         redex)

(provide (all-defined-out))


(define-test-suite parser-test-suite
  
  ;                                                                                  
  ;                                                                                  
  ;                                                                                  
  ;    ;;;;;    ;               ;                                       ;            
  ;   ;;    ;   ;               ;                                       ;            
  ;   ;       ;;;;;;    ;;;   ;;;;;;   ;;;;   ;;;;;;;  ;;;;   ; ;;;   ;;;;;;   ;;;;  
  ;   ;;        ;      ;   ;    ;     ;;  ;;  ;  ;  ; ;;  ;;  ;;   ;    ;     ;    ; 
  ;    ;;;;;    ;          ;    ;     ;    ;  ;  ;  ; ;    ;  ;    ;    ;     ;      
  ;        ;;   ;      ;;;;;    ;     ;;;;;;  ;  ;  ; ;;;;;;  ;    ;    ;      ;;;;  
  ;         ;   ;     ;    ;    ;     ;       ;  ;  ; ;       ;    ;    ;          ; 
  ;   ;    ;;   ;     ;   ;;    ;     ;;   ;  ;  ;  ; ;;   ;  ;    ;    ;     ;    ; 
  ;    ;;;;;     ;;;   ;;; ;     ;;;   ;;;;   ;  ;  ;  ;;;;   ;    ;     ;;;   ;;;;  
  ;                                                                                  
  ;                                                                                  
  ;                                                                                  
  
  (check-equal? (parse-this ";" #f (void))
                (term \;) )
  
  (check-equal? (parse-this "break"  #f (void))
                (term break) )
  
  ; return
  (check-equal? (parse-this "return 1" #f (void))
                (term (return (< 1.0 >))) )
  
  (check-equal? (parse-this "return 1;"  #f (void))
                (term (return (< 1.0 >))) )
  
  (check-equal? (parse-this "return"  #f (void))
                (term (return nil)) )
  
  (check-equal? (parse-this "return 1,2;"  #f (void))
                (term (return (< 1.0 2.0 >))))
  
  (check-equal? (parse-this "return function() end"  #f (void))
                (term (return (< (function $1 () \; end) >))))
  
  ;functioncall
  (check-equal? (parse-this "x ()"  #f (void))
                (term ((~ENV  \[ "x" \]) ())))
  
  (check-equal? (parse-this "x () ()"  #f (void))
                (term (((~ENV  \[ "x" \]) ()) ())))
  
  (check-equal? (parse-this "x (1, 2)"  #f (void))
                (term ((~ENV  \[ "x" \]) (1.0 2.0))))
  
  (check-equal? (parse-this "x {1}"  #f (void))
                (term ((~ENV  \[ "x" \]) ((\{ 1.0 \})))))
  
  (check-equal? (parse-this "(x) {1}" #f (void))
                (term ((\( (~ENV  \[ "x" \]) \)) ((\{ 1.0 \})))))
  
  (check-equal? (parse-this "x : method_name {1}" #f (void))
                (term ((~ENV  \[ "x" \]) : method~name ((\{ 1.0 \})))))
  
  (check-equal? (parse-this "y : method_name_1 {1} : method_name_2 {2}" #f (void))
                (term (((~ENV  \[ "y" \]) : method~name~1 ((\{ 1.0 \})))
                       : method~name~2 ((\{ 2.0 \})))))
  
  ; local var
  (check-equal? (parse-this "local a" #f (void))
                (term (local (a) = (nil) in \; end)))
  
  (check-equal? (parse-this "local a,b" #f (void))
                (term (local (a b) = (nil nil) in \; end)))
  
  (check-equal? (parse-this "local a a = 1" #f (void))
                (term (local (a) = (nil) in (\; ((a) = (1.0))) end)))
  
  ; assignment
  (check-equal? (parse-this "c = 1" #f (void))
                (term (((~ENV  \[ "c" \])) = (1.0))))
  
  (check-equal? (parse-this "c[1] = 2" #f (void))
                (term ((((~ENV  \[ "c" \]) \[ 1.0 \])) = (2.0))))
  
  ; do-end
  (check-equal? (parse-this "do end" #f (void))
                (term (do \; end)))
  
  (check-equal? (parse-this "do ; end" #f (void))
                (term (do \; end)))
  
  (check-equal? (parse-this "fact = false
                             do
                              local res = 1
                              local function fact (n)
                                    do end
                                    end
                              assert(fact(5) == 120)
                             end
                             assert(fact == false)" #f (void))
                (term ((((~ENV |[| "fact" |]|)) = (false))
                       (do (local (res) = (1.0)
                             in
                             (|;| (local (fact) = (nil)
                                    in
                                    (((fact) = ((function $1 (n) (do \; end) end)))
                                     ((~ENV |[| "assert" |]|) (((fact (5.0)) == 120.0))))
                                    end)
                                  )
                             end)
                         end)
                       ((~ENV |[| "assert" |]|) (((~ENV |[| "fact" |]|) == false))))))
  
  (check-equal? (parse-this "do local c = 1 ; c = 2 end ; c = 3" #f (void))
                (term ((do (local (c) = (1.0) in (\; \; ((c) = (2.0))) end) end)
                       \;
                       (((~ENV  \[ "c" \])) = (3.0)))))
  
  ; while
  (check-equal? (parse-this "while true do end" #f (void))
                (term (while true do \; end)))
  
  (check-equal? (parse-this "while true do do end end" #f (void))
                (term (while true do (do \; end) end)))
  
  (check-equal? (parse-this "while true do local d = 1 end d = 2" #f (void))
                (term ((while true do (local (d) = (1.0) in \; end) end)
                       (((~ENV  \[ "d" \])) = (2.0)))))
  
  ; numeric for
  (check-equal? (parse-this "for var = 1 , 10 do var = 1 end" #f (void))
                (term (do
                          (local ($var $limit $step) = (($builtIn tonumber (1.0 10.0))
                                                        ($builtIn tonumber (10.0 10.0))
                                                        ($builtIn tonumber (1.0 10.0)))
                            in
                            ((if (not (($var and $limit) and $step))
                                 then
                                 ($builtIn error ())
                                 else
                                 \;
                                 end)
                             (while ((($step > 0.0) and ($var <= $limit)) or (($step <= 0.0) and ($var >= $limit)))
                                    do
                                    (local (var) = ($var)
                                      in
                                      (((var) = (1.0))
                                       (($var) = (($var + $step))))
                                      end)
                                    end))
                            end)
                        end)))
  
  (check-equal? (parse-this "for var = 1 , 10 , 1 do local a a = 1 end a = 2; var = 3" #f (void))
                (term ((do
                           (local ($var $limit $step) = (($builtIn tonumber (1.0 10.0))
                                                         ($builtIn tonumber (10.0 10.0))
                                                         ($builtIn tonumber (1.0 10.0)))
                             in
                             ((if (not (($var and $limit) and $step))
                                  then
                                  ($builtIn error ())
                                  else
                                  \;
                                  end)
                              (while ((($step > 0.0) and ($var <= $limit)) or (($step <= 0.0) and ($var >= $limit)))
                                     do
                                     (local (var) = ($var)
                                       in
                                       (local (a) = (nil)
                                         in
                                         (\;
                                          ((a) = (1.0))
                                          (($var) = (($var + $step))))
                                         end)
                                       end)
                                     end))
                             end)
                         end)
                       (((~ENV \[ "a" \])) = (2.0))
                       \;
                       (((~ENV \[ "var" \])) = (3.0)))))
  
  ; generic for
  (check-equal? (parse-this "for k,v in a do ; end" #f (void))
                (term (do (local
                            ($f $s $var) = ((~ENV |[| "a" |]|))
                            in
                            (while true do
                                   (local (k v) = (($f ($s $var)))
                                     in
                                     ((if (k == nil) then
                                          break
                                       else
                                          |;|
                                       end)
                                      (($var) = (k))
                                      |;|)
                                     end)
                                 end)
                            end)
                        end)))

  (check-equal? (parse-this "for k,v in a do local b end b = 1" #f (void))
                (term ((do (local
                            ($f $s $var) = ((~ENV |[| "a" |]|))
                            in
                            (while true do
                                   (local (k v) = (($f ($s $var)))
                                     in
                                     ((if (k == nil) then
                                          break
                                       else
                                          |;|
                                       end)
                                      (($var) = (k))
                                      (local (b) = (nil) in \; end))
                                     end)
                                 end)
                            end)
                        end)
                       (((~ENV \[ "b" \])) = (1.0)))))

  (check-equal? (parse-this "for k,v in a do k = 1 end b = 1" #f (void))
                (term ((do (local
                            ($f $s $var) = ((~ENV |[| "a" |]|))
                            in
                            (while true do
                                   (local (k v) = (($f ($s $var)))
                                     in
                                     ((if (k == nil) then
                                          break
                                       else
                                          |;|
                                       end)
                                      (($var) = (k))
                                      ((k) = (1.0)))
                                     end)
                                 end)
                            end)
                        end)
                       (((~ENV \[ "b" \])) = (1.0)))))
  ; conditional
  (check-equal? (parse-this "if true then ; end" #f (void))
                (term (if true then \; else \; end)))
  
  (check-equal? (parse-this "if true then ; else do end end" #f (void))
                (term (if true then \; else (do \; end) end)))
  
  (check-equal? (parse-this "if true then ; elseif false then ;; else do end end" #f (void))
                (term (if true then \; else (if false then (\; \;) else (do \; end) end) end)))
  
  (check-equal? (parse-this "if true then ; elseif false then ;; elseif false then ;;; else do end end" #f (void))
                (term (if true then \; else (if false then (\; \;) else (if false then (\; \; \;) else (do \; end) end) end) end)))
  
  (check-equal? (parse-this "if true then local e = 1 end e = 2" #f (void))
                (term ((if true then (local (e) = (1.0) in \; end) else \; end)
                       (((~ENV  \[ "e" \])) = (2.0)))))
  
  (check-equal? (parse-this "if true then local e = 1
                             elseif false then e = 2 local f = 1
                             else f = 2 local g = 3 end
                             g = 4" #f (void))
                (term ((if true then
                           (local (e) = (1.0) in |;| end)
                           else
                           (if false then
                               ((((~ENV  |[| "e" |]|)) = (2.0))
                                (local (f) = (1.0) in |;| end))
                               else ((((~ENV  |[| "f" |]|)) = (2.0))
                                     (local (g) = (3.0) in |;| end))
                               end)
                           end)
                       (((~ENV  |[| "g" |]|)) = (4.0)))))
  
  (check-equal? (parse-this "local b

                             if 1 then ;
                             elseif 2 then ;
                             else if 3 then ; end
                             end

                             b = 1" #f (void))
                (term (local (b) = (nil)
                        in
                        (|;| (if 1.0 then |;|
                                 else
                                 (if 2.0 then
                                     |;|
                                     else
                                     (if 3.0 then
                                         |;|
                                         else |;|
                                         end)
                                     end)
                                 end)
                             ((b) = (1.0)))
                        end)))
  
  ; Global function def
  (check-equal? (parse-this "function global() do end end" #f (void))
                (term (((~ENV  \[ "global" \])) = ((function $1 () (do \; end) end)))))
  
  (check-equal? (parse-this "function global() local l l = 1 end l = 2" #f (void))
                (term ((((~ENV  \[ "global" \])) = ((function $1 () (local (l) = (nil) in (\; ((l) = (1.0))) end) end)))
                       (((~ENV  \[ "l" \])) = (2.0)))))
  
  (check-equal? (parse-this "function clase.metodo() do end end" #f (void))
                (term ((((~ENV  \[ "clase" \]) \[ "metodo" \])) = ((function $1 () (do \; end) end)))))
  
  (check-equal? (parse-this "function clase.objeto_miembro.metodo() do end end" #f (void))
                (term (((((~ENV  \[ "clase" \]) \[ "objeto~miembro" \]) \[ "metodo" \])) = ((function $1 () (do \; end) end)))))
  
  (check-equal? (parse-this "function clase.objeto_miembro:metodo() self = 1 end" #f (void))
                (term (((((~ENV  \[ "clase" \]) \[ "objeto~miembro" \]) \[ "metodo" \])) = ((function $1 (self) ((self) = (1.0)) end)))))
  
  ; Local function def
  (check-equal? (parse-this "local function fa() fa () local fb = 1 end fb = 2" #f (void))
                (term (local (fa) = (nil) in (((fa) = ((function $1 () ((fa ())
                                                                        (local (fb) = (1.0) in \; end)) end)))
                                              (((~ENV  \[ "fb" \])) = (2.0))) end)))
  
  (check-equal? (parse-this "local function fact (n) if n==0 then return res else return n*fact(n-1) end end
                             assert(fact(5) == 120)" #f (void))
                (term (local (fact) = (nil)
                        in
                        (((fact) = ((function $1 (n)
                                              (if (n == 0.0) then
                                                  (return (< (~ENV |[| "res" |]|) >))
                                                  else
                                                  (return (< (n * (fact ((n - 1.0)))) >))
                                                  end)
                                              end)))
                         ((~ENV |[| "assert" |]|) (((fact (5.0)) == 120.0))))
                        end)))
  
  ;                                                                                          
  ;                                                             ;                            
  ;                                                                                          
  ;   ;;;;;;;                                                                                
  ;   ;                                                                                      
  ;   ;       ;;  ;;  ;;;;;    ;;;;    ;;;;    ;;;;    ;;;;   ;;;      ;;;;   ; ;;;    ;;;;  
  ;   ;        ;  ;   ;;  ;;   ;;  ;  ;;  ;;  ;    ;  ;    ;    ;     ;;  ;;  ;;   ;  ;    ; 
  ;   ;;;;;;;   ;;    ;    ;   ;      ;    ;  ;       ;         ;     ;    ;  ;    ;  ;      
  ;   ;         ;;    ;    ;   ;      ;;;;;;   ;;;;    ;;;;     ;     ;    ;  ;    ;   ;;;;  
  ;   ;         ;;    ;    ;   ;      ;            ;       ;    ;     ;    ;  ;    ;       ; 
  ;   ;        ;  ;   ;;  ;;   ;      ;;   ;  ;    ;  ;    ;    ;     ;;  ;;  ;    ;  ;    ; 
  ;   ;;;;;;; ;;  ;;  ;;;;;    ;       ;;;;    ;;;;    ;;;;   ;;;;;    ;;;;   ;    ;   ;;;;  
  ;                   ;                                                                      
  ;                   ;                                                                      
  ;                   ;                                                                      
  (check-equal? (parse-this "h = ..." #f (void))
                (term (((~ENV  |[| "h" |]|)) = (<<<))))
  
  (check-equal? (parse-this "h = \"string\"" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ("string"))))
  
  (check-equal? (parse-this "h,str = \"string1\", \"string2\"" #f (void))
                (term (((~ENV  |[| "h" |]|) (~ENV  |[| "str" |]|)) = ("string1" "string2"))))
  
  (check-equal? (parse-this "h = 'string'" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ("string"))))
  
  (check-equal? (parse-this "h = [[string]]" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ("string"))))

  (check-equal? (parse-this "f = load [[ ]]
                             f = [[ x[i] == select(i, ...) ]]" #f (void))
                (term ((((~ENV |[| "f" |]|)) = (((~ENV |[| "load" |]|) (" "))))
                       (((~ENV |[| "f" |]|)) = (" x[i] == select(i, ...) ")))))
  
  (check-equal? (parse-this "h = _i" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((~ENV  |[| "~i" |]|)))))
  
  (check-equal? (parse-this "h = 1.1" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (1.1))))

  (check-equal? (parse-this "h = 0XffP+1" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((255 * (2 ^ 1))))))
  
  (check-equal? (parse-this "h = nil" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (nil))))
  
  (check-equal? (parse-this "h = true" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (true))))
  
  (check-equal? (parse-this "h = false" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (false))))
  
  (check-equal? (parse-this "h = 1.1 + 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 + 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 - 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 - 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 * 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 * 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 / 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 / 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 % 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 % 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 ^ 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 ^ 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 < 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 < 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 <= 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 <= 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 > 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 > 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 >= 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 >= 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 == 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 == 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 ~= 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((not (1.1 == 0.5))))))
  
  (check-equal? (parse-this "h = 1.1 .. 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 .. 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 and 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 and 0.5)))))
  
  (check-equal? (parse-this "h = 1.1 or 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((1.1 or 0.5)))))
  
  (check-equal? (parse-this "h = not 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((not 0.5)))))
  
  (check-equal? (parse-this "h = # 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\# 0.5)))))
  
  (check-equal? (parse-this "h = - 0.5" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((- 0.5)))))
  
  
  ; tableconstructor
  (check-equal? (parse-this "h = {}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ \})))))
  
  (check-equal? (parse-this "h = {1}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ 1.0 \})))))
  
  (check-equal? (parse-this "h = {[1] = 2}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ (\[ 1.0 \] = 2.0) \})))))
  
  (check-equal? (parse-this "h = {[ 1 ] = 2}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ (\[ 1.0 \] = 2.0) \})))))
  
  (check-equal? (parse-this "h = {a = 2}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ (\[ "a" \] = 2.0) \})))))
  
  (check-equal? (parse-this "h = {[ 1 ] = 2 , 3}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ (\[ 1.0 \] = 2.0) 3.0 \})))))
  
  (check-equal? (parse-this "h = {[ 1 ] = 2 , 3 ;}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ (\[ 1.0 \] = 2.0) 3.0 \})))))

  (check-equal? (parse-this "h = {_A = _G}" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\{ (\[ "_A" \] = (~ENV  |[| "~G" |]|)) \})))))
  
  ; parenthesized expressions
  (check-equal? (parse-this "h = (1)" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\( 1.0 \))))))
  
  (check-equal? (parse-this "h = ({[ 1 ] = 2 , 3 ;})" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((\( (\{ (\[ 1.0 \] = 2.0) 3.0 \}) \))))))
  
  ; var
  (check-equal? (parse-this "h = a" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((~ENV  |[| "a" |]|)))))
  ; Global vars
  (check-equal? (parse-this "h = z" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((~ENV  |[| "z" |]|)))))
  
  (check-equal? (parse-this "h = a [ 1 ]" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (((~ENV  |[| "a" |]|) \[ 1.0 \])))))
  
  (check-equal? (parse-this "h = z [ 1 ]" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (((~ENV  |[| "z" |]|) \[ 1.0 \])))))
  
  (check-equal? (parse-this "h = z . name" #f (void))
                (term (((~ENV  |[| "h" |]|)) = (((~ENV  |[| "z" |]|) \[ "name" \])))))
  
  ; functiondef
  (check-equal? (parse-this "h = function () do end end" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((function $1 () (do |;| end) end)))))
  
  (check-equal? (parse-this "h = function (x) do end end" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((function $1 (x) (do |;| end) end)))))
  
  (check-equal? (parse-this "h = function (x , ...) do end end" #f (void))
                (term (((~ENV  |[| "h" |]|)) = ((function $1 (x <<<) (do |;| end) end)))))
  
  (check-equal? (parse-this "h = function (x) local u u = 1 end u = 2" #f (void))
                (term ((((~ENV  |[| "h" |]|)) = ((function $1 (x) (local (u) = (nil) in (\; ((u) = (1.0))) end) end)))
                       (((~ENV  |[| "u" |]|)) = (2.0)))))
  
  
  ;                                  
  ;             ;                    
  ;                                  
  ;   ;;   ;;                        
  ;   ;;   ;;                        
  ;   ; ; ; ; ;;;      ;;;;     ;;;  
  ;   ; ; ; ;   ;     ;    ;   ;   ; 
  ;   ; ; ; ;   ;     ;       ;      
  ;   ;  ;  ;   ;      ;;;;   ;      
  ;   ;     ;   ;          ;  ;      
  ;   ;     ;   ;     ;    ;   ;   ; 
  ;   ;     ; ;;;;;    ;;;;     ;;;  
  ;                                  
  ;                                  
  ;                                  
  (check-equal? (parse-this "--This is a single line comment
                            a = 1" #f (void))
                (term (((~ENV  |[| "a" |]|)) = (1.0))))
  
  (check-equal? (parse-this "--[[This is a multiple-lines
                                 comment]]--
                            a = 1" #f (void))
                (term (((~ENV  |[| "a" |]|)) = (1.0))))
  
  
  )

(provide parser-test-suite)

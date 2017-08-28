#lang racket
; Statements that don't interact with some store.

(require redex
         "../grammar.rkt")

(define stats
  (reduction-relation
   core-lang
   #:domain s
   ; If statement
   [--> (if v then s_1 else s_2 end)
      s_1
      IfTrue
      
      (side-condition (and (not (redex-match core-lang
                                             nil
                                             (term v)))
                           (not (redex-match core-lang
                                             false
                                             (term v)))))]
   
   [--> (if v then s_1 else s_2 end)
      s_2
      IfFalse
      
      (side-condition (or (redex-match core-lang
                                             nil
                                             (term v))
                          
                          (redex-match core-lang
                                             false
                                             (term v))))]

   ; While statement
   [--> (while e do s end)
        ((($nextItWhile e do s end))Break)
      SignpostWhile]
   
   [--> ($nextItWhile e do s end)
      (if e then (s ($nextItWhile e do s end)) else \; end)
      While]

   
   ; Concatenation of statements
   ; This added rule has to do with the concrete grammar used
   ; in this mechanization.
   [--> (\; s)
        s
        ConcatBehavior]

   [--> (\; s_1 s_2 s_3 ...)
        (s_1 s_2 s_3 ...)
        ConcatBehavior2]

   ; Do ... End block
   [--> (do \; end)
        \;
        DoEnd]

   ; List length-equating rules for assignment statements
   [--> ((evar ...) = vlist_1)
        ((evar ...) = vlist_2)
        AssignDiscardRvalues
        
        (where Number_1 ,(length (term (evar ...))))
        (where Number_2 ,(length (term vlist_1)))
        
        (side-condition (< (term Number_1) (term Number_2)))
        
        (where vlist_2 ,(take (term vlist_1) (term Number_1)))
        ]
   
   [--> ((evar ...) = vlist_1)
        ((evar ...) = vlist_2)
        AssignCompleteRvalues
        
        (where Number_1 ,(length (term (evar ...))))
        (where Number_2 ,(length (term vlist_1)))
        
        (side-condition (> (term Number_1) 
                           (term Number_2)))

        (where vlist_2 ,(append (term vlist_1)
                                (make-list (- (term Number_1) (term Number_2))
                                           (term nil))))
        ]
   
   [--> ((evar_1 evar_2 ... evar_3) = (v_1 v_2 ... v_3))
        (((evar_3) = (v_3)) ((evar_1 evar_2 ...) = (v_1 v_2 ...)))
        AssignSplit
        
        (side-condition (= (length (term (evar_1 evar_2 ... evar_3)))
                           (length (term (v_1 v_2 ... v_3)))))]
   
   [--> (local (Name ...) = vlist_1 in s end)
        (local (Name ...) = any in s end)
        LocalDiscardRvalues
        
        (where Number_1 ,(length (term (Name ...))))
        (where Number_2 ,(length (term vlist_1)))
        
        (side-condition (< (term Number_1) (term Number_2)))
        
        (where any ,(take (term vlist_1) (term Number_1)))]
   
   [--> (local (Name ...) = vlist_1 in s end)
        (local (Name ...) = vlist_2 in s end)
        LocalCompleteRvalues

        (where Number_1 ,(length (term (Name ...))))
        (where Number_2 ,(length (term vlist_1)))
        
        (side-condition (> (term Number_1) (term Number_2)))
        
        (where vlist_2 ,(append (term vlist_1) 
                                (make-list (- (term Number_1) (term Number_2)) 
                                           (term nil))))
        ]

   ; Break

   [--> (((in-hole Elf break))Break)
        \;
        Break]

   [--> ((\;)Break)
        \;
        FinalizationWhile]
   ))

(provide stats)

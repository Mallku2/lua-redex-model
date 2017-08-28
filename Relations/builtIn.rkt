#lang racket
(require redex
         "../grammar.rkt"
         "../Meta-functions/delta.rkt")

; Interface with the delta interpretation function

(define built-in
  (reduction-relation
   core-lang
   #:domain (θ : any)

   [--> (θ : ($builtIn Name (v ...)))
        (θ : (< (δ (Name v ...)) >))

        (side-condition (member (term Name)
                                (term (; basic functions
                                       assert
                                       error
                                       pcall
                                       print
                                       rawequal
                                       select
                                       tonumber
                                       ; math
                                       math.abs
                                       math.acos
                                       math.asin
                                       math.atan
                                       math.ceil
                                       math.cos
                                       math.cosh
                                       math.deg
                                       math.exp
                                       math.floor
                                       math.fmod
                                       math.log
                                       math.max
                                       math.modf
                                       math.rad
                                       math.sin
                                       math.sinh
                                       math.sqrt
                                       math.tan
                                       math.tanh
                                       ; string
                                       string.len
                                       string.rep
                                       string.reverse
                                       string.sub
                                       ; table
                                       table.pack
                                       ))))

        E-BuiltIn1]

   [--> (θ : ($builtIn Name (v ...)))
        (θ : (< (δ (Name v ... θ)) >))

        (side-condition (member (term Name)
                                (term (; basic functions
                                       ipairs
                                       next
                                       pairs
                                       load
                                       loadfile
                                       getmetatable
                                       tostring
                                       type
                                       rawget
                                       rawlen
                                       ; package
                                       require
                                       ; string
                                       string.dump
                                       ; table
                                       table.concat
                                       table.unpack))))

        E-BuiltIn3]

   [--> (θ_1 : ($builtIn Name (v ...)))
        (θ_2 : (< any >))
        
        E-BuiltIn5

        (side-condition (member (term Name)
                                (term (; basic functions
                                       setmetatable
                                       rawset))))
        
        (where (θ_2 any) (δ (Name v ... θ_1)))]
  ))

(provide built-in)

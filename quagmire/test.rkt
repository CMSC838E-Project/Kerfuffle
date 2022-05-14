#lang typed/racket

(: f (-> Integer String (Listof (Union Integer String)) Integer))

(define (f x y z) x)

(f 3 "good" (cons 4 (cons "str" (cons #f '()))))
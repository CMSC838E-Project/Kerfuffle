#lang typed/racket

(: f (-> (Listof (Union Integer String)) String))

(define (f x) "abc")

(f (cons 1 (cons #f '())))
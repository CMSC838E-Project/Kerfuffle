#lang racket

(struct Apple (x))
(: f (-> Struct Integer))
(define (f x) (match x [(Apple x) x]))
(f 1)
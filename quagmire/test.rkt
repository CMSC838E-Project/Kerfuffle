#lang racket

(: adder (-> (-> Integer Integer) (-> Integer Integer)))
(define (adder f) (λ ([x : Integer]) (+ (f x) 1)))

(adder (λ ([x : String]) x))
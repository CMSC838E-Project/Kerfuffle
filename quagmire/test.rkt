#lang racket

(struct s1 (x))
(struct s2 (y))

(: f1 (-> Integer Integer))
(: f2 (-> s1 s1))

(define (f1 x) x)
(define (f2 x) x)

(define (func f) (f 3))

(func f2)


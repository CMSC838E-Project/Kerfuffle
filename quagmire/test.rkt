#lang racket

(struct s1 (x))
(struct s2 (y))

(: func (-> s1 s1))
(define (func a) a)

(func (s2 5))


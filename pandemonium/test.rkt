#lang racket

(: func (-> Integer String Integer String Integer))
(define (func a b c d) (+ a c))
(func 5 #\a 6 "bac")
#lang racket

(: func (-> Integer Char Integer String Integer))
(define (func a b c d) (+ a c))
(define (func2 b) (+ (string-length b) (func 3 #\a 2 "hi")))
(func2 "bac")
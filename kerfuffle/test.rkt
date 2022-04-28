#lang racket

(: f (-> (Union Integer (Union String Char)) String))

(define (f x) "abc")

(f #f)
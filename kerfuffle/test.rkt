#lang racket

(: f (-> (Listof String) String))
(define (f lst) (match lst [(cons h t) h]))
(f (cons 1 (cons "cd" (cons "ef" '()))))
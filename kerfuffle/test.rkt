#lang typed/racket

(: f (-> (Vectorof (Union Integer (Union String (Listof Integer)))) String))

(define (f x) "abc")
(let ([v (make-vector 3 1)])
  (begin
    (begin
      (vector-set! v 1 "a")
      (vector-set! v 2 (cons 1 (cons "a" '()))))
    (f v)))
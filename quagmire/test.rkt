#lang typed/racket

(ann-error (cons 3 (cons #f (cons 3 '()))) (Listof (Union 'Integer 'String)))

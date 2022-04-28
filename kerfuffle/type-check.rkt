#lang racket
(provide (all-defined-out))
(require "types.rkt" "compile-ops.rkt" a86/ast)

(define (assert-type-ok mask type)
  (λ (arg ok)
    (seq (Mov r9 arg)
         (And r9 mask)
         (Cmp r9 type)
         (Je ok))))

(define assert-integer-ok
  (assert-type-ok mask-int type-int))
(define assert-char-ok
  (assert-type-ok mask-char type-char))
(define assert-box-ok
  (assert-type-ok ptr-mask type-box))
(define assert-cons-ok
  (assert-type-ok ptr-mask type-cons))
(define assert-vector-ok
  (assert-type-ok ptr-mask type-vect))
(define assert-string-ok
  (assert-type-ok ptr-mask type-str))
(define (assert-bool-ok r ok)
  (seq  (Mov r8 r) 
        (Cmp r8 val-true)
        (Je ok)
        (Mov r8 r) 
        (Cmp r8 val-false)
        (Je ok)))
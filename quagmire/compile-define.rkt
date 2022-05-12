#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "fv.rkt"
         "utils.rkt"
         "compile-expr.rkt"
         a86/ast)

;; [Listof Defn] -> [Listof Id]
(define (define-ids ds ts)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (cons f (define-ids ds ts))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds ts)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d ts)
          (compile-defines ds ts))]))

;; Defn -> Asm
(define (compile-define d ts)
  (match d
    [(Defn f xs e)
     (compile-lambda-define (Lam f xs e) ts)]))

;; Defns -> Asm
;; Compile the closures for ds and push them on the stack
(define (compile-defines-values ds ts)
  (seq (alloc-defines ds 0)
       (init-defines ds (reverse (define-ids ds ts)) 8)
       (add-rbx-defines ds 0)))

;; Defns Int -> Asm
;; Allocate closures for ds at given offset, but don't write environment yet
(define (alloc-defines ds off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (Lea rax (symbol->label f))
            (Mov (Offset rbx off) rax)
            (Mov rax rbx)
            (Add rax off)
            (Or rax type-proc)
            (Push rax)
            (alloc-defines ds (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns CEnv Int -> Asm
;; Initialize the environment for each closure for ds at given offset
(define (init-defines ds c off)
  (match ds
    ['() (seq)]
    [(cons (Defn f xs e) ds)
     (let ((fvs (fv (Lam f xs e))))
       (seq (free-vars-to-heap fvs c off)
            (init-defines ds c (+ off (* 8 (add1 (length fvs)))))))]))

;; Defns Int -> Asm
;; Compute adjustment to rbx for allocation of all ds
(define (add-rbx-defines ds n)
  (match ds
    ['() (seq (Add rbx (* n 8)))]
    [(cons (Defn f xs e) ds)
     (add-rbx-defines ds (+ n (add1 (length (fv (Lam f xs e))))))]))

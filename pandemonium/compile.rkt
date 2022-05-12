#lang racket
(provide (all-defined-out))
(require "ast.rkt"
         "types.rkt"
         "lambdas.rkt"
         "fv.rkt"
         "utils.rkt"
         "compile-define.rkt"
         "compile-expr.rkt"
         "compile-literals.rkt"
         a86/ast)

;; Registers used
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Id]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ts ds e)
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer
           (init-symbol-table p)
           (compile-defines-values ds ts)

           (compile-e e (reverse (define-ids ds ts)) #t ts #f)

           (Add rsp (* 8 (length ds))) ;; pop function definitions
           (Ret)
           (compile-defines ds ts)
           (compile-lambda-defines (lambdas p) ts)

           (Label 'raise_error_type_align)
           pad-stack
           (Call 'raise_error_type)

           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error)
           (Data)
           (compile-literals p))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)
       (Extern 'raise_error_type)
       (Extern 'intern_symbol)
       (Extern 'symb_cmp)
       (Extern 'memcpy)))


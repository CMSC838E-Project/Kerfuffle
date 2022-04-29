#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" "type-check.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define rsi 'rsi) ; arg2

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ts ds e)  
     (prog (externs)
           (Global 'entry)
           (Label 'entry)
           (Mov rbx rdi) ; recv heap pointer

           (compile-e e '() #t ts #f)
           (Ret)
           (compile-defines ts ds)

           (Label 'raise_error_type_align)
           pad-stack
           (Call 'raise_error_type)

           (Label 'raise_error_align)
           pad-stack
           (Call 'raise_error))]))

(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)
       (Extern 'raise_error_type)
       (Extern 'writeln)))

;; [Listof Defn] -> Asm
(define (compile-defines ts ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define ts d)
          (compile-defines ts ds))]))

;; -------------- Changes Start --------------

(define (raise-error-type expec actual)
  (seq  (Mov rdi expec)
        (Mov rsi actual)
        (Jmp 'raise_error_type_align)))

(define (debug x)
  (seq  pad-stack
        (Mov rdi (imm->bits x))
        (Call 'writeln)
        unpad-stack
        (Mov rax val-void)))

;; Defn -> Asm
(define (compile-define ts d)
  (match d
    [(Defn f xs e)
     (let ([ts* (lookup-type f ts)])
      (seq  (Label (symbol->label f))
            (compile-e e (reverse xs) (not ts*) ts ts*)
            (Add rsp (* 8 (length xs))) ; pop args

            ; Figure out how to determine if this function is "untyped".
            ; This should go in compile-app
            (if ts*  (let  ([ok (gensym)])
                      (seq  (Push rax)
                            (type-check (last ts*) rax ok)
                            (Pop r8)
                            (compile-string (type->string (last ts*)))
                            (raise-error-type rax r8)
                            (Label ok)
                            (Pop rax)))
                    (seq))

            (Ret))
            
        )]))

(define (type-check-param ts xs)
  (match (list ts xs)
    [(list '() '())                         (seq)]
    [(list (cons t ts) (cons _ xs))         (let ([ok (gensym)]
                                                  [mem (Offset rsp (* 8 (length xs)))]) 
                                              (seq  (type-check t mem ok)
                                                    (compile-string (type->string t))
                                                    (raise-error-type rax mem)
                                                    (Label ok)
                                                    (type-check-param ts xs)))]))

(define (type-check t mem ok)
  
  (seq  (match t
          [(TInt)               (seq  (assert-integer-ok mem ok))]
          [(TChar)              (seq  (assert-char-ok mem ok))]
          [(TStr)               (seq  (assert-string-ok mem ok))]
          [(TBool)              (seq  (assert-bool-ok mem ok))]
          [(TUnion t1 t2)       (seq  (type-check t1 mem ok)
                                      (type-check t2 mem ok))]
          [(TAny)               (seq  (Jmp ok))]
          [(TList t)      (let ([loop (gensym 'lst_loop)]
                                [next-element (gensym 'lst_next)]
                                [cont (gensym 'lst_cont)]
                                [end (gensym 'lst_end)])
                            (seq (Mov rax mem)
                                 (Jmp loop)
                                 (Label next-element)
                                 (Pop rax)
                                 (Mov rax (Offset rax 0))
                                 (Label loop)
                                 (assert-empty-ok rax ok)
                                 (assert-cons-ok rax cont)
                                 (Jmp end)
                                 (Label cont)
                                 (Xor rax type-cons)
                                 (Push rax)
                                 (Mov rax (Offset rax 8))
                                 (type-check t rax next-element)
                                 (Pop rax)
                                 (Label end)))]
          [(TVec t)        (let ([end (gensym 'vec_end)]
                                 [cont (gensym 'vec_cont)]
                                 [loop (gensym 'vec_loop)]
                                 [next-element (gensym 'vec_next)])
                             (seq (assert-vector-ok mem cont)
                                  (Jmp end)
                                  (Label cont)
                                  (Mov rax mem)
                                  (Xor rax type-vect)
                                  (Cmp rax 0)
                                  (Je ok)
                                  (Mov r8 (Offset rax 0))
                                  (Jmp loop)
                                  (Label next-element)
                                  (Pop rax)
                                  (Pop r8)
                                  (Label loop)
                                  (Cmp r8 0)
                                  (Je ok)
                                  (Sub r8 1)
                                  (Push r8)
                                  (Add rax 8)
                                  (Push rax)
                                  (Mov rax (Offset rax 0))
                                  (type-check t rax next-element)
                                  (Pop rax)
                                  (Pop r8)
                                  (Label end)))])
          ))

;; -------------- Changes End --------------

;; Expr CEnv Bool -> Asm
(define (compile-e e c t? ts typed?)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]
    [(Prim1 p e)        (compile-prim1 p e c ts typed?)]
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c ts typed?)]
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c ts typed?)]
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c t? ts typed?)]
    [(Begin e1 e2)      (compile-begin e1 e2 c t? ts typed?)]
    [(Let x e1 e2)      (compile-let x e1 e2 c t? ts typed?)]
    [(App f es)         (compile-app f es c t? ts typed?)]
    [(Match e ps es)    (compile-match e ps es c t? ts typed?)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c ts typed?)
  (seq (compile-e e c #f ts typed?)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c ts typed?)
  (seq (compile-e e1 c #f ts typed?)
       (Push rax)
       (compile-e e2 (cons #f c) #f ts typed?)
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c ts typed?)
  (seq (compile-e e1 c #f ts typed?)
       (Push rax)
       (compile-e e2 (cons #f c) #f ts typed?)
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)) #f ts typed?)
       (compile-op3 p)))

;; Expr Expr Expr CEnv Bool -> Asm
(define (compile-if e1 e2 e3 c t? ts typed?)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c #f ts typed?)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c t? ts typed?)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c t? ts typed?)
         (Label l2))))

;; Expr Expr CEnv Bool -> Asm
(define (compile-begin e1 e2 c t? ts typed?)
  (seq (compile-e e1 c #f ts typed?)
       (compile-e e2 c t? ts typed?)))

;; Id Expr Expr CEnv Bool -> Asm
(define (compile-let x e1 e2 c t? ts typed?)
  (seq (compile-e e1 c #f ts typed?)
       (Push rax)
       (compile-e e2 (cons x c) t? ts typed?)
       (Add rsp 8)))

;; Id [Listof Expr] CEnv Bool -> Asm
(define (compile-app f es c t? ts typed?)
  (if t?
      (compile-app-tail f es c ts typed?)
      (compile-app-nontail f es c ts typed?)))

;; Id [Listof Expr] CEnv -> Asm
(define (compile-app-tail f es c ts typed?)
  (seq (compile-es es c ts typed?)
       (move-args (length es) (length c))
       (Add rsp (* 8 (length c)))
       (if (not typed?)
           (let ([ts (lookup-type f ts)])
             (if ts (type-check-param (first ts) es)
                    (seq)))
           (seq))
       (Jmp (symbol->label f))))

;; Integer Integer -> Asm
(define (move-args i off)
  (cond [(zero? off) (seq)]
        [(zero? i)   (seq)]
        [else
         (seq (Mov r8 (Offset rsp (* 8 (sub1 i))))
              (Mov (Offset rsp (* 8 (+ off (sub1 i)))) r8)
              (move-args (sub1 i) off))]))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app-nontail f es c ts typed?)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c) ts typed?)
         (if (not typed?)
             (let ([ts (lookup-type f ts)])
               (if ts (type-check-param (first ts) es)
                      (seq)))
             (seq))
         (Jmp (symbol->label f))
         (Label r))))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c ts typed?)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c #f ts typed?)
          (Push rax)
          (compile-es es (cons #f c) ts typed?))]))

;; Expr [Listof Pat] [Listof Expr] CEnv Bool -> Asm
(define (compile-match e ps es c t? ts typed?)
  (let ((done (gensym)))  
    (seq (compile-e e c #f ts typed?)
         (Push rax) ; save away to be restored by each clause
         (compile-match-clauses ps es (cons #f c) done t?)
         (Jmp 'raise_error_align)
         (Label done)
         (Add rsp 8)))) ; pop the saved value being matched

;; [Listof Pat] [Listof Expr] CEnv Symbol Bool -> Asm
(define (compile-match-clauses ps es c done t? typed?)
  (match* (ps es)
    [('() '()) (seq)]
    [((cons p ps) (cons e es))
     (seq (compile-match-clause p e c done t? typed?)
          (compile-match-clauses ps es c done t? typed?))]))

;; Pat Expr CEnv Symbol Bool -> Asm
(define (compile-match-clause p e c done t? typed?)
  (let ((next (gensym)))
    (match (compile-pattern p '() next)
      [(list i f cm)
       (seq (Mov rax (Offset rsp 0)) ; restore value being matched
            i
            (compile-e e (append cm c) t? typed?)
            (Add rsp (* 8 (length cm)))
            (Jmp done)
            f
            (Label next))])))

;; Pat CEnv Symbol -> (list Asm Asm CEnv)
(define (compile-pattern p cm next)
  (match p
    [(PWild)
     (list (seq) (seq) cm)]
    [(PVar x)
     (list (seq (Push rax))
           (seq)
           (cons x cm))]
    [(PLit l)
     (let ((fail (gensym)))
       (list (seq (Cmp rax (imm->bits l))
                  (Jne fail))
             (seq (Label fail)
                  (Add rsp (* 8 (length cm)))
                  (Jmp next))
             cm))]
    [(PAnd p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (list
            (seq (Push rax)
                 i1
                 (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                 i2)
            (seq f1 f2)
            cm2)])])]
    [(PBox p)
     (match (compile-pattern p cm next)
       [(list i1 f1 cm1)
        (let ((fail (gensym)))
          (list
           (seq (Mov r8 rax)
                (And r8 ptr-mask)
                (Cmp r8 type-box)
                (Jne fail)
                (Xor rax type-box)
                (Mov rax (Offset rax 0))
                i1)
           (seq f1
                (Label fail)
                (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                (Jmp next))
           cm1))])]
    [(PCons p1 p2)
     (match (compile-pattern p1 (cons #f cm) next)
       [(list i1 f1 cm1)
        (match (compile-pattern p2 cm1 next)
          [(list i2 f2 cm2)
           (let ((fail (gensym)))
             (list
              (seq (Mov r8 rax)
                   (And r8 ptr-mask)
                   (Cmp r8 type-cons)
                   (Jne fail)                   
                   (Xor rax type-cons)
                   (Mov r8 (Offset rax 0))
                   (Push r8)                ; push cdr
                   (Mov rax (Offset rax 8)) ; mov rax car
                   i1
                   (Mov rax (Offset rsp (* 8 (- (sub1 (length cm1)) (length cm)))))
                   i2)
              (seq f1
                   f2
                   (Label fail)
                   (Add rsp (* 8 (length cm))) ; haven't pushed anything yet
                   (Jmp next))
              cm2))])])]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))

#lang racket
(provide interp interp-env)
(require "ast.rkt"
         "env.rkt"
         "interp-prims.rkt"
         "types.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; | '()
;; | (cons Value Value)
;; | (box Value)
;; | (vector Value ...)
;; | (string Char ...)
;; | (Value ... -> Answer)
;; | (StructVal Symbol (Vectorof Val))

;; type REnv = (Listof (List Id Value))
;; type Defns = (Listof Defn)

;; Prog -> Answer
(define (interp p)
  (match p
    [(Prog ts ds e)
     (interp-env e '() ds ts)]))

;; Expr Env Defns -> Answer
(define (interp-env e r ds ts)
  (match e
    [(Quote d)  d]
    [(Eof)    eof]
    [(Var x)  (interp-var x r ds ts)]
    [(Prim p es)
     (match (interp-env* es r ds ts)
       ['err 'err]
       [vs (interp-prim p vs)])]    
    [(If p e1 e2)
     (match (interp-env p r ds ts)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r ds ts)
            (interp-env e2 r ds ts))])]
    [(Begin e1 e2)
     (match (interp-env e1 r ds ts)
       ['err 'err]
       [_    (interp-env e2 r ds ts)])]
    [(Let x e1 e2)
     (match (interp-env e1 r ds ts)
       ['err 'err]
       [v (interp-env e2 (ext r x v) ds ts)])]
    [(Lam _ xs e)
     (λ vs
       ; check arity matches
       (if (= (length xs) (length vs))
           (interp-env e (append (zip xs vs) r) ds ts)
           'err))]
    [(App e es)
     (match (interp-env e r ds ts)
       ['err 'err]
       [f
        (match (interp-env* es r ds ts)
          ['err 'err]
          [vs
            (if (procedure? f)
              (match e
                ;; TODO --> for now we only perform type checking for non-lambda functions
                [(Var fname) (interp-app f fname vs ds ts)]
                [_ (apply f vs)])
              'err)])])]
    [(Match e ps es)
     (match (interp-env e r ds ts)
       ['err 'err]
       [v
        (interp-match v ps es r ds ts)])]))

(define (interp-app f fname vs ds ts)
  (let ([ts* (lookup-type fname ts)])
    (if ts*
      (if (check-parameters vs (first ts*))
        (let ([ret (apply f vs)])
          (if (check-type ret (last ts*))
              ret
              'err))
        'err)
      (apply f vs))))

;; Value [Listof Pat] [Listof Expr] Env Defns -> Answer
(define (interp-match v ps es r ds ts)
  (match* (ps es)
    [('() '()) 'err]
    [((cons p ps) (cons e es))
     (match (interp-match-pat p v r)
       [#f (interp-match v ps es r ds ts)]
       [r  (interp-env e r ds ts)])]))

;; Pat Value Env -> [Maybe Env]
(define (interp-match-pat p v r)
  (match p
    [(PWild) r]
    [(PVar x) (ext r x v)]
    [(PSymb s) (and (eq? s v) r)]
    [(PStr s) (and (string? v) (string=? s v) r)]
    [(PLit l) (and (eqv? l v) r)]
    [(PBox p)
     (match v
       [(box v)
        (interp-match-pat p v r)]
       [_ #f])]
    [(PCons p1 p2)
     (match v
       [(cons v1 v2)
        (match (interp-match-pat p1 v1 r)
          [#f #f]
          [r1 (interp-match-pat p2 v2 r1)])]
       [_ #f])]
    [(PAnd p1 p2)
     (match (interp-match-pat p1 v r)
       [#f #f]
       [r1 (interp-match-pat p2 v r1)])]
    [(PStruct t ps)
     (match v
       [(StructVal n vs)
        (and (eq? t n)
             (interp-match-pats ps (vector->list vs) r))]
       [_ #f])]))

;; [Listof Pat] [Listof Val] Env -> [Maybe Env]
(define (interp-match-pats ps vs r)
  (match ps
    ['() r]
    [(cons p ps)
     (match vs
       [(cons v vs)
        (match (interp-match-pat p v r)
          [#f #f]
          [r1 (interp-match-pats ps vs r1)])])]))

;; Id Env [Listof Defn] -> Answer
(define (interp-var x r ds ts)
  (match (lookup r x)
    ['err (match (defns-lookup ds x)
            [(Defn f xs e) (interp-env (Lam f xs e) '() ds ts)]
            [#f 'err])]
    [v v]))

;; (Listof Expr) REnv Defns -> (Listof Value) | 'err
(define (interp-env* es r ds ts)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r ds ts)
       ['err 'err]
       [v (match (interp-env* es r ds ts)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Defns Symbol -> [Maybe Defn]
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))

(define (check-parameters vs ts)
  (match* (vs ts)
    [('() '()) #t]
    [((cons v vs) (cons t ts))
     (and (check-type v t) (check-parameters vs ts))]))

(define (check-type v t)
  (match t
    [(TInt)     (integer? v)]
    [(TChar)    (char? v)]
    [(TStr)     (string? v)]
    [(TBool)    (boolean? v)]
    [(TAny)     #t]
    [(TList t)  (if (empty? v)
                    #t
                    (and (cons? v)
                         (check-type (car v) t)
                         (check-type (cdr v) (TList t))))]
    [(TVec t)       (and (vector? v)
                         (for/fold ([b #t])
                                   ([x (in-vector v)])
                           (and b (check-type x t))))]
    [(TUnion t1 t2) (or (check-type v t1)
                        (check-type v t2))]))
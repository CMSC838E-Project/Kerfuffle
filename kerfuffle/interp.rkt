#lang racket
(provide interp interp-env interp-match-pat interp-match)
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
    [(Int i)  i]
    [(Bool b) b]
    [(Char c) c]
    [(Eof)    eof]
    [(Empty)  '()]
    [(Var x)  (lookup r x)]
    [(Str s)  s]
    [(Prim0 'void) (void)]
    [(Prim0 'read-byte) (read-byte)]
    [(Prim0 'peek-byte) (peek-byte)]
    [(Prim1 p e)
     (match (interp-env e r ds ts)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r ds ts)
       ['err 'err]
       [v1 (match (interp-env e2 r ds ts)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    [(Prim3 p e1 e2 e3)
     (match (interp-env e1 r ds ts)
       ['err 'err]
       [v1 (match (interp-env e2 r ds ts)
             ['err 'err]
             [v2 (match (interp-env e3 r ds ts)
                   ['err 'err]
                   [v3 (interp-prim3 p v1 v2 v3)])])])]
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
    [(App f es)
     (match (interp-env* es r ds ts)
       ['err 'err]
       [vs
        (match (defns-lookup ds f)
          [(Defn f xs e)
           ; check arity matches
           (if (= (length xs) (length vs))
               (interp-app f e xs vs ds ts)
               'err)])])]
    [(Match e ps es)
     (match (interp-env e r ds ts)
       ['err 'err]
       [v
        (interp-match v ps es r ds ts)])]))

(define (interp-app f e xs vs ds ts)
  (let ([ts* (lookup-type f ts)])
    (if ts*
        (if (check-parameters vs (first ts*))
            (let ([ret (interp-env e (zip xs vs) ds ts)])
              (if (check-type ret (last ts*))
                  ret
                  'err))
            'err)
        (interp-env e (zip xs vs) ds ts))))

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
       [r1 (interp-match-pat p2 v r1)])]))

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

;; Defns Symbol -> Defn
(define (defns-lookup ds f)
  (findf (match-lambda [(Defn g _ _) (eq? f g)])
         ds))

(define (zip xs ys)
  (match* (xs ys)
    [('() '()) '()]
    [((cons x xs) (cons y ys))
     (cons (list x y)
           (zip xs ys))]))

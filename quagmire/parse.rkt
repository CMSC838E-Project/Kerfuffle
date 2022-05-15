#lang racket
(provide parse parse-define parse-e parse-struct get-lam-e type->runtime-struct)
(require "ast.rkt" "types.rkt")

;; -------------- Changes Start --------------

(define (parse s)
  (match (parse-get-lam-types s)
    [(Prog ts ds e)
     (Prog ts (insert-check-def ds ts) (insert-check e ts #f))]
  )
)

(define (insert-check-def ds ts)
  (match ds
    ['() '()]
    [(cons (Defn f xs e) ds)
     (let ([ts* (lookup-type f ts)])
      (cons (Defn f xs (insert-check e ts ts*)) (insert-check-def ds ts)))]))

(define (insert-check e ts typed?)
  (match e
    [(Prim p es)        (Prim p (map (lambda (x) (insert-check x ts typed?)) es))]
    [(If e1 e2 e3)      (If (insert-check e1 ts typed?) (insert-check e2 ts typed?) (insert-check e3 ts typed?))]
    [(Begin e1 e2)      (Begin (insert-check e1 ts typed?) (insert-check e2 ts typed?))]
    [(Let x e1 e2)      (Let x (insert-check e1 ts typed?) (insert-check e2 ts typed?))]
    [(App e es)         (insert-check-app e es ts typed?)]
    [(Lam f xs e)       (Lam f xs (insert-check e ts typed?))]
    [(Match e ps es)    (Match (insert-check e ts typed?) ps
                        (map (lambda (x) (insert-check x ts typed?)) es))]
    [(And-op es)        (And-op (map (lambda (x) (insert-check x ts typed?)) es))]
    [(Or-op es)         (Or-op (map (lambda (x) (insert-check x ts typed?)) es))]
    [r                  r]
  )
)

(define (insert-check-app e es ts typed?)
  (let ([ts* (if (not typed?)
                  (match (lookup-type (match e [(Var f) f]) ts)
                    [#f #f]
                    [(TFunc ins out) ins])
                  #f)]
        [es (map (lambda (x) (insert-check x ts typed?)) es)])
        (if ts*
          (App e
                (map (λ (e t)
                        (App (Var 'ann-error)
                            (list e
                                  (type->runtime-struct t))))
                      es
                      ts*))
          (App e es))))

(define (parse-get-lam-types s)
  (match (parse-base s)
    [(Prog ts ds e)
     (Prog (append (get-lam-def ds) (get-lam-e e) ts) ds e)]))

(define (get-lam-def ds)
  (match ds
    ['() '()]
    [(cons (Defn _ _ e) ds)
     (append (get-lam-e e) (get-lam-def ds))]))

(define (get-lam-e e)
  (match e
    [(Quote d)          '()]
    [(Eof)              '()]
    [(Var x)            '()]
    [(Prim p es)        (apply append (map get-lam-e es))]
    [(If e1 e2 e3)      (append (get-lam-e e1) (get-lam-e e2) (get-lam-e e3))]
    [(Begin e1 e2)      (append (get-lam-e e1) (get-lam-e e2))]
    [(Let x e1 e2)      (append (get-lam-e e1) (get-lam-e e2))]
    [(App e es)         (append (get-lam-e e) (apply append (map get-lam-e es)))]
    [(Lam f xs e)       (append (list (Type f (TFunc '((TAny)) (TAny)))) (get-lam-e e))]
    [(Match e ps es)    (append (get-lam-e e) (apply append (map get-lam-e es)))]
    [(And-op es)        (apply append (map get-lam-e es))]
    [(Or-op es)         (apply append (map get-lam-e es))]
  )
)

;; [Listof S-Expr] -> Prog
(define (parse-base s)
  (match s
    [(cons (and (cons 'struct _) d) s)
     (match (parse-base s)
       [(Prog ts ds e)
        (Prog ts (append (parse-struct d) ds) e)])]
    [(cons (and (cons 'define _) d) s)
     (match (parse-base s)
       [(Prog ts ds e)
        (Prog ts (cons (parse-define d) ds) e)])]
    [(cons (cons ': t) s)
        (match (parse-base s)
          [(Prog ts ds e)
          (Prog (cons (parse-type-def t) ts) ds e)])]
    [(cons e '()) (Prog '() '() (parse-e e))]
    [_ (error "program parse error")]))

;; [Listof S-Expr -> Type]
(define (parse-type-def s)
  (match s
    [(list (? symbol? f) types)
      (Type f (parse-type types))]))

(define (parse-type t)
  (match t
    ['Integer                 (TInt)]
    ['Char                    (TChar)]
    ['String                  (TStr)]
    ['Boolean                 (TBool)]
    ['Any                     (TAny)]
    [(? symbol?)              (TStruct t)]
    ((list '-> ins ... out)   (TFunc (map parse-type ins) out))
    [(list 'Listof t)         (TList (parse-type t))]
    [(list 'Vectorof t)       (TVec (parse-type t))]
    [(list 'Union t1 t2)      (TUnion (parse-type t1) (parse-type t2))]
    [_                        (error "Parse Error: invalid type")]
  )
)

;; -------------- Changes End --------------

;; S-Expr -> [Listof Defn]
(define (parse-struct s)
  (match s
    [(list 'struct (? symbol? n) flds)
     (if (andmap symbol? flds)
         (list* (make-struct-defn-construct n flds)
                (make-struct-defn-predicate n)
                (make-struct-defn-accessors n (reverse flds)))
         (error "parse struct definition error"))]
    [_ (error "parse struct definition error")]))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-construct n flds)
  (Defn n flds
    (Prim 'make-struct (cons (Quote n) (map Var flds)))))

;; Id -> [Listof Defn]
(define (make-struct-defn-predicate n)
  (Defn (symbol-append n '?) (list 'x)
    (Prim 'struct? (list (Quote n) (Var 'x)))))

;; Id [Listof Id] -> [Listof Defn]
(define (make-struct-defn-accessors n flds)
  (match flds
    ['() '()]
    [(cons f flds)
     (cons (Defn (symbol-append n '- f) (list 'x)
             (Prim 'struct-ref
                   (list (Quote n)
                         (Quote (length flds))
                         (Var 'x))))
           (make-struct-defn-accessors n flds))]))

;; Symbol ... -> Symbol
(define (symbol-append . ss)
  (string->symbol
   (apply string-append (map symbol->string ss))))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? self-quoting?)             (Quote s)]
    [(list 'quote d)               (Quote d)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list (? (op? op0) p0))       (Prim p0 '())]
    [(list (? (op? op1) p1) e)     (Prim p1 (list (parse-e e)))]
    [(list (? (op? op2) p2) e1 e2) (Prim p2 (list (parse-e e1) (parse-e e2)))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim p3 (list (parse-e e1) (parse-e e2) (parse-e e3)))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons 'match (cons e ms))
     (parse-match (parse-e e) ms)]
    [(list (or 'lambda 'λ) xs e)
     (if (and (list? xs)
              (andmap symbol? xs))
         (Lam (gensym 'lambda) xs (parse-e e))
         (error "parse lambda error"))]
    [(cons 'and es) (And-op (map parse-e es))]
    [(cons 'or  es) (Or-op  (map parse-e es))]
    [(cons e es)
     (App (parse-e e) (map parse-e es))]
    [_ (error "Parse error" s)]))

(define (parse-match e ms)
  (match ms
    ['() (Match e '() '())]
    [(cons (list p r) ms)
     (match (parse-match e ms)
       [(Match e ps es)
        (Match e
               (cons (parse-pat p) ps)
               (cons (parse-e r) es))])]))

(define (parse-pat p)
  (match p
    [(? boolean?) (PLit p)]
    [(? integer?) (PLit p)]
    [(? char?)    (PLit p)]
    ['_           (PWild)]
    [(? symbol?)  (PVar p)]
    [(? string?)  (PStr p)]
    [(list 'quote (? symbol? s))
     (PSymb s)]
    [(list 'quote (list))
     (PLit '())]
    [(list 'box p)
     (PBox (parse-pat p))]
    [(list 'cons p1 p2)
     (PCons (parse-pat p1) (parse-pat p2))]
    [(list 'and p1 p2)
     (PAnd (parse-pat p1) (parse-pat p2))]
    [(cons 'list '())
     (PLit '())]
    [(cons 'list (cons p1 ps))
     (PCons (parse-pat p1)
            (parse-pat (cons 'list ps)))]
    [(cons (? symbol? n) ps)
     (PStruct n (map parse-pat ps))]))

(define (self-quoting? x)
  (or (integer? x)
      (boolean? x)
      (char? x)
      (string? x)
      (box? x)
      (vector? x)))

(define op0
  '(read-byte peek-byte void))
(define op1
  '(add1 sub1 zero? char? write-byte eof-object?
         integer? boolean? integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length
         symbol? symbol->string string->symbol string->uninterned-symbol))
(define op2
  '(+ - < = cons eq? make-vector vector-ref struct?
      make-string string-append string-ref raise-type-error))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))

(define (type->runtime-struct t)
  (parse-e (type->runtime-struct* t)))

(define (type->runtime-struct* t)
  (match t
    [(TInt) ''Integer]
    [(TBool) ''Boolean]
    [(TChar) ''Character]
    [(TStr) ''String]
    [(TStruct name) `(Struct ',name)]
    [(TAny) ''Any]
    [(TList t) `(Listof ,(type->runtime-struct* t))]
    [(TVec t) `(Vectorof ,(type->runtime-struct* t))]
    [(TUnion t1 t2) `(Union ,(type->runtime-struct* t1)
                            ,(type->runtime-struct* t2))]))

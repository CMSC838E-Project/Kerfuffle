#lang racket
(provide Listof Vectorof Union ann)

(struct Listof (t))
(struct Vectorof (t))
(struct Union (t1 t2))
(struct Struct (name))
(struct Proc (ins out))

(define (check-function-args t args)
  (match t
    [(Proc ins out) (check-function-args* ins args)]))

(define (check-function-args* ts args)
  (match args
    ['() #t]
    [(cons a args) (begin (ann a (car ts))
                          (check-function-args* (cdr ts) args))]))


(define (ann v t)
  (if (ann* v t)
      v
      (raise-type-error (type->string t) v)))

(define (ann* v t)
  (match t
    ['Integer (integer? v)]
    ['Boolean (boolean? v)]
    ['Character (char? v)]
    ['String (string? v)]
    [(Struct name) (struct? name v)]
    ['Any #t]
    [(Listof t) (or (empty? v)
                    (and (cons? v)
                         (ann* (car v) t)
                         (ann* (cdr v) (Listof t))))]
    [(Vectorof t) (and (vector? v)
                       (matches-vector v t 0))]
    [(Union t1 t2) (or (ann* v t1)
                       (ann* v t2))]))

(define (matches-vector v t i)
  (if (< i (vector-length v))
      (and (ann* (vector-ref v i) t)
           (matches-vector v t (add1 i)))
      #t))

(define (type->string t)
  (match t
    ['Integer "Integer"]
    ['Character "Character"]
    ['String "String"]
    ['Any "Any"]
    [(Struct name) (symbol->string name)]
    [(Listof t) (string-append "(Listof "
                               (string-append (type->string t)
                                              ")"))]
    [(Vectorof t) (string-append "(Vectorof "
                                 (string-append (type->string t)
                                                ")"))]
    [(Union t1 t2) (string-append "(Union "
                                  (string-append (type->string t1)
                                                 (string-append " "
                                                                (string-append (type->string t2) ")"))))]))
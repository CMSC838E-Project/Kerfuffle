#lang racket
(provide Listof Vectorof Union ann)

(struct Listof (t))
(struct Vectorof (t))
(struct Union (t1 t2))

(define (ann v t)
  (if (ann* v t)
      v
      #f))

(define (ann-error v t)
  (if (ann* v t)
      v
      (raise-type-error (type->string t) v)))

(define (ann* v t)
  (match t
    ['Integer (integer? v)]
    ['Character (char? v)]
    ['String (string? v)]
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
#lang racket
(provide main)
(require "parse.rkt" "compile.rkt" "read-all.rkt" a86/printer)

;; String -> Void
;; Compile contents of given file name,
;; emit asm code on stdout
(define (main fn)
  (let ((p (open-input-file fn))
        (type-lib (open-input-file "type-match.rkt")))
    (begin
      (read-line p) ; ignore #lang racket line
      (read-line type-lib)
      (read-line type-lib)
      (asm-display (compile (parse (append (read-all type-lib) 
                                           (read-all p)))))
      (close-input-port p)
      (close-input-port type-lib))))

#lang racket
(require "test-runner.rkt"
         "../parse.rkt"
         "../compile.rkt"
         "../unload-bits-asm.rkt"
         "../read-all.rkt"
         a86/interp)

;; link with runtime for IO operations
(unless (file-exists? "../runtime.o")
  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string (normalize-path "../runtime.o"))))

(let ((p (open-input-file "../type-match.rkt")))
  (read-line p)
  (read-line p)
  (let ((match-lib (read-all p)))
    (test-runner    (λ p (unload/free (asm-interp (compile (parse (append match-lib
                                                                          p)))))))
    (test-runner-io (λ (s . p)
                      (match (asm-interp/io (compile (parse (append match-lib
                                                                    p))) s)
                        ['err 'err]
                        [(cons r o) (cons (unload/free r) o)]))))
  
  (close-input-port p))

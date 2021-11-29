#lang rosette

(require rackunit)
(require "../traat/termIR.rkt")
(require "../halide-sketch.rkt")

;; this is definitely failing
(define f (termIR->function (sigma-term '/ (list (sigma-term '+ (list (sigma-term '* '("t0" -1)) "n0")) "n1")) (list "t0" "n0" "n1")))

(define term1 (sigma-term '+ (list "x" "y")))
(define term1-f (termIR->function term1 (list "x" "y")))

(check-equal? (term1-f 3 4) 7)

(define term2 (sigma-term '* (list "x" -1)))
(define term2-f (termIR->function term2 (list "x")))

(check-equal? (term2-f 3) -3)

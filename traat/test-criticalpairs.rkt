#lang racket

(require rackunit)
(require "termIR.rkt")
(require "matching.rkt")
(require "criticalpairs.rkt")

(define (cf s)
    (sigma-term 'f (list s "z")))
(define t (sigma-term 'f (list "x" "y")))
(define r1 (make-rule (sigma-term 'f (list (sigma-term 'f (list "x" "y")) "z"))
                      (sigma-term 'f (list "x" (sigma-term 'f (list "y" "z"))))))
(define r2 (make-rule (sigma-term 'f (list (sigma-term 'i (list "x1")) "x1"))
                       "e"))

(check-equal? (CP cf t (rule-rhs r1) r2) (eq-identity
                                          (sigma-term 'f (list (sigma-term 'i '("x1")) (sigma-term 'f '("x1" "z"))))
                                          (sigma-term 'f '("e" "z"))))

(check-equal? (CPs (list r2) r1) (list (eq-identity
                                        (sigma-term 'f (list (sigma-term 'i '("x1")) (sigma-term 'f '("x1" "v2"))))
                                        (sigma-term 'f '("e" "v2")))))

(define cp1 (eq-identity
  (sigma-term 'f (list (sigma-term 'i '("x1")) (sigma-term 'f '("x1" "v2"))))
  (sigma-term 'f '("e" "v2"))))
(define cp2 (eq-identity
    (sigma-term 'f (list "x" (sigma-term 'f '("y" "z"))))
    (sigma-term 'f (list "x" (sigma-term 'f '("y" "z"))))))
(define cp3 (eq-identity
    (sigma-term 'f (list (sigma-term 'f '("x" "y")) (sigma-term 'f '("z" "v2"))))
    (sigma-term
     'f
     (list (sigma-term 'f (list "x" (sigma-term 'f '("y" "z")))) "v2"))))
(define cp4 (eq-identity "v1" "e"))

(check-true (not (not (member cp1 (critical-pairs (list r1 r2))))))
(check-true (not (not (member cp2 (critical-pairs (list r1 r2))))))
(check-true (not (not (member cp3 (critical-pairs (list r1 r2))))))
(check-true (not (not (member cp4 (critical-pairs (list r1 r2))))))
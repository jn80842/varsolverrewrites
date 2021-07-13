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
;; it would be better to check these are equal modulo alpha renaming
(check-equal? (critical-pair-subst (CP cf t (rule-rhs r1) r2 r1))  (list '("y" . "x1") (cons "x" (sigma-term 'i '("x1")))))

(check-equal? (critical-pair-subst (list-ref (CPs (list r2) r1) 0)) (list '("v1" . "x1") (cons "v0" (sigma-term 'i '("x1")))))

(define cp1 (critical-pair
             (rule (sigma-term 'f (list (sigma-term 'f '("v0" "v1")) "v2"))
                   (sigma-term 'f (list "v0" (sigma-term 'f '("v1" "v2"))))
                   "")
             (rule
              (sigma-term 'f (list (sigma-term 'f '("x" "y")) "z"))
              (sigma-term 'f (list "x" (sigma-term 'f '("y" "z"))))
              "")
             (list '("v1" . "z") (cons "v0" (sigma-term 'f '("x" "y"))))))
(define cp2 (critical-pair (rule
                            (sigma-term 'f (list (sigma-term 'f '("v0" "v1")) "v2"))
                            (sigma-term 'f (list "v0" (sigma-term 'f '("v1" "v2"))))
                            "")
                           (rule (sigma-term 'f (list (sigma-term 'i '("x1")) "x1")) "e" "")
                           (list '("v1" . "x1") (cons "v0" (sigma-term 'i '("x1"))))))

(check-true (not (not (member cp1 (critical-pairs (list r1 r2))))))
(check-true (not (not (member cp2 (critical-pairs (list r1 r2))))))
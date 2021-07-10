#lang racket

(require rackunit)
(require "traat/termIR.rkt")
(require "saturation-synthesis.rkt")

(define (exists-in? elt l)
  (not (not (member elt l))))

(check-equal? (find-all-patterns "x" "y") (list "x"))
(check-equal? (find-all-patterns 2 "y") (list "v0" 2))
(check-equal? (length (find-all-patterns (sigma-term '+ (list "x" 2)) "y")) 3)
(check-true (exists-in? (sigma-term '+ (list "x" "v1"))
                        (find-all-patterns (sigma-term '+ (list "x" 2)) "y")))
(check-true (exists-in? (sigma-term '+ (list "v1" "v1"))
                        (find-all-patterns (sigma-term '+ (list 2 2)) "y")))
(check-true (exists-in? (sigma-term '+ (list "v1" "v1"))
                        (find-all-patterns (sigma-term '+ (list (sigma-term '* (list "x" "y")) (sigma-term '* (list "x" "y"))))
                                           "z")))
(check-true (exists-in? (sigma-term '+ (list "tvar1" "tvar1"))
                        (find-all-patterns (sigma-term '+ (list (sigma-term '* (list "x" "y")) (sigma-term '* (list "x" "y"))))
                                           "y")))
#lang racket

(require rackunit)
(require "../traat/termIR.rkt")
(require "../saturation-synthesis.rkt")

(define (exists-in? elt l)
  (not (not (member elt l))))

;; patterns that are just 1 variable can't be LHSs
(check-equal? (find-all-patterns "x" "y" 10) '())
(check-equal? (find-all-patterns 2 "y" 10) '())
(check-equal? (find-all-patterns "x" "x" 10) '())

(check-equal? (length (find-all-patterns (sigma-term '+ (list "x" 2)) "y" 10)) 1)
(check-true (exists-in? (sigma-term '+ (list "n1" "n2"))
                        (find-all-patterns (sigma-term '+ (list "x" 2)) "y" 10)))
(check-true (exists-in? (sigma-term '+ (list "n1" "n1"))
                        (find-all-patterns (sigma-term '+ (list 2 2)) "y" 10)))
(check-true (exists-in? (sigma-term '+ (list "n1" "n1"))
                        (find-all-patterns (sigma-term '+ (list (sigma-term '* (list "x" "y")) (sigma-term '* (list "x" "y"))))
                                           "z" 10)))
(check-true (exists-in? (sigma-term '+ (list "t1" "t1"))
                        (find-all-patterns (sigma-term '+ (list (sigma-term '* (list "x" "y")) (sigma-term '* (list "x" "y"))))
                                           "y" 10)))
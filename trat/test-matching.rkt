#lang racket

(require rackunit)
(require "matching.rkt")

(define patt1 (sigma-term "+" (list "x" "y")))
(define patt2 (sigma-term "x" (list "x" "x")))

(define obj1 (sigma-term "+" (list (sigma-term "+" (list "a" "b")) "c")))
(define obj2 (sigma-term "*" (list (sigma-term "+" (list "a" "b")) "c")))

(check-equal? (match patt1 obj1) (make-immutable-hash (list (cons "x" (sigma-term "+" (list "a" "b"))) (cons "y" "c"))))
(check-equal? (match patt1 obj2) 'fail)
(check-equal? (match patt2 obj1) 'fail)
#lang racket

(require rackunit)
(require "matching.rkt")

(define patt1 (sigma-term "+" (list "x" "y")))
(define patt2 (sigma-term "*" (list "x" "x")))

(define vs-patt1 (sigma-term "+" (list "t0" "n0")))
(define vs-patt2 (sigma-term "*" (list "n0" "n0")))

(define obj1 (sigma-term "+" (list (sigma-term "+" (list "a" "b")) "c")))
(define obj2 (sigma-term "*" (list (sigma-term "+" (list "a" "b")) "c")))

(check-equal? (match patt1 obj1) (make-immutable-hash (list (cons "x" (sigma-term "+" (list "a" "b"))) (cons "y" "c"))))
(check-equal? (match patt1 obj2) 'fail)
(check-equal? (match patt2 obj1) 'fail)

(check-equal? (varsolver-match vs-patt1 obj1 "a") (make-immutable-hash (list (cons "t0" (sigma-term "+" (list "a" "b"))) (cons "n0" "c"))))
(check-equal? (varsolver-match vs-patt1 obj1 "c") 'fail)
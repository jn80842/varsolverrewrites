#lang racket

(require rackunit)
(require "matching.rkt")

(define patt1 (sigma-term "+" (list "x" "y")))
(define patt2 (sigma-term "*" (list "x" "x")))

(define vs-patt1 (sigma-term "+" (list "t0" "n0")))
(define vs-patt2 (sigma-term "*" (list "n0" "n0")))

(define obj1 (sigma-term "+" (list (sigma-term "+" (list "a" "b")) "c")))
(define obj2 (sigma-term "*" (list (sigma-term "+" (list "a" "b")) "c")))
(define obj3 (sigma-term "*" (list (sigma-term "+" (list "a" "b")) (sigma-term "+" (list "a" "b")))))

(check-equal? (match patt1 obj1) (make-immutable-hash (list (cons "x" (sigma-term "+" (list "a" "b"))) (cons "y" "c"))))
(check-equal? (match patt1 obj2) 'fail)
(check-equal? (match patt2 obj1) 'fail)
(check-equal? (match patt2 obj3) (make-immutable-hash (list (cons "x" (sigma-term "+" (list "a" "b"))))))

(check-equal? (varsolver-match vs-patt1 obj1 "a") (make-immutable-hash (list (cons "t0" (sigma-term "+" (list "a" "b"))) (cons "n0" "c"))))
(check-equal? (varsolver-match vs-patt1 obj1 "c") 'fail)

(define rule1-LHS (sigma-term "+" (list "x" "x")))
(define rule1-RHS (sigma-term "*" (list "x" "2")))
(define ruleset1 (list (cons rule1-LHS rule1-RHS)))
(define input (sigma-term "+" (list "a" "a")))
(define input2 (sigma-term "+" (list "a" "b")))

(check-equal? (rewrite ruleset1 input) (sigma-term "*" (list "a" "2")))
(check-equal? (rewrite ruleset1 input2) 'fail)
(check-equal? (rewrite ruleset1 (sigma-term "-" (list input "d"))) 'fail)
(check-equal? (rewrite ruleset1 (sigma-term "+" (list (sigma-term "*" (list "a" "2"))
                                                      (sigma-term "*" (list "a" "2")))))
                       (sigma-term "*" (list (sigma-term "*" (list "a" "2")) "2")))

(check-equal? (rewrite* ruleset1 "x") "x")
(check-equal? (rewrite* ruleset1 input) (sigma-term "*" (list "a" "2")))
(check-equal? (rewrite* ruleset1 (sigma-term "-" (list input "d")))
              (sigma-term "-" (list (sigma-term "*" (list "a" "2")) "d")))
(check-equal? (rewrite* ruleset1 (sigma-term "+" (list (sigma-term "*" (list "a" "2"))
                                                      (sigma-term "+" (list "a" "a")))))
              (sigma-term "*" (list (sigma-term "*" (list "a" "2")) "2")))
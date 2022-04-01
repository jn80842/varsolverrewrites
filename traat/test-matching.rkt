#lang racket

(require rackunit)
(require "termIR.rkt")
(require "matching.rkt")

(define patt1 (sigma-term '+ (list "x" "y")))
(define patt2 (sigma-term '* (list "x" "x")))

(define vs-patt1 (sigma-term '+ (list "t0" "n0")))
(define vs-patt2 (sigma-term '* (list "n0" "n0")))
(define vs-patt3 (sigma-term '+ (list "t0" "c0")))

(define obj1 (sigma-term '+ (list (sigma-term '+ (list "a" "b")) "c")))
(define obj2 (sigma-term '* (list (sigma-term '+ (list "a" "b")) "c")))
(define obj3 (sigma-term '* (list (sigma-term '+ (list "a" "b")) (sigma-term '+ (list "a" "b")))))
(define obj4 (sigma-term '% (list (sigma-term '+ (list "a" "b")) (sigma-term '+ (list "a" "b")))))
(define objc1 (sigma-term '+ (list "a" 3)))

(check-equal? (match patt1 obj1) (make-immutable-hash (list (cons "x" (sigma-term '+ (list "a" "b"))) (cons "y" "c"))))
(check-equal? (match patt1 obj2) 'fail)
(check-equal? (match patt2 obj1) 'fail)
(check-equal? (match patt2 obj3) (make-immutable-hash (list (cons "x" (sigma-term '+ (list "a" "b"))))))

(check-equal? (varsolver-match "a" vs-patt1 obj1) (make-immutable-hash (list (cons "t0" (sigma-term '+ (list "a" "b"))) (cons "n0" "c"))))
(check-equal? (varsolver-match "c" vs-patt1 obj1) 'fail)
(check-equal? (varsolver-match "a" vs-patt3 objc1) (make-immutable-hash (list (cons "t0" "a") (cons "c0" 3))))
(check-equal? (varsolver-match "a" vs-patt3 obj1) 'fail)

(define rule1-LHS (sigma-term '+ (list "x" "x")))
(define rule1-RHS (sigma-term '* (list "x" 2)))
(define rule2-LHS (sigma-term '&& (list "x" 'true)))
(define rule2-RHS "x")
(define ruleset1 (list (make-rule rule1-LHS rule1-RHS)
                       (make-rule rule2-LHS rule2-RHS)))
(define input (sigma-term '+ (list "a" "a")))
(define input2 (sigma-term '+ (list "a" "b")))

(check-equal? (rewrite ruleset1 "a") 'fail)
(check-equal? (rewrite ruleset1 input) (sigma-term '* (list "a" 2)))
(check-equal? (rewrite ruleset1 input2) 'fail)
(check-equal? (rewrite ruleset1 (sigma-term '- (list input "d"))) 'fail)
(check-equal? (rewrite ruleset1 (sigma-term '+ (list (sigma-term '* (list "a" 2))
                                                      (sigma-term '* (list "a" 2)))))
                       (sigma-term '* (list (sigma-term '* (list "a" 2)) 2)))
(check-equal? (rewrite ruleset1 (sigma-term '&& (list "a" 'true))) "a")

(check-equal? (rewrite* ruleset1 "a") "a")
(check-equal? (rewrite* ruleset1 input) (sigma-term '* (list "a" 2)))
(check-equal? (rewrite* ruleset1 (sigma-term '- (list input "d")))
              (sigma-term '- (list (sigma-term '* (list "a" 2)) "d")))
(check-equal? (rewrite* ruleset1 (sigma-term '+ (list (sigma-term '* (list "a" 2))
                                                      (sigma-term '+ (list "a" "a")))))
              (sigma-term '* (list (sigma-term '* (list "a" 2)) 2)))

(define vsrule1-LHS (sigma-term '+ (list "n0" "t0")))
(define vsrule1-RHS (sigma-term '+ (list "t0" "n0")))
(define vsruleset1 (list (make-rule vsrule1-LHS vsrule1-RHS)))

(check-equal? (varsolver-rewrite "y" vsruleset1 patt1) (sigma-term '+ (list "y" "x")))
(check-equal? (varsolver-rewrite "x" vsruleset1 patt1) 'fail)

(check-equal? (varsolver-rewrite* "c" vsruleset1 input2) input2)
(check-equal? (varsolver-rewrite* "b" vsruleset1 obj4)
              (sigma-term '% (list (sigma-term '+ (list "b" "a")) (sigma-term '+ (list "b" "a")))))
(define obj5 (sigma-term '+ (list "z" (sigma-term '* '("x" "y")))))
(define obj6 (sigma-term
              'min
              (list
               (sigma-term '+ (list (sigma-term '+ '("y" "x")) "z"))
               (sigma-term '+ (list "z" (sigma-term '* '("x" "y")))))))

(check-equal? (varsolver-rewrite "x" vsruleset1 obj5) (sigma-term '+ (list (sigma-term '* '("x" "y")) "z")))
(check-equal? (varsolver-rewrite* "x" vsruleset1 obj6) (sigma-term
              'min
              (list
               (sigma-term '+ (list (sigma-term '+ '("x" "y")) "z"))
               (sigma-term '+ (list (sigma-term '* '("x" "y")) "z")))))

(define vsruleset2 (list (make-rule (sigma-term '+ (list (sigma-term '* (list "t0" "x")) (sigma-term '* (list "t0" "y"))))
                                    (sigma-term '* (list "t0" (sigma-term '+ (list "x" "y")))))))

(define obj7 (sigma-term '+ (list (sigma-term '* (list "a" "b")) (sigma-term '* (list "a" "c")))))
(check-equal? (varsolver-rewrite* "a" vsruleset2 obj7) (sigma-term '* (list "a" (sigma-term '+ (list "b" "c")))))

(check-equal? (unify "x" (sigma-term '+ (list "a" "b"))) (list (cons "x" (sigma-term '+ (list "a" "b")))))
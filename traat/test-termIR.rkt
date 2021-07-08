#lang racket

(require rackunit)
(require "termIR.rkt")

(define term1 (sigma-term '+ (list (sigma-term '* (list "a" 2))
                                   (sigma-term '+ (list "b" "a")))))
(define term2 (sigma-term '* (list "a" (sigma-term '+ (list "c" "d")))))

(check-equal? (length (termIR->variables term1)) 2)

(check-true (termIR->in-solved-form? term2 "a"))
(check-false (termIR->in-solved-form? term1 "a"))
(check-true (termIR->in-solved-form? term1 "c"))

(define term3 (sigma-term '+ (list (sigma-term '+ (list (sigma-term '* (list "a" 2))
                                   (sigma-term '+ (list "b" "a")))) "d")))

(check-equal? (term-size term3) 9)
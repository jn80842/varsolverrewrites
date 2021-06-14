#lang racket

(require rackunit)
(require "termIR.rkt")

(check-equal? (length (termIR->variables (sigma-term '+ (list (sigma-term '* (list "a" 2))
                                                              (sigma-term '+ (list "b" "a")))))) 2)
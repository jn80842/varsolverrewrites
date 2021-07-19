#lang rosette

(require rackunit)

(require "traat/termIR.rkt")
(require "halide-parser.rkt")
(require "varsolver-synthesis.rkt")

(define expr1 (halide->termIR "n0 + ((n1 * 0) + (t0 - n0))"))

(check-true (verify-RHS-is-target-variable? expr1))

(define expr2 (halide->termIR "((n0 + t0) - t0) - n1"))
(define nonly-rule (synthesize-nonly-rewrite expr2 1))

(check-equal? nonly-rule (make-rule expr2 (sigma-term '- (list "n0" "n1"))))

(define expr3 (halide->termIR "min(n0 + t0, n1 + t0)"))
(define topn-rule (synthesize-topn-rewrite expr3 1))

(check-true (or (equal? topn-rule (make-rule expr3 (sigma-term '+ (list "t0" (sigma-term 'min (list "n0" "n1"))))))
                (equal? topn-rule (make-rule expr3 (sigma-term '+ (list "t0" (sigma-term 'min (list "n1" "n0"))))))))
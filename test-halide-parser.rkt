#lang racket

(require rackunit)
(require "halide-parser.rkt")

(check-true (halide-expr-in-solved-form? "t0"))
(check-true (halide-expr-in-solved-form? "n0 + n1"))
(check-true (halide-expr-in-solved-form? "t0 + (n0 - n1)"))
(check-false (halide-expr-in-solved-form? "n0 + (t0 - n1)"))
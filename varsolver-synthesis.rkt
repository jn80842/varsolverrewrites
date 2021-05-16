#lang rosette

(require "halide-lang.rkt")
(require "halide-sketch.rkt")
(require "halide-print-sketch.rkt")

;; assume LHS is a function that takes the same inputs as the RHS sketch
(define (synthesize-rewrite LHS sk inputs)
  (let* ([evaled-sketch (apply (get-sketch-function sk) inputs)]
         [evaled-LHS (apply LHS inputs)]
         [model (time (synthesize #:forall (symbolics inputs)
                                  #:guarantee (assert (equal? evaled-sketch evaled-LHS))))])
    (if (unsat? model)
        (displayln "Could not find an equivalent RHS")
        (displayln (print-sketch (evaluate sk model))))))

(define LHS1
  (λ (i1 i2 i3)
    (hld-add (hld-mul i1 i2) (hld-mul i1 i3))))

(define sk1 (get-symbolic-sketch 3 3))

(define-symbolic* i1 integer?)
(define-symbolic* i2 integer?)
(define-symbolic* i3 integer?)

(define inputs (list i1 i2 i3))

;(synthesize-rewrite LHS1 sk1 (list i1 i2 i3))

;; (rewrite(max(t0 + n0, t0 + n1), (t0 + max(n0, n1)), "coqMax396c"))

(define LHS2
  (λ (t0 n0 n1) (max (+ t0 n0) (+ t0 n1))))

(define-symbolic* sym-op-idx integer?)

(define sk2 (get-symbolic-sketch 2 2))

(define-symbolic* sym-tarvar integer?)

(define (synthesize-topn-rewrite LHS sk op-idx tarvar inputs)
  (let* ([evaled-sketch (apply (get-topn-sketch-function sk op-idx) tarvar inputs)]
         [evaled-LHS (apply LHS tarvar inputs)]
         [model (time (synthesize #:forall (symbolics (cons tarvar inputs))
                                  #:guarantee (assert (equal? evaled-sketch evaled-LHS))))])
    (if (unsat? model)
        (displayln "Could not find equivalent RHS")
        (displayln (print-topn-sketch (evaluate sk model) (evaluate op-idx model))))))
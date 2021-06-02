#lang rosette

(require "halide-lang.rkt")
(require "halide-sketch.rkt")
(require "halide-print-sketch.rkt")
(require "halide-parser.rkt")

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

(define (pull-out-target-var var-list target-idx)
  (let ([size (length var-list)])
    (append (list (list-ref var-list target-idx)) (take var-list target-idx) (drop var-list (add1 target-idx)))))

(define (insert-target-var nv-list tarvar target-idx)
  (append (take nv-list target-idx) (list tarvar) (drop nv-list target-idx)))

;; assumptions:
;; LHS contains 3 variables
(define (synthesize-3var-rewrite LHS-string tar-idx)
  (let* ([LHS-func (eval (call-with-input-string (halide->rktlang LHS-string) read))]
         [op-count (halide->countops LHS-string)]
         [sk (get-symbolic-sketch 2 op-count)]
         [renamed-LHS (halide->renamevars LHS-string (make-hash (map cons (list "x" "y" "z")
                                                                     (insert-target-var (list "n0" "n1") "t0" tar-idx))))])
    (begin
      (clear-asserts!)
      (define-symbolic* tarvar integer?)
      (define-symbolic* n0 integer?)
      (define-symbolic* n1 integer?)
      (define-symbolic* root-op integer?)
      (let* ([evaled-sketch (apply (get-topn-sketch-function sk root-op) tarvar (list n0 n1))]
             [evaled-LHS (apply LHS-func (insert-target-var (list n0 n1) tarvar tar-idx))]
             [model (time (synthesize #:forall (list tarvar n0 n1)
                                      #:guarantee (assert (equal? evaled-sketch evaled-LHS))))])
        (if (unsat? model)
            (displayln (format "Could not find equivalent RHS for ~a" renamed-LHS))
            (displayln (format "rewrite(~a, ~a);" renamed-LHS
                               (topn-sketch->halide-expr (evaluate sk model) (evaluate root-op model))))))
            )))


(define patts (list
"((x/y)*z)"
"((x/y)/z)"
"(x/(y/z))"
"((x*y)*z)"
"((x/y)*z)"
"(x + (y*z))"
"((x/y) + z)"
"((x - y)/z)"
"min(x*y, z)"
"((x*y) + z)"
"((x + y)/z)"
"((x % y)*z)"
"((x/y) < z)"
"(x*(y + z))"
"((x + y)*z)"
"((x - y)*z)"
"(x - (y/z))"
"((x*y) - z)"
"(x + (y/z))"
"(x - (y*z))"
"((x*y) + z)"
"((x/y) - z)"
"(x + (y*z))"
"((x/y) + z)"
"min(x, y/z)"
"max(x*y, z)"
"min(x/y, z)"
"max(x, y/z)"
"max(x/y, z)"
"(x < (y/z))"
"min(x, y*z)"
"(x < (y*z))"
"((x + y)/z)"
"max(x, y*z)"
"(x/(y + z))"
"((x/y) % z)"
"(x % (y/z))"
"((x + y)*z)"
"((x*y) < z)"
"(x*(y - z))"
"(x <= (y/z))"
"(x <= (y*z))"
"(x <= (y/z))"
"(x <= (y*z))"
"!((x/y) < z)"
"((x/y) <= z)"
"((x/y) >= z)"
"((x*y) >= z)"
"((x*y) <= z)"
"(x >= (y*z))"
"!(x < (y/z))"
"((x + y) + z)"
"(x - (y % z))"
"((x + y) % z)"
"min(x, y + z)"
"(x + (y + z))"
"((x + y) < z)"
"max(x + y, z)"
"((x + y) - z)"
"(x < (y % z))"
"(x + (y - z))"
"((x + y) < z)"
"!(x <= (y/z))"
"(x < (y + z))"
"((x % y) < z)"
"((x - y) + z)"
"(x - (y - z))"
"max(x, y + z)"
"min(x, y - z)"
"max(x - y, z)"
"min(x - y, z)"
"min(x + y, z)"
"((x - y) - z)"
"(x - (y + z))"
"(x + (y + z))"
"(max(x, y)*z)"
"(min(x, y)*z)"
"(min(x, y)/z)"
"((x + y) + z)"
"(((x/y)/z)*z)"
"min((x*y), z)"
"min(x/y, z/y)"
"min(x*y, z*y)"
"((x % y) + z)"
"(x % (y + z))"
"(max(x, y)/z)"
"min((x), y/z)"
"max(x*y, z*y)"
"(((x/y)/y)*z)"
"max(x, y - z)"
"((x % y) - z)"
"((x + y) + z)"
"max(x/y, z/y)"
"((x + y) <= z)"
"(x <= (y + z))"
"((x - y) <= z)"
"((x + y) <= z)"
"(x || (y < z))"
"!(x < (y + z))"
"!((x + y) < z)"
))

(for ([lhs (take patts 2)])
  (begin
    (synthesize-3var-rewrite lhs 0)
    (synthesize-3var-rewrite lhs 1)
    (synthesize-3var-rewrite lhs 2)))
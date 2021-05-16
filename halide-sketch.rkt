#lang rosette

(require "halide-lang.rkt")

(provide (all-defined-out))

(define (get-sym-int)
  (define-symbolic* x integer?)
  x)

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)

(struct insn (op-idx arg1-idx arg2-idx arg3-idx) #:transparent)

(define (call-insn i registers)
  ((get-operator-function-by-idx (insn-op-idx i)) (list-ref registers (insn-arg1-idx i))
                                                  (list-ref registers (insn-arg2-idx i))
                                                  (list-ref registers (insn-arg3-idx i))))

(define (get-sym-insn)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (insn op arg1 arg2 arg3))

(struct sketch (insn-list retval-idx input-count) #:transparent)

(define (get-symbolic-sketch input-count insn-count)
  (define-symbolic* retval integer?)
  (sketch (for/list ([i (range insn-count)]) (get-sym-insn)) retval input-count))

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-regs i)
                (cond [(equal? (length (sketch-insn-list sk)) i) calculated-regs]
                      [else (let ([next-reg (call-insn (list-ref (sketch-insn-list sk) i) calculated-regs)])
                              (f (append calculated-regs (list next-reg)) (add1 i)))]))])
    (λ inputs (list-ref (f inputs 0) (sketch-retval-idx sk)))))

(define (get-topn-sketch-function sk op-idx)
  (λ (tarvar . inputs) 
    ((get-operator-function-by-idx op-idx) tarvar (apply (get-sketch-function sk) inputs) 0)))
#lang rosette

(require "typed-halide-lang.rkt")
(require "traat/termIR.rkt")

(provide (struct-out sketch)
         (struct-out insn))
(provide get-sym-int
         get-register
         get-symbolic-sketch
         get-sketch-function
         get-topn-sketch-function
         termIR->function)

(define (get-sym-int)
  (define-symbolic* x integer?)
  x)

(define (get-sym-int-register)
  (register 'int (get-sym-int)))

(define (get-sym-bool)
  (define-symbolic* b boolean?)
  b)

(define (get-sym-bool-register)
  (register 'bool (get-sym-bool)))

(define (get-register val)
  (let ([reg-type (if (integer? val)
                      'int
                      (if (boolean? val)
                          'bool
                          'error))])
    (register reg-type val)))

(struct insn (op-idx arg1-idx arg2-idx arg3-idx) #:transparent)

(define (call-insn operator-list i registers)
  ((get-operator-function-by-idx operator-list (insn-op-idx i)) (list-ref registers (insn-arg1-idx i))
                                                                (list-ref registers (insn-arg2-idx i))
                                                                (list-ref registers (insn-arg3-idx i))))

(define (get-sym-insn)
  (define-symbolic* op integer?)
  (define-symbolic* arg1 integer?)
  (define-symbolic* arg2 integer?)
  (define-symbolic* arg3 integer?)
  (insn op arg1 arg2 arg3))

(struct sketch (operator-list insn-list retval-idx input-count) #:transparent)

(define (get-symbolic-sketch operator-list input-count insn-count)
  (define-symbolic* retval integer?)
  (sketch operator-list (for/list ([i (range insn-count)]) (get-sym-insn)) retval input-count))

(define (get-sketch-function sk)
  (letrec ([f (λ (calculated-regs i)
                (cond [(equal? (length (sketch-insn-list sk)) i) calculated-regs]
                      [else (let ([next-reg (call-insn (sketch-operator-list sk) (list-ref (sketch-insn-list sk) i) calculated-regs)])
                              (f (append calculated-regs (list next-reg)) (add1 i)))]))])
    (λ inputs (register-value (list-ref (f (map get-register inputs) 0) (sketch-retval-idx sk))))))

(define (get-topn-sketch-function sk op-idx)
  (λ (tarvar . inputs) 
    (register-value ((get-operator-function-by-idx (sketch-operator-list sk) op-idx)
                     (register-value tarvar) (get-register (apply (get-sketch-function sk) inputs)) 0))))

(define variable-list (list "x" "y" "z"))
(define constant-list (list 0 1 2 -1 #t #f))
(define operator-lookup (make-hash
                         (list (cons '+ hld-add)
                               (cons '- hld-sub)
                               (cons '* hld-mul)
                               (cons '/ hld-div)
                               (cons '% hld-mod)
                               (cons 'max hld-max)
                               (cons 'min hld-min)
                               (cons 'select hld-seli)
                               (cons '== hld-eqi)
                               (cons '!= hld-neqi)
                               (cons '< hld-lt)
                               (cons '> hld-gt)
                               (cons '<= hld-le)
                               (cons '>= hld-ge)
                               (cons '&& hld-and)
                               (cons 'or hld-or)
                               (cons '! hld-not))))

;; takes a termIR expression and returns a function
(define (termIR->function t variable-list)
  (letrec ([f (λ (inputs t)
                (cond [(term-variable? t) (list-ref inputs (index-of variable-list t))]
                      [(equal? 'true t) #t]
                      [(equal? 'false t) #f]
                      [(term-constant? t) t]
                      [(sigma-term? t) (apply (hash-ref operator-lookup (sigma-term-symbol t))
                                              (map (curry f inputs) (sigma-term-term-list t)))]
                      [else 'fail]))])
    (λ inputs (register-value (f (map get-register inputs) t)))))
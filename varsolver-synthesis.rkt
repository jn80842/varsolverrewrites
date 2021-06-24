#lang rosette

(require "halide-lang.rkt")
(require "halide-sketch.rkt")
(require "halide-print-sketch.rkt")
(require "halide-parser.rkt")
(require "varsolverTRS.rkt")
(require "trat/termIR.rkt")
(require "trat/matching.rkt")

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

(define sk1 (get-symbolic-sketch operator-list 3 3))

(define-symbolic* i1 integer?)
(define-symbolic* i2 integer?)
(define-symbolic* i3 integer?)

(define inputs (list i1 i2 i3))

;(synthesize-rewrite LHS1 sk1 (list i1 i2 i3))

;; (rewrite(max(t0 + n0, t0 + n1), (t0 + max(n0, n1)), "coqMax396c"))

(define LHS2
  (λ (t0 n0 n1) (max (+ t0 n0) (+ t0 n1))))

(define-symbolic* sym-op-idx integer?)

(define sk2 (get-symbolic-sketch operator-list 2 2))

(define-symbolic* sym-tarvar integer?)

(define (pull-out-target-var var-list target-idx)
  (let ([size (length var-list)])
    (append (list (list-ref var-list target-idx)) (take var-list target-idx) (drop var-list (add1 target-idx)))))

(define (insert-target-var nv-list tarvar target-idx)
  (append (take nv-list target-idx) (list tarvar) (drop nv-list target-idx)))

;; inputs: pattern, target variable idx
;; returns: rule or void
(define (synthesize-topn-rewrite renamed-LHS LHS-inputs sk tar-idx)
  (begin
    (clear-asserts!)
    (define-symbolic* tarvar integer?)
    (define-symbolic* root-op integer?)
    (let* ([non-tarvars (for/list ([i (range (sketch-input-count sk))]) (get-sym-int))]
           [evaled-sketch (apply (get-topn-sketch-function sk root-op) tarvar non-tarvars)]
           [evaled-LHS (apply (termIR->function (halide->termIR renamed-LHS) LHS-inputs) tarvar non-tarvars)]
           [model (time (with-handlers ([(λ (e) #t)
                                         (λ (e) (displayln (format "Timeout searching for RHS for ~a with insn count ~a"
                                                                   renamed-LHS (length (sketch-insn-list sk)))))])
                          (synthesize #:forall (cons tarvar non-tarvars)
                                      #:guarantee (assert (equal? evaled-sketch evaled-LHS)
                                                                       ))))])
      (unless (void? model)
        (if (unsat? model)
            (displayln (format "Could not find topn RHS for ~a with insn count ~a" renamed-LHS (length (sketch-insn-list sk))))
            (begin
           ;   (displayln (format "Found rule ~a -> ~a" renamed-LHS (topn-sketch->halide-expr (evaluate sk model) (evaluate root-op model))))
              (make-rule (halide->termIR renamed-LHS) (halide->termIR (topn-sketch->halide-expr (evaluate sk model) (evaluate root-op model))))))))))

(define (synth-topn-over-insn-count-range LHS inputs insn-count tar-idx)
  (letrec ([f (λ (i)
                (if (> i insn-count)
                    'fail
                    (let* ([sk (get-symbolic-sketch operator-list (sub1 (length inputs)) i)]
                           [rule (synthesize-topn-rewrite LHS inputs sk tar-idx)])
                      (if (void? rule)
                          (f (add1 i))
                          rule))))])
    (f 0)))

;; assume that target variable is first in LHS-inputs
(define (synthesize-nonly-rewrite renamed-LHS LHS-inputs sk)
  (begin
    (clear-asserts!)
    (define-symbolic* tarvar integer?)
    (let* ([non-tarvars (for/list ([i (range (sketch-input-count sk))]) (get-sym-int))]
           [evaled-sketch (apply (get-sketch-function sk) non-tarvars)]
           [evaled-LHS (apply (termIR->function (halide->termIR renamed-LHS) LHS-inputs) tarvar non-tarvars)]
           [model (time (with-handlers ([(λ (e) #t)
                                         (λ (e) (displayln (format "Timeout searching for n-only RHS for ~a with insn count ~a"
                                                                   renamed-LHS (length (sketch-insn-list sk)))))])
                          (synthesize #:forall (cons tarvar non-tarvars)
                                      #:guarantee (assert (equal? evaled-sketch evaled-LHS)))))])
      (unless (void? model)
        (if (unsat? model)
            (displayln (format "Could not find n-only RHS for ~a with insn count ~a" renamed-LHS (length (sketch-insn-list sk))))
            (make-rule (halide->termIR renamed-LHS) (halide->termIR (sketch->halide-expr (evaluate sk model) (cdr LHS-inputs)))))))))

(define (synth-nonly-over-insn-count-range LHS inputs insn-count)
  (letrec ([f (λ (i)
                (if (> i insn-count)
                    'fail
                    (let ([rule (synthesize-nonly-rewrite LHS inputs
                                                          (get-symbolic-sketch operator-list (sub1 (length inputs)) i))])
                      (if (void? rule)
                          (f (add1 i))
                          rule))))])
    (f 0)))

(define (verify-RHS-is-target-variable? renamed-LHS LHS-inputs)
  (clear-asserts!)
  (define-symbolic* tarvar integer?)
  (let* ([non-tarvars (for/list ([i (range (sub1 (length LHS-inputs)))]) (get-sym-int))]
         [evaled-LHS (apply (termIR->function renamed-LHS LHS-inputs) tarvar non-tarvars)]
         [cex (verify (assert (equal? evaled-LHS tarvar)))])
    (unsat? cex)))

;; assume input pattern is normalized and not in solved form
(define (find-rule patt tvar)
  (let* ([pattIR (halide->termIR patt)]
         [LHS-variables (termIR->variables pattIR)]
         [renamed-LHS-variables (cons "t0" (for/list ([i (range (sub1 (length LHS-variables)))])
                                                                                (format "n~a" i)))]
         [candidate-LHS (termIR->renamevars pattIR (make-hash (map cons (cons tvar (remove tvar LHS-variables))
                                                                   renamed-LHS-variables)))])
    (if (verify-RHS-is-target-variable? candidate-LHS renamed-LHS-variables)
        (make-rule candidate-LHS "t0")
        (if (equal? (length renamed-LHS-variables) 1)
            (begin
              (displayln (format "No possible equivalent RHS in form t op N for ~a" patt))
              'fail)
            (let ([nonly-rule (synth-nonly-over-insn-count-range (termIR->halide candidate-LHS) renamed-LHS-variables
                                                                 (halide->countops (termIR->halide candidate-LHS)))])
              (if (not (equal? 'fail nonly-rule))
                  nonly-rule
                  (synth-topn-over-insn-count-range (termIR->halide candidate-LHS) renamed-LHS-variables
                                                    (halide->countops (termIR->halide candidate-LHS)) 0)))))))

;; assume input patterns have target variable x
(define (find-rules patts originalTRS)
  (letrec ([f (λ (patterns TRS)
                (if (empty? patterns)
                    TRS
                    (begin
                      (displayln (format "Searching for rule for input ~a" (car patterns)))
                      (let ([e (varsolver-rewrite* "x" TRS (halide->termIR (car patterns)) rule->halide-string)])
                      (if (termIR->in-solved-form? e "x")
                          (begin
                            (displayln (format "Input ~a was solved by TRS" (car patterns)))
                            (f (cdr patterns) TRS))
                          (let ([candidate-rule (find-rule (termIR->halide e) "x")])
                            (if (equal? 'fail candidate-rule)
                                (begin
                                  (displayln (format "Could not find RHS for input ~a" (car patterns)))
                                  (f (cdr patterns) TRS))
                                (if (equal? (rule-lhs candidate-rule) (rule-rhs candidate-rule))
                                    (begin
                                      (displayln (format "Synthesized same RHS as LHS: ~a" (rule-lhs candidate-rule)))
                                      (f (cdr patterns) TRS))
                                    (begin
                                      (displayln (format "FOUND NEW RULE: ~a -> ~a" (termIR->halide (rule-lhs candidate-rule))
                                                         (termIR->halide (rule-rhs candidate-rule))))
                                      (f (cdr patterns) (append TRS (list candidate-rule))))))))))))])
    (f patts originalTRS)))

(define (check-patt patt tar-idx)
  (let ([renamed-patt (halide->renamevars patt (make-hash (map cons (list "x" "y" "z")
                                                                     (insert-target-var (list "n0" "n1") "t0" tar-idx))))]
        [tvar (list-ref '("x" "y" "z") tar-idx)])
    (if (halide-expr-in-solved-form? renamed-patt)
        (displayln (format "~a with target variable ~a is already in solved form" patt tvar))
        (let ([normalized-patt (normalize->termIR patt tvar)])
          (unless (equal? normalized-patt (halide->termIR patt))
            (displayln (format "~a with target variable ~a normalized to ~a"
                               patt tvar (termIR->halide normalized-patt))))))))

#;(for ([lhs-pair patts])
  (find-rule (car lhs-pair)))

(define (rule-search filename initTRS)
  (with-input-from-file filename
                  (thunk
                   (let ([patterns (for/list ([e (in-lines)]) e)])
                      (find-rules patterns initTRS)))))

#;(for ([r (rule-search "patterns/2varpatterns.txt" '())])
  (displayln (rule->halide-string r)))

(define 1var-rules
  (list
   (make-rule (halide->termIR "(t0 - t0)") 0)
   (make-rule (halide->termIR "(t0 >= t0)") 'true)
   (make-rule (halide->termIR "(t0 || t0)") 'true)
   (make-rule (halide->termIR "max(t0, t0)") "t0")
   (make-rule (halide->termIR "min(t0, t0)") "t0")))
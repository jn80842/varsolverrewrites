#lang rosette

(require "typed-halide-lang.rkt")
(require "typed-halide-sketch.rkt")
(require "halide-print-sketch.rkt")
(require "halide-parser.rkt")
(require "rule-orders.rkt")
(require "traat/termIR.rkt")
(require "traat/matching.rkt")

(provide verify-RHS-is-target-variable?
         find-target-variable-RHS-rule
         synthesize-fewer-target-variables-rule
         synthesize-nonly-rewrite
         synth-nonly-over-insn-count-range
         synthesize-topn-rewrite
         synth-topn-over-insn-count-range)

(define CURRENT-WIDTH 16)

(define (overflow-bounds width maxdepth)
  (floor (expt (expt 2 (sub1 width)) (/ 1 (expt 2 maxdepth)))))

;; assume LHS is a function that takes the same inputs as the RHS sketch
(define (synthesize-rewrite LHS sk inputs)
  (let* ([evaled-sketch (apply (get-sketch-function sk) inputs)]
         [evaled-LHS (apply LHS inputs)]
         [model (time (synthesize #:forall (symbolics inputs)
                                  #:guarantee (assert (equal? evaled-sketch evaled-LHS))))])
    (if (unsat? model)
        (displayln "Could not find an equivalent RHS")
        (displayln (print-sketch (evaluate sk model))))))

(define (pull-out-target-var var-list target-idx)
  (let ([size (length var-list)])
    (append (list (list-ref var-list target-idx)) (take var-list target-idx) (drop var-list (add1 target-idx)))))

(define (insert-target-var nv-list tarvar target-idx)
  (append (take nv-list target-idx) (list tarvar) (drop nv-list target-idx)))

;; only one synthesis attempt per insn size
;; if we find a solution but it doesn't reduce target variable count, we just move on
(define (synthesize-fewer-target-variables-rule LHS)
  (let* ([LHS-op-count (term-op-count LHS)]
         [LHS-variables (termIR->variables LHS)]
         [sym-variables (map (λ (v) (get-sym-input-int)) LHS-variables)])
    (letrec ([f (λ (insn-count)
                  (if (>= insn-count LHS-op-count)
                      'fail
                      (begin
                        (clear-vc!)
                        (let* ([sk (get-symbolic-sketch operator-list (length LHS-variables) insn-count)]
                               [evaled-sketch (apply (get-sketch-function sk) sym-variables)]
                               [evaled-LHS (apply (termIR->function LHS LHS-variables) sym-variables)]
                               [bound (overflow-bounds CURRENT-WIDTH insn-count)]
                               [model (begin
                                        (for ([v sym-variables])
                                          (assume (bvsle v (bv bound CURRENT-WIDTH)))
                                          (assume (bvsge v (bv (- bound) CURRENT-WIDTH))))
                                        (time (with-handlers ([exn:fail:contract? (λ (e) (displayln (format "Function contract error ~a" (exn-message e))))]
                                                              [exn:fail? (λ (e) (displayln (format "Synthesis error ~a" (exn-message e))))])
                                                (synthesize #:forall sym-variables
                                                            #:guarantee (assert (equal? evaled-sketch evaled-LHS))))))])
                          (if (or (void? model)
                                  (unsat? model)
                                  (unknown? model))
                              (f (add1 insn-count))
                              (let ([candidate-rule (make-rule LHS (halide->termIR (sketch->halide-expr (evaluate sk model) LHS-variables)))])
                                (if (tvar-count-reduction-order? candidate-rule)
                                    candidate-rule
                                    (f (add1 insn-count)))))))))])
      (f 0))))

(define (order-symbolic-arguments term-vars sym-tvars sym-nvars)
  (let ([tvar-idxes (filter (λ (i) (is-tvar-matching? (list-ref term-vars i))) (range (length term-vars)))]
        [nvar-idxes (filter (λ (i) (not (is-tvar-matching? (list-ref term-vars i)))) (range (length term-vars)))])
    (map (λ (i) (if (member i tvar-idxes) (list-ref sym-tvars (index-of tvar-idxes i))
                    (list-ref sym-nvars (index-of nvar-idxes i)))) (range (length term-vars)))))

;; inputs: pattern, target variable idx
;; returns: rule or void
(define (synthesize-topn-rewrite LHS insn-count)
  (begin
    (clear-vc!)
  (let* ([LHS-variables (termIR->variables LHS)]
         [target-variables (filter is-tvar-matching? LHS-variables)]
         [non-tvar-variables (filter (λ (v) (not (is-tvar-matching? v))) LHS-variables)]
         [sk (get-symbolic-sketch operator-list (length non-tvar-variables) insn-count)]
         [bound (overflow-bounds CURRENT-WIDTH (add1 insn-count))])
    (letrec ([f (λ (tvars-pos)
                  (if (empty? tvars-pos)
                      (displayln (format "Could not find t-op-n RHS for ~a with insn count ~a" (termIR->halide LHS) insn-count))
                      (begin
                        (clear-vc!)
                        (define-symbolic* root-op integer?)
                        (let* ([tarvars (for/list ([i (range (length target-variables))]) (get-sym-input-int))]
                               [non-tarvars (for/list ([i (range (length non-tvar-variables))]) (get-sym-input-int))]
                               [evaled-sketch (apply (get-topn-sketch-function sk root-op) (list-ref tarvars (car tvars-pos)) non-tarvars)]
                               [evaled-LHS (apply (termIR->function LHS LHS-variables)
                                                  (order-symbolic-arguments LHS-variables tarvars non-tarvars))]
                               [model (begin
                                        (for ([v (append tarvars non-tarvars)])
                                          (assume (bvsle v (bv bound CURRENT-WIDTH)))
                                          (assume (bvsge v (bv (- bound) CURRENT-WIDTH))))
                                        (time (with-handlers ([exn:fail:contract? (λ (e) (displayln (format "Function contract error ~a" (exn-message e))))]
                                                            [exn:fail? (λ (e) (displayln (format "Synthesis error ~a" (exn-message e))))])
                                              (synthesize #:forall (append tarvars non-tarvars)
                                                          #:guarantee (assert (equal? evaled-sketch evaled-LHS))))))])
            (if (or (unsat? model) (unknown? model) (void? model))
                  (begin
                    (displayln (format "Could not find t-op-n RHS for ~a with insn count ~a" (termIR->halide LHS) insn-count))
                    (f (cdr tvars-pos)))
                  (make-rule LHS (halide->termIR (topn-sketch->halide-expr (evaluate sk model)
                                                                           (evaluate root-op model)
                                                                           (list-ref target-variables (car tvars-pos)) non-tvar-variables))))))))])
      (f (range (length target-variables)))))))

(define (synth-topn-over-insn-count-range LHS insn-count)
  (letrec ([f (λ (i)
                (if (> i insn-count)
                    'fail
                    (let* ([synthed-rule (synthesize-topn-rewrite LHS i)])
                      (if (void? synthed-rule)
                          (f (add1 i))
                          synthed-rule))))])
    (f 0)))

(define (synthesize-nonly-rewrite LHS insn-count)
  (begin (clear-vc!)
  (let* ([LHS-variables (termIR->variables LHS)]
         [target-variables (filter is-tvar-matching? LHS-variables)]
         [non-tvar-variables (filter (λ (v) (not (is-tvar-matching? v))) LHS-variables)]
         [sk (get-symbolic-sketch operator-list (length non-tvar-variables) insn-count)]
         [bound (overflow-bounds CURRENT-WIDTH (add1 insn-count))])
    (if (not (equal? (length target-variables) 1))
        (void)
        (begin
          (clear-vc!)
          (let* ([tarvar (get-sym-input-int)]
                 [non-tarvars (for/list ([i (range (length non-tvar-variables))]) (get-sym-input-int))]
                 [evaled-LHS (apply (termIR->function LHS LHS-variables)
                                    (insert-target-var non-tarvars tarvar (index-of LHS-variables (car target-variables))))]
                 [evaled-sketch (apply (get-sketch-function sk) non-tarvars)]
                 [model (begin
                          (for ([v (cons tarvar non-tarvars)])
                            (assume (bvsle v (bv bound CURRENT-WIDTH)))
                            (assume (bvsge v (bv (- bound) CURRENT-WIDTH))))
                          (time (with-handlers ([exn:fail:contract? (λ (e) (displayln (format "Function contract error ~a" (exn-message e))))]
                                                [exn:fail? (λ (e) (displayln (format "Synthesis error ~a" (exn-message e))))])
                                  (synthesize #:forall (cons tarvar non-tarvars)
                                              #:guarantee (assert (equal? evaled-sketch evaled-LHS)))
                                  )
                              ))])
            (if (void? model)
                (displayln (format "Synthesis threw an error while finding n-only RHS for ~a with insn count ~a" (termIR->halide LHS) insn-count))
              (if (or (unsat? model) (unknown? model))
                  (displayln (format "Could not find n-only RHS for ~a with insn count ~a" (termIR->halide LHS) insn-count))
                  (make-rule LHS (halide->termIR (sketch->halide-expr (evaluate sk model) non-tvar-variables)))))))))))

(define (synth-nonly-over-insn-count-range LHS insn-count)
  (letrec ([f (λ (i)
                (if (> i insn-count)
                    'fail
                    (let ([synthed-rule (synthesize-nonly-rewrite LHS i)])
                      (if (void? synthed-rule)
                          (f (add1 i))
                          synthed-rule))))])
    (f 0)))

(define (verify-RHS-is-target-variable? LHS)
  (let* ([LHS-variables (termIR->variables LHS)]
         [target-variables (filter is-tvar-matching? LHS-variables)]
         [non-tvar-variables (filter (λ (v) (not (is-tvar-matching? v))) LHS-variables)])
    (if (not (equal? (length target-variables) 1))
        #f
        (begin (clear-vc!)
              ; (define-symbolic* tarvar integer?)
               (let* ([tarvar (get-sym-input-int)]
                      [non-tarvars (for/list ([i (range (length non-tvar-variables))]) (get-sym-input-int))]
                      [evaled-LHS (apply (termIR->function LHS LHS-variables)
                                         (insert-target-var non-tarvars tarvar (index-of LHS-variables (car target-variables))))]
                      [cex (verify (assert (equal? evaled-LHS tarvar)))])
                 (unsat? cex))))))

(define (find-target-variable-RHS-rule LHS)
  (let* ([LHS-variables (termIR->variables LHS)]
         [variable-count (length LHS-variables)]
         [sym-variables (map (λ (v) (get-sym-int)) LHS-variables)])
    (letrec ([f (λ (idx)
                  (cond [(>= idx variable-count) 'fail]
                        [(not (is-tvar-matching? (list-ref LHS-variables idx))) (f (add1 idx))]
                        [else (begin
                                (clear-vc!)
                                (let* ([evaled-LHS (apply (termIR->function LHS LHS-variables) sym-variables)]
                                       [cex (verify (assert (equal? evaled-LHS (list-ref sym-variables idx))))])
                                  (if (unsat? cex)
                                      (make-rule LHS (list-ref LHS-variables idx))
                                      (f (add1 idx)))))]))])
      (f 0))))

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
   (make-rule (halide->termIR "(t0 - t0)") 0) ;; not in original rules
   (make-rule (halide->termIR "(t0 >= t0)") 'true) ;; not in original rules
   (make-rule (halide->termIR "(t0 || t0)") "t0")
   (make-rule (halide->termIR "max(t0, t0)") "t0")
   (make-rule (halide->termIR "min(t0, t0)") "t0")))

(define batch1-rules
  (list
   (make-rule (halide->termIR "!((t0 < n0))") (halide->termIR "(t0 >= n0)"))
(make-rule (halide->termIR "!((t0 <= n0))") (halide->termIR "(t0 > n0)"))
(make-rule (halide->termIR "!((t0 == n0))") (halide->termIR "(t0 != n0)"))
(make-rule (halide->termIR "((t0 * n0) * n1)") (halide->termIR "(t0 * (n0 * n1))"))
(make-rule (halide->termIR "((t0 + n0) - n0)") (halide->termIR "t0"))
(make-rule (halide->termIR "max(t0, (t0 + n0))") (halide->termIR "(t0 + max(n0, (n0 - n0)))"))
(make-rule (halide->termIR "min((t0 + n0), t0)") (halide->termIR "(t0 + min((n0 - n0), n0))"))
(make-rule (halide->termIR "(t0 - (t0 + n0))") (halide->termIR "((n0 - n0) - n0)"))
(make-rule (halide->termIR "(t0 - (n0 + t0))") (halide->termIR "((n0 - n0) - n0)"))
(make-rule (halide->termIR "(t0 + (n0 - t0))") (halide->termIR "n0"))
(make-rule (halide->termIR "((t0 + n0) - t0)") (halide->termIR "n0"))
(make-rule (halide->termIR "((t0 - n0) + n0)") (halide->termIR "t0"))
(make-rule (halide->termIR "((t0 + n0) + n1)") (halide->termIR "(t0 + (n1 + n0))"))
(make-rule (halide->termIR "((t0 + n0) < n1)") (halide->termIR "(t0 < (n1 - n0))"))
(make-rule (halide->termIR "((t0 - n0) + n1)") (halide->termIR "(t0 + (n1 - n0))"))
(make-rule (halide->termIR "((t0 - n0) - n1)") (halide->termIR "(t0 - (n1 + n0))"))
(make-rule (halide->termIR "(t0 <= (t0 + n0))") (halide->termIR "(n0 <= (n0 + n0))"))
(make-rule (halide->termIR "((t0 + n0) <= n1)") (halide->termIR "(t0 <= (n1 - n0))"))
(make-rule (halide->termIR "((t0 - n0) <= n1)") (halide->termIR "(t0 <= (n1 + n0))"))
(make-rule (halide->termIR "((t0 + n0) >= n1)") (halide->termIR "(t0 >= (n1 - n0))"))
(make-rule (halide->termIR "((t0 - n0) >= n1)") (halide->termIR "(t0 >= (n1 + n0))"))
(make-rule (halide->termIR "((t0 * n0) + (t0 * n1))") (halide->termIR "(t0 * (n0 + n1))"))
(make-rule (halide->termIR "((t0 || n0) || n1)") (halide->termIR "(t0 || (n1 || n0))"))
))

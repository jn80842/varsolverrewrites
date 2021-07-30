#lang rosette

(require "traat/termIR.rkt")
(require "traat/matching.rkt")
(require "halide-parser.rkt")
(require "varsolverTRS.rkt")
(require "varsolver-synthesis.rkt")

(provide find-all-patterns find-all-subterms synthesize-rule
         synth-rule-from-LHS-pattern saturating-synthesis)

;; find all patterns that can match the full input term
;; NB: we always replace the same expr with the same variable
;; so "((x*y) + (x*y))" will not produce the pattern "v0 + v1" even though it could match it
(define (find-all-patterns term tvar max-size)
  (define counter 0)
  (define expr-to-var (make-hash '()))
  (letrec ([get-fresh-var (λ (e v)
                            (if (hash-has-key? expr-to-var e)
                                (hash-ref expr-to-var e)
                                (begin
                                  (set! counter (add1 counter))
                                  (let ([fresh-var (if (contains-target-variable? e v)
                                                       (format "tvar~a" (sub1 counter))
                                                       (format "nvar~a" (sub1 counter)))])
                                  (hash-set! expr-to-var e fresh-var)
                                  fresh-var))))]
           [outer (λ (t)
                    (cond [(term-variable? t) (list (get-fresh-var t tvar))]
                          [(term-constant? t) (list (get-fresh-var t tvar))] ;; always replace constants with fresh variables
                          [else (cons (get-fresh-var t tvar) (flatten (inner (sigma-term-symbol t) '() (sigma-term-term-list t))))]))]
           [inner (λ (sym args1 args2)
                    (if (empty? args2)
                        (sigma-term sym args1)
                        (let ([arg-versions (outer (car args2))])
                          (map (λ (a) (inner sym (append args1 (list a)) (cdr args2))) arg-versions))))])
    (cap-and-sort-terms-by-size max-size (outer term))))

(define (find-all-subterms term)
  (letrec ([f (λ (tprime)
                (if (or (term-variable? tprime) (term-constant? tprime))
                    '()
                    (cons tprime (flatten (map f (sigma-term-term-list tprime))))))])
    (f term)))

(define (find-all-subterms-for-synthesis term tvar max-size)
  (filter (λ (t) (not (termIR->in-solved-form? t tvar)))
          (cap-and-sort-terms-by-size 100
                                      (find-all-subterms (termIR->replace-constant-variables term)))))

(define (synthesize-rule LHS)
  (let* ([LHS-variables (termIR->variables LHS)]
               [LHS-op-count (term-op-count LHS)]
               [nonly-output (synth-nonly-over-insn-count-range LHS (max 5 (add1 LHS-op-count)))])
          (if (equal? 'fail nonly-output)
              (synth-topn-over-insn-count-range LHS (max 5 LHS-op-count))
              nonly-output)))

;; pattern should use rule var naming conventions distinct from t0/n0/x scheme
(define (synth-rule-from-LHS-pattern pattern TRS blacklist)
  (let* ([normalized-patt (varsolver-rules-rewrite* TRS pattern)]
         [LHS (cdr (rename-to-tarvar-aware-vars normalized-patt (make-hash '()) (list "t" "n" "v")))]
         [LHS-variables (termIR->variables LHS)])
    (let-values ([(target-variables ntvar-variables) (partition is-tvar-matching? LHS-variables)])
      (displayln (format "CANDIDATE LHS ~a" (termIR->halide LHS)))
      (cond [(termIR->rule-in-solved-form? LHS) (displayln (format "~a candidate LHS already in solved form"
                                                                   (termIR->halide LHS)))
                                                'pass]
            [(member-mod-alpha-renaming? LHS blacklist) (begin
                                                          (displayln (format "LHS ON BLACKLIST: ~a" (termIR->halide LHS)))
                                                          'pass)]
            [(and (equal? (length target-variables) 1)
                  (verify-RHS-is-target-variable? LHS)) (make-rule LHS (list-ref target-variables 0))]
            [else (let ([output (synthesize-rule LHS)])
                    (if (equal? 'fail output)
                        LHS
                        output))]))))

(define (termIR->rule-style-varnames term)
  (cdr (rename-to-tarvar-aware-vars term (make-hash '()) (list "t" "n" "v"))))

;; saturating synthesis algo
;; parameters:
;; top size of LHS term (15)
;; max insn count for n-only (op count of LHS + 1) and t-op-n sketches (op count of LHS)
(define (saturating-synthesis input tvar TRS blacklist)
  (letrec ([f (λ (input subtrees patterns TRS blacklist)
                                 (cond [(and (empty? subtrees)
                                             (empty? patterns)) (begin
                                                                  (displayln "CHECKED ALL SUBTREES/PATTERNS")
                                                                  (cons TRS blacklist))]
                                       [(empty? patterns) (begin
                                                            (displayln (format "SUBTERM ~a:" (termIR->halide (car subtrees))))
                                                            (f input (cdr subtrees) (find-all-patterns (car subtrees) tvar 15) TRS blacklist))]
                                       [(termIR->rule-in-solved-form? (car patterns)) (begin
                                                                                        (displayln (format "~a candidate LHS already in solved form" (termIR->halide (car patterns))))
                                                                                        (f input subtrees (cdr patterns) TRS blacklist))]
                                       [(member-mod-alpha-renaming? (car patterns) blacklist) (begin
                                                                                                (displayln (format "LHS ON BLACKLIST: ~a" (termIR->halide (car patterns))))
                                                                                                (f input subtrees (cdr patterns) TRS blacklist))]
                                       [(not (termIR->typechecks? (car patterns))) (begin
                                                                                     (displayln (format "PATTERN ~a DOES NOT TYPECHECK; ADDING TO BLACKLIST" (termIR->halide (car patterns))))
                                                                                     (f input subtrees (cdr patterns) TRS (cons (termIR->rule-style-varnames (car patterns)) blacklist)))]
                                       [else   (displayln (format "Synthesize RHS for candidate LHS ~a?" (termIR->halide (termIR->rule-style-varnames (car patterns)))))
                                               (let ([cmd-input (read)])
                                                 (if (equal? 'stop cmd-input)
                                                     (cons TRS (cons (termIR->rule-style-varnames (car patterns)) blacklist))
                                                     (if (not (equal? 'y cmd-input))
                                                         #;(begin (displayln "Skipping synthesis")
                                                                (f input subtrees (cdr patterns) TRS (cons (termIR->rule-style-varnames (car patterns)) blacklist)))
                                                         (let ([result (synth-rule-from-LHS-pattern (car patterns) TRS blacklist)])
                                                           (if (equal? 'pass result)
                                                               (f input subtrees (cdr patterns) TRS blacklist)
                                                               (if (sigma-term? result)
                                                                   (begin
                                                                     (displayln (format "ADDING ~a TO BLACKLIST" (termIR->halide result)))
                                                                     (f input subtrees (cdr patterns) TRS (cons result blacklist)))
                                                                   (let* ([updated-TRS (append TRS (list result))]
                                                                          [normed-input (varsolver-rewrite* tvar updated-TRS input)])
                                                                     (begin
                                                                       (displayln (format "LEARNED NEW RULE ~a --> ~a" (termIR->halide (rule-lhs result))
                                                                                          (termIR->halide (rule-rhs result))))
                                                                       (if (termIR->in-solved-form? normed-input tvar)
                                                                           (begin
                                                                             (displayln (format "INPUT ~a NOW SOLVED TO ~a" (termIR->halide input) (termIR->halide normed-input)))
                                                                             (cons updated-TRS blacklist))
                                                                           (begin
                                                                             (displayln (format "NOW SOLVING FOR ~a" (termIR->halide normed-input)))
                                                                             (f normed-input (find-all-subterms-for-synthesis normed-input tvar 15)
                                                                                '() updated-TRS blacklist)))))))))))
                                             ]))])
    (let ([normed-init-input (varsolver-rewrite* tvar TRS input)])
      (if (termIR->in-solved-form? normed-init-input tvar)
          (displayln "Initial input is solved by existing TRS")
          (f normed-init-input (find-all-subterms-for-synthesis normed-init-input tvar 15) '() TRS blacklist)))))


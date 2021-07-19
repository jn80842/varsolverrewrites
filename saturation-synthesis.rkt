#lang rosette

(require "traat/termIR.rkt")
(require "traat/matching.rkt")
(require "halide-parser.rkt")
(require "varsolverTRS.rkt")
(require "varsolver-synthesis.rkt")

(provide find-all-patterns)

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
       ;   (cap-and-sort-terms-by-size max-size
                                      (find-all-subterms (termIR->replace-constant-variables term))))
  ;)

(define (synthesize-rule LHS)
  (let* ([LHS-variables (termIR->variables LHS)]
         [nonly-output (synth-nonly-over-insn-count-range LHS 5)])
    (if (equal? 'fail nonly-output)
        (synth-topn-over-insn-count-range LHS 4)
        nonly-output)))

;; pattern should use rule var naming conventions distinct from t0/n0/x scheme
(define (synth-rule-from-LHS-pattern pattern TRS blacklist)
  (let* ([normalized-patt (varsolver-rules-rewrite* TRS pattern)]
         [LHS (cdr (rename-to-tarvar-aware-vars normalized-patt (make-hash '()) (list "t" "n" "v")))]
         [LHS-variables (termIR->variables LHS)])
    (let-values ([(target-variables ntvar-variables) (partition is-tvar-matching? LHS-variables)])
      (cond [(termIR->rule-in-solved-form? LHS) (displayln (format "~a candidate LHS already in solved form"
                                                                   (termIR->halide LHS)))
                                                'pass]
            [(member-mod-alpha-renaming? LHS blacklist) 'pass]
            [(and (equal? (length target-variables) 1)
                  (verify-RHS-is-target-variable? LHS)) (make-rule LHS (list-ref target-variables 0))]
            [else (let ([output (synthesize-rule LHS)])
                    (if (equal? 'fail output)
                        LHS
                        output))]))))

;; saturating synthesis algo
;; parameters:
;; top size of LHS term (15)
;; max insn count for n-only and t-op-n sketches (5, 5)
#;(define (saturating-synthesis input tvar TRS blacklist)
  (let ([normalized-input (varsolver-rewrite* tvar TRS input)])
    (if (termIR->in-solved-form? normalized-input tvar)
        (displayln (format "~a input rewrites to solved form ~a" (termIR->halide input) (termIR->halide normalized-input)))
        (let ([candidate-subtrees (filter (λ (t) (not (termIR->in-solved-form? t tvar)))
                                          (cap-and-sort-terms-by-size 15
                                                                      (find-all-subterms (termIR->replace-constant-variables normalized-input))))])
          (for ([candidate-LHS-pattern candidate-subtrees])
            (for ([candidate-LHS (find-all-patterns candidate-LHS-pattern "x")])
              (let ([normalized-LHS (varsolver-rules-rewrite* TRS candidate-LHS)])
                (if (termIR->rule-in-solved-form? normalized-LHS)
                    (displayln (format "~a candidate LHS already in solved form" (termIR->halide normalized-LHS)))
                    (if (member-mod-alpha-renaming? normalized-LHS blacklist)
                        (displayln (format "~a is on the blacklist" (termIR->halide normalized-LHS)))
                        (displayln (format "~a is a valid LHS candidate" (termIR->halide normalized-LHS))))))))))))

(define (saturating-synthesis input tvar TRS blacklist)
  (letrec ([f (λ (input subtrees patterns TRS blacklist)
                                 (cond [(empty? subtrees) (begin
                                                            (displayln "CHECKED ALL SUBTREES/PATTERNS")
                                                            (cons TRS blacklist))]
                                       [(empty? patterns) (begin
                                                            (displayln (format "SUBTERM ~a:" (termIR->halide (car subtrees))))
                                                            (f input (cdr subtrees) (find-all-patterns (car subtrees) tvar 15) TRS blacklist))]
                                       [else (let ([result (synth-rule-from-LHS-pattern (car patterns) TRS blacklist)])
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
                                                                 (displayln (format "INPUT ~a NOW SOLVED TO ~a" input normed-input))
                                                                 (cons updated-TRS blacklist))
                                                               (begin
                                                                 (displayln (format "NOW SOLVING FOR ~a" normed-input))
                                                                 (f normed-input (find-all-subterms-for-synthesis normed-input tvar 15)
                                                                    '() updated-TRS blacklist))))))))]))])
    (let ([normed-init-input (varsolver-rewrite* tvar TRS input)])
      (if (termIR->in-solved-form? normed-init-input tvar)
          (displayln "Initial input is solved by existing TRS")
          (f normed-init-input (find-all-subterms-for-synthesis normed-init-input tvar 15) '() TRS blacklist)))))

(define input1 (sigma-term '+ (list (sigma-term '- (list "w" "y")) "x")))
(define input2-halide "(((((-272 - ((min(y*256, z + -256) + w) % 8))/8) + (u*16)) + x) <= 2)")
(define input3-halide "(((((((((((max(y, 0) + max(z, 0)) + 159)/60)*15) + ((((min(x*160, w + -160) + (u + (0 - max(y, 0)))) + 61)/4) + ((9 - (min(x*160, w + -160) + (u + (0 - max(y, 0)))))/4))) + 1)/4)*4) + max(((v + 9)/4) + ((((max(y, 0) + max(z, 0)) + 159)/60)*15), ((v + -6)/4) + ((((max(y, 0) + max(z, 0)) + 159)/60)*15))) + 4) <= (n5/4))")
(define normalized-input3 (varsolver-rewrite* "x" originalvarsolverTRS (halide->termIR input3-halide)))
(define input3-synth-candidate-subtrees (map termIR->halide (filter (λ (t) (not (termIR->in-solved-form? t "x")))
                                                (cap-and-sort-terms-by-size 15 (find-all-subterms (termIR->replace-constant-variables normalized-input3))))))

(define input4-halide "(((((((((max(max(max(y, max(0 - y, y)), 0) + max(y, 0), 0) + 31)/25)*25) + (((((((z + min(x*160, w + -160)) + min(max(0 - min(y, 0), y), 0 - max(y, 0))) + -6)/4)*-4) + max(max(0 - min(y, 0), y), 0 - max(y, 0))) + (z + min(x*160, w + -160)))) + 30)/20)*5) + (((max(((max(max(max(y, max(0 - y, y)), 0) + max(y, 0), 0) + 31)/25)*25, (((max(max(max(y, max(0 - y, y)), 0) + max(y, 0), 0) + 31)/25)*25) + 15) + u) + -6)/4)) <= ((v/4) - 1))")
;;(saturating-synthesis (halide->termIR input4-halide) "x" originalvarsolverTRS '())
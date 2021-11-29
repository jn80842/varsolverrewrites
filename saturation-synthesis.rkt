#lang rosette

(require "traat/termIR.rkt")
(require "traat/matching.rkt")
(require "halide-parser.rkt")
(require "rule-orders.rkt")
(require "varsolver-synthesis.rkt")
(require "typed-halide-sketch.rkt")
(require "typed-fixed-sketch-synthesis.rkt")

(provide (all-defined-out))

#;(provide find-all-patterns find-all-subterms synthesize-rule
         synth-rule-from-LHS-pattern termIR->rule-style-varnames)

;; find all patterns that can match the full input term
;; NB: we always replace the same expr with the same variable
;; so "((x*y) + (x*y))" will not produce the pattern "v0 + v1" even though it could match it
(define (find-all-patterns term tvar max-size)
  (find-all-patterns-list (filter (λ (t) (< (term-size t) max-size)) (find-all-subterms term)) tvar))

(define (find-all-patterns-list term-list tvar)
  (define t-counter 0)
  (define n-counter 0)
  (define expr-to-var (make-hash '()))
  (letrec ([get-fresh-var (λ (e v)
                            (if (hash-has-key? expr-to-var e)
                                (hash-ref expr-to-var e)
                                (begin
                                  (let ([fresh-var (if (contains-target-variable? e v)
                                                       (begin (set! t-counter (add1 t-counter))
                                                              (format "t~a" (sub1 t-counter)))
                                                       (begin (set! n-counter (add1 n-counter))
                                                              (format "n~a" (sub1 n-counter))))])
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
    (filter (λ (t) (not (term-variable? t))) (flatten (map outer term-list)))))

(define (find-all-patterns-to-match-term term tvar)
  (find-all-patterns-list (list term) tvar))

(define (find-all-subterms-bottom-up term)
  (letrec ([f (λ (tprime)
                (if (or (term-variable? tprime) (term-constant? tprime))
                    '()
                    (append (flatten (map f (sigma-term-term-list tprime))) (list tprime))))])
    (flatten (f term))))

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
               [nonly-output (synth-nonly-over-insn-count-range LHS (max 3 (add1 LHS-op-count)))])
          (if (equal? 'fail nonly-output)
               (synthesize-from-fixed-metasketches LHS) ;(synth-topn-over-insn-count-range LHS (max 3 LHS-op-count))
              nonly-output)))

;; pattern should use rule var naming conventions distinct from t0/n0/x scheme
(define (synth-rule-from-LHS-pattern pattern TRS blacklist)
  (let* ([normalized-patt (varsolver-rules-rewrite* TRS pattern)]
         [LHS (cdr (rename-to-tarvar-aware-vars normalized-patt (make-hash '()) (list "t" "n" "v")))]
         [LHS-variables (termIR->variables LHS)])
    (let-values ([(target-variables ntvar-variables) (partition is-tvar-matching? LHS-variables)])
      (displayln (format "CANDIDATE LHS ~a" (termIR->halide LHS)))
      (cond [(> (length target-variables) 1) (displayln "Fixed sketch synthesis not implemented for multiple target variables")
                                             'pass]
            [(termIR->rule-in-solved-form? LHS) (displayln (format "~a candidate LHS already in solved form"
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

(define (termIR->rule-style-varnames term [var-hash (make-hash '())])
  (cdr (rename-to-tarvar-aware-vars term var-hash (list "t" "n" "v"))))

;;;; helpers
(define (terms->varsolver-reduction-order? target-variable t1 t2)
  (let ([renamed-t1 (cdr (rename-to-tarvar-aware-vars t1 (make-hash (list (cons target-variable "tvro")))))]
        [renamed-t2 (cdr (rename-to-tarvar-aware-vars t2 (make-hash (list (cons target-variable "tvro")))))])
    (varsolver-reduction-order? (rule renamed-t1 renamed-t2 ""))))

(define (wrong-direction-rewrites TRS1 TRS2 terms)
  (filter (λ (t) (terms->varsolver-reduction-order? "x"
                                                         (varsolver-rewrite* "x" TRS2 t)
                                                         (varsolver-rewrite* "x" TRS1 t))) terms))

(define (add-to-end l e)
  (append l (list e)))

(define (pattern->in-solved-form? patt)
  (let ([target-vars (filter is-tvar-matching? (termIR->variables patt))])
    (and (equal? (length target-vars) 1)
         (termIR->in-solved-form? patt (car target-vars)))))

(define (synthesis-iteration input current-TRS current-blacklist)
  (let ([normed-input (varsolver-rewrite* "x" current-TRS input)])
    (displayln (format "INPUT EXPR: ~a" (termIR->halide input)))
    (if (termIR->in-solved-form? normed-input "x")
        (begin
          (displayln (format "NORMALIZED INPUT ~a IN SOLVED FORM" (termIR->halide normed-input)))
          (list current-TRS current-blacklist))
        (let ([patterns (find-all-patterns normed-input "x" 15)])
          (displayln (format "Found ~a candidate LHS patterns" (length patterns)))
          (letrec ([f (λ (patts TRS blacklist)
                        (cond [(empty? patts) (begin
                                                (displayln "COULD NOT FIND RULE FOR INPUT")
                                                (list TRS blacklist))]
                              [(termIR->contains-div-mod? (car patts)) (begin
                                                                         (displayln (format "Pattern ~a contains div or mod so is being skipped" (termIR->halide (car patts))))
                                                                         (f (cdr patts) TRS blacklist))]
                              [(not (termIR->typecheck? (car patts))) (begin
                                                                        (displayln (format "Pattern ~a did not typecheck" (termIR->halide (car patts))))
                                                                        (f (cdr patts) TRS blacklist))]
                              [(pattern->in-solved-form? (car patts)) (begin
                                                                        (displayln (format "Pattern ~a in solved form" (termIR->halide (car patts))))
                                                                        (f (cdr patts) TRS blacklist))]
                              [(member-mod-alpha-renaming? (car patts) blacklist) (begin
                                                                                    (displayln (format "Pattern ~a on blacklist" (termIR->halide (car patts))))
                                                                                    (f (cdr patts) TRS blacklist))]
                              [else (let ([synth-output (synth-rule-from-LHS-pattern (car patts) TRS blacklist)])
                                      (cond [(rule? synth-output) (begin
                                                                    (displayln (format "NEW RULE FOUND: ~a" (rule->halide-string synth-output)))
                                                                    ;(list (append TRS (list synth-output)) blacklist updated-regression))]
                                                                    (f patts (add-to-end TRS synth-output) blacklist))]
                                            [(sigma-term? synth-output) (begin
                                                                          (displayln (format "Could not find valid RHS for LHS ~a" (termIR->halide (car patts))))
                                                                          (f (cdr patts) TRS (append blacklist (list synth-output))))]
                                            [else (begin
                                                    (displayln (format "Pattern ~a failed, continuing" (termIR->halide (car patts))))
                                                    (f (cdr patts) TRS blacklist))]))]))])
            (f patterns current-TRS current-blacklist))))))

(define (recursive-synthesis inputs input-TRS input-blacklist [input-regression '()])
  (letrec ([f (λ (exprs TRS blacklist)
              (if (empty? exprs)
                  (list TRS blacklist)
                  (let ([t-b-r (synthesis-iteration (car exprs) TRS blacklist)])
                    (f (cdr exprs) (first t-b-r) (second t-b-r)))))])
    (f inputs input-TRS input-blacklist)))

;;;; regression tests
(define (regression benchmarks TRS1 TRS2)
  (letrec ([f (λ (exprs solved1 solved2 better worse)
            (if (empty? exprs)
                (list solved1 solved2 better worse)
                (let* ([normed1 (varsolver-rewrite* "x" TRS1 (car exprs))]
                       [normed2 (varsolver-rewrite* "x" TRS2 (car exprs))]
                       [updated-solved1 (+ solved1
                                           (if (termIR->in-solved-form? normed1 "x") 1 0))]
                       [updated-solved2 (+ solved2
                                           (if (termIR->in-solved-form? normed2 "x") 1 0))]
                       [updated-better (+ better (if (terms->varsolver-reduction-order? "x" normed1 normed2) 1 0))]
                       [updated-worse (+ worse (if (terms->varsolver-reduction-order? "x" normed2 normed1)
                                                   (begin
                                                     (displayln (format "WORSE ~a" (termIR->halide (car exprs)))) 1) 0))])
                  (f (cdr exprs) updated-solved1 updated-solved2 updated-better updated-worse))))])
    (let ([output (f benchmarks 0 0 0 0)])
      (begin
        (displayln (format "Prior TRS solved ~a benchmarks, current TRS solved ~a benchmarks of ~a"
                           (first output) (second output) (length benchmarks)))
        (displayln (format "Change to TRS moved ~a benchmarks in the right direction, ~a in the wrong direction"
                           (third output) (fourth output)))))))

;; assumes all rules have unique names
(define (find-changed-rewrite-paths benchmarks TRS1 TRS2)
  (let ([f (λ (expr)
             (let ([rewritten1-output (varsolver-logging-rewrite* "x" TRS1 expr)]
                   [rewritten2-output (varsolver-logging-rewrite* "x" TRS2 expr)])
               (list (termIR->halide expr) (second rewritten1-output) (second rewritten2-output))))])
    (filter (λ (histories) (not (equal? (second histories) (third histories)))) (map f benchmarks))))
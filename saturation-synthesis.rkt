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

(define (string->rule rule-string)
  (let ([rule-list (string-split rule-string "REWRITES")])
    (make-rule (halide->termIR (first rule-list)) (halide->termIR (second rule-list)))))
(define (rule->string r)
  (format "~aREWRITES~a" (termIR->halide (rule-lhs r)) (termIR->halide (rule-rhs r))))

;; find all patterns that can match the full input term
;; NB: we always replace the same expr with the same variable
;; so "((x*y) + (x*y))" will not produce the pattern "v0 + v1" even though it could match it
(define (find-all-patterns term tvar [max-size 100])
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
    (filter (λ (t) (not (term-variable? t))) (outer term))))

(define (find-all-patterns-to-match-term term tvar [max-size 15])
  (filter (λ (t) (< (term-size t) max-size)) (remove-duplicates (find-all-patterns term tvar))))

(define (find-all-subterms-bottom-up term tvar [max-size 15])
  (letrec ([f (λ (tprime)
                (if (or (term-variable? tprime) (term-constant? tprime))
                    '()
                    (append (flatten (map f (sigma-term-term-list tprime))) (list tprime))))])
    (filter
     (λ (t) (and (< (term-size t) max-size) (contains-target-variable? t tvar)))
     (remove-duplicates (flatten (f term))))))

(define (find-all-subterms term)
  (letrec ([f (λ (tprime)
                (if (or (term-variable? tprime) (term-constant? tprime))
                    '()
                    (cons tprime (flatten (map f (sigma-term-term-list tprime))))))])
    (f term)))

;; fully solved form order
;; we look for
;; RHS that is exactly 1 target variable
;; RHS that contains no target variables
;; RHS in t * e form
;; else return 'fail
(define (synthesize-rule LHS)
  (let ([target-variable-RHS-output (find-target-variable-RHS-rule LHS)])
    (if (rule? target-variable-RHS-output)
        (begin
          (displayln (format "Found target-variable only rule: ~a" (rule->halide-string target-variable-RHS-output)))
          target-variable-RHS-output)
        (let* ([LHS-op-count (term-op-count LHS)]
               [nonly-output (synth-nonly-over-insn-count-range LHS (max 3 (add1 LHS-op-count)))])
          (if (rule? nonly-output)
              (begin
                (displayln (format "Found non-target-variable only rule: ~a" (rule->halide-string nonly-output)))
                nonly-output)
              (let ([topn-output (synth-topn-over-insn-count-range LHS (max 3 LHS-op-count))])
                (if (rule? topn-output)
                    (begin
                      (displayln (format "Found t * e style rule: ~a" (rule->halide-string topn-output)))
                      topn-output)
                    topn-output)))))))

;; gradual order
;; we look for
;; RHS that is exactly 1 target variable
;; RHS that contains no target variables
;; RHS in t * e form
;; RHS with fewer target variable instances
;; RHS moving target variables left or up
;; else return 'fail
(define (synthesize-gradual-rule LHS)
  (displayln "using gradual version")
  (let ([target-variable-RHS-output (find-target-variable-RHS-rule LHS)])
    (if (rule? target-variable-RHS-output)
        (begin
          (displayln (format "Found target-variable only rule: ~a" (rule->halide-string target-variable-RHS-output)))
          target-variable-RHS-output)
        (let* ([LHS-op-count (term-op-count LHS)]
               [nonly-output (synth-nonly-over-insn-count-range LHS (max 3 (add1 LHS-op-count)))])
          (if (rule? nonly-output)
              (begin
                (displayln (format "Found non-target-variable only rule: ~a" (rule->halide-string nonly-output)))
                nonly-output)
              (let ([topn-output (synth-topn-over-insn-count-range LHS (max 3 LHS-op-count))])
                (if (rule? topn-output)
                    (begin
                      (displayln (format "Found t * e style rule: ~a" (rule->halide-string topn-output)))
                      topn-output)
                    (let ([fewer-target-variables-output (synthesize-fewer-target-variables-rule LHS)])
                      (if (rule? fewer-target-variables-output)
                          fewer-target-variables-output
                          (synthesize-from-fixed-metasketches LHS))))))))))

;; pattern should use rule var naming conventions distinct from t0/n0/x scheme
(define (synth-rule-from-LHS-pattern pattern [synth-func synthesize-rule])
  (let* ([LHS (cdr (rename-to-tarvar-aware-vars pattern (make-hash '()) (list "t" "n" "v")))]
         [output (synth-func LHS)])
    (if (equal? 'fail output)
        LHS
        output)))

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

(define (pattern->in-solved-form? patt)
  (let ([target-vars (filter is-tvar-matching? (termIR->variables patt))])
    (and (equal? (length target-vars) 1)
         (termIR->in-solved-form? patt (car target-vars)))))

(define (synthesis-iteration input current-TRS current-blacklist [synth-func synthesize-rule])
  (let ([normed-input (varsolver-rewrite* "x" current-TRS input)])
    (displayln (format "INPUT EXPR: ~a" (termIR->halide input)))
    (if (termIR->in-solved-form? normed-input "x")
        (begin
          (displayln (format "NORMALIZED INPUT ~a IN SOLVED FORM" (termIR->halide normed-input)))
          (list current-TRS current-blacklist))
        (letrec ([patternsλ (λ (patts TRS blacklist)
                             (cond [(empty? patts) (list TRS blacklist)]
                               ;    [(termIR->contains-div-mod? (car patts)) (begin
                               ;                                               (displayln (format "Pattern ~a contains div or mod so is being skipped" (termIR->halide (car patts))))
                               ;                                               (patternsλ (cdr patts) TRS blacklist))]
                                   [(not (termIR->typecheck? (car patts))) (begin
                                                                             (displayln (format "Pattern ~a did not typecheck" (termIR->halide (car patts))))
                                                                             (patternsλ (cdr patts) TRS blacklist))]
                                   [(pattern->in-solved-form? (car patts)) (begin
                                                                             (displayln (format "Pattern ~a in solved form" (termIR->halide (car patts))))
                                                                             (patternsλ (cdr patts) TRS blacklist))]
                                   [(member-mod-alpha-renaming? (car patts) blacklist) (begin
                                                                                         (displayln (format "Pattern ~a on blacklist" (termIR->halide (car patts))))
                                                                                         (patternsλ (cdr patts) TRS blacklist))]
                                   [else (let ([synth-output (synth-rule-from-LHS-pattern (car patts) synth-func)])
                                           (cond [(rule? synth-output) (begin
                                                                         (displayln (format "NEW RULE FOUND: ~a" (rule->halide-string synth-output)))
                                                                         (list (append TRS (list synth-output)) blacklist))]
                                                 [(sigma-term? synth-output) (begin
                                                                               (displayln (format "Could not find valid RHS for LHS ~a" (termIR->halide (car patts))))
                                                                               (patternsλ (cdr patts) TRS (append blacklist (list synth-output))))]
                                                 [else (begin
                                                         (displayln (format "Pattern ~a failed, continuing" (termIR->halide (car patts))))
                                                         (patternsλ (cdr patts) TRS blacklist))]))]))]
                 [subtermsλ (λ (subterms TRS blacklist)
                              (if (empty? subterms)
                                  (begin
                                    (displayln (format "COULD NOT LEARN RULE FROM INPUT ~a" (termIR->halide (varsolver-rewrite* "x" current-TRS input))))
                                    (list TRS blacklist))
                                  (let* ([pattern-result (patternsλ (find-all-patterns-to-match-term (car subterms) "x") TRS blacklist)]
                                         [new-TRS (first pattern-result)]
                                         [new-blacklist (second pattern-result)])
                                    (if (> (length new-TRS) (length TRS)) ;; we learned a rule
                                        (let ([new-normed-input (varsolver-rewrite* "x" new-TRS input)])
                                          (if (termIR->in-solved-form? new-normed-input "x")
                                              (begin
                                                (displayln (format "NORMALIZED INPUT ~a NOW IN SOLVED FORM" (termIR->halide new-normed-input)))
                                                (list new-TRS new-blacklist))
                                              (subtermsλ (find-all-subterms-bottom-up new-normed-input "x" 100) new-TRS new-blacklist)))
                                        (subtermsλ (cdr subterms) new-TRS new-blacklist)))))])
          (subtermsλ (find-all-subterms-bottom-up normed-input "x" 100) current-TRS current-blacklist)))))

(define (saturation-synthesis inputs input-TRS input-blacklist [synth-func synthesize-rule])
  (letrec ([f (λ (exprs TRS blacklist)
              (if (empty? exprs)
                  (list TRS blacklist)
                  (let ([t-b-r (synthesis-iteration (car exprs) TRS blacklist synth-func)])
                    (f (cdr exprs) (first t-b-r) (second t-b-r)))))])
    (f inputs input-TRS input-blacklist)))

;;;; regression tests
(define (regression benchmarks TRS1 TRS2)
  (letrec ([f (λ (exprs solved1 solved2 solved-by-either better worse)
            (if (empty? exprs)
                (list solved1 solved2 solved-by-either better worse)
                (let* ([normed1 (varsolver-rewrite* "x" TRS1 (car exprs))]
                       [normed2 (varsolver-rewrite* "x" TRS2 (car exprs))]
                       [updated-solved1 (+ solved1
                                           (if (termIR->in-solved-form? normed1 "x")
                                               (begin
                                              ;   (unless (termIR->in-solved-form? normed2 "x") (displayln (format "SOLVED BY PRIOR NOT BY CURRENT: ~a" (termIR->halide (car exprs)))))
                                               1) 0))]
                       [updated-solved2 (+ solved2
                                           (if (termIR->in-solved-form? normed2 "x")
                                               (begin
                                              ;   (unless (termIR->in-solved-form? normed1 "x") (displayln (format "SOLVED BY CURRENT NOT BY PRIOR: ~a" (termIR->halide (car exprs)))))
                                               1) 0))]
                       [updated-solved-by-either (+ solved-by-either
                                                    (if (or (termIR->in-solved-form? normed1 "x") (termIR->in-solved-form? normed2 "x")) 1 0))]
                       [updated-better (+ better (if (terms->varsolver-reduction-order? "x" normed1 normed2)
                                                     (begin
                                                      ; (displayln (format "BETTER ~a" (termIR->halide (car exprs))))
                                                     1) 0))]
                       [updated-worse (+ worse (if (terms->varsolver-reduction-order? "x" normed2 normed1)
                                                   (begin
                                                     ;(displayln (format "WORSE ~a" (termIR->halide (car exprs))))
                                                     1) 0))])
                  (f (cdr exprs) updated-solved1 updated-solved2 updated-solved-by-either updated-better updated-worse))))])
    (let ([output (f benchmarks 0 0 0 0 0)])
      (begin
        (displayln (format "Prior TRS solved ~a benchmarks, current TRS solved ~a benchmarks of ~a"
                           (first output) (second output) (length benchmarks)))
        (displayln (format "~a benchmarks were solved by at least one of the TRSs" (third output)))
        (displayln (format "Change to TRS moved ~a benchmarks in the right direction, ~a in the wrong direction"
                           (fourth output) (fifth output)))))))

;; assumes all rules have unique names
(define (find-changed-rewrite-paths benchmarks TRS1 TRS2)
  (let ([f (λ (expr)
             (let ([rewritten1-output (varsolver-logging-rewrite* "x" TRS1 expr)]
                   [rewritten2-output (varsolver-logging-rewrite* "x" TRS2 expr)])
               (list (termIR->halide expr) (second rewritten1-output) (second rewritten2-output))))])
    (filter (λ (histories) (not (equal? (second histories) (third histories)))) (map f benchmarks))))

(define test-expr (halide->termIR "(max((((min((((x * c0) + y) - max(z, c1)), min((((x * c0) + y) + w), (((x * c0) + y) - max(z, c1)))) + c2) / c3) + ((((((min((y + w), (y - max(z, c1))) + c4) / c5) + ((u / c3) * c3)) - ((min((y + w), (y - max(z, c1))) + c6) / c5)) / c3) * c5)), (((min((((x * c0) + y) - max(z, c1)), min((((x * c0) + y) + w), (((x * c0) + y) - max(z, c1)))) + c7) / c3) + (((((((min((y + w), (y - max(z, c1))) + c4) / c3) + ((u / c3) * c5)) - ((min((y + w), (y - max(z, c1))) + c8) / c3)) + c9) / c3) * c3))) - ((min((((x * c0) + y) - max(z, c1)), min((((x * c0) + y) + w), (((x * c0) + y) - max(z, c1)))) + c8) / c3))"))
(define test-expr2 (halide->termIR "(((((((((y * c0) - (((((y * c0) + z) + c1) / c2) * c2)) + (w + z)) + c1) / c2) - x) * c3) - u) + (((x * c3) + u) + ((v * c4) + v5)))"))


(define input1 (sigma-term
 '<=
 (list
  (sigma-term '+ (list (sigma-term '* '("x" "c0")) "c1"))
  (sigma-term '/ (list (sigma-term 'min '("v1" "c2")) "c3")))))
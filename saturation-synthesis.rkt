#lang rosette

(require "traat/termIR.rkt")
(require "traat/matching.rkt")
(require "halide-parser.rkt")
(require "varsolverTRS.rkt")

(provide find-all-patterns)

;; find all patterns that can match the full input term
;; NB: we always replace the same expr with the same variable
;; so "((x*y) + (x*y))" will not produce the pattern "v0 + v1" even though it could match it
(define (find-all-patterns term tvar)
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
    (outer term)))

(define (find-all-subterms term)
  (letrec ([f (λ (tprime)
                (if (or (term-variable? tprime) (term-constant? tprime))
                    '()
                    (cons tprime (flatten (map f (sigma-term-term-list tprime))))))])
    (f term)))

;; saturating synthesis algo
(define (saturating-synthesis input tvar TRS blacklist)
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

(define input1 (sigma-term '+ (list (sigma-term '- (list "w" "y")) "x")))
(define input2-halide "(((((-272 - ((min(y*256, z + -256) + w) % 8))/8) + (u*16)) + x) <= 2)")
(define input3-halide "(((((((((((max(y, 0) + max(z, 0)) + 159)/60)*15) + ((((min(x*160, w + -160) + (u + (0 - max(y, 0)))) + 61)/4) + ((9 - (min(x*160, w + -160) + (u + (0 - max(y, 0)))))/4))) + 1)/4)*4) + max(((v + 9)/4) + ((((max(y, 0) + max(z, 0)) + 159)/60)*15), ((v + -6)/4) + ((((max(y, 0) + max(z, 0)) + 159)/60)*15))) + 4) <= (n5/4))")
(define normalized-input3 (varsolver-rewrite* "x" originalvarsolverTRS (halide->termIR input3-halide)))
(define input3-synth-candidate-subtrees (map termIR->halide (filter (λ (t) (not (termIR->in-solved-form? t "x")))
                                                (cap-and-sort-terms-by-size 15 (find-all-subterms (termIR->replace-constant-variables normalized-input3))))))
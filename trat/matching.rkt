#lang racket

(require "termIR.rkt")

(provide match varsolver-match rewrite rewrite*)



;; terms are vname/variables, integers, or sigma-terms
;; let's make variables strings for now

;; subst is a hash-table mapping variables to terms
(struct subst (mapping) #:transparent)

;; indom: vname -> subst -> bool, returns true if variable has a mapping in subst
(define (indom v sub)
  (hash-has-key? sub v))

;; app: subst -> vname -> term
(define (app sub v)
  (hash-ref sub v))

;; lift: subst -> term -> term
(define (lift sub t)
  (letrec ([f (λ (t1)
                (cond [(string? t1) (if (indom t1 sub) (app sub t1) t1)]
                      [(term-constant? t1) t1]
                      [else (sigma-term (sigma-term-symbol t1) (map f (sigma-term-term-list t1)))]))])
    (f t)))

;; occurs: vname -> term -> bool
(define (occurs var t)
  (letrec ([f (λ (t1)
                (cond [(string? t1) (equal? var t1)]
                      [(term-constant? t1) #f]
                      [else (ormap f (sigma-term-term-list t1))]))])
    (f t)))

;; match: term -> term -> subst
;; given a pattern (LHS) and object (input term), find a substitution that will match one to the other
;; instead of throwing an exception, return 'fail symbol
(define (match patt obj)
  (letrec ([matches (λ (eq-set sub)
                      (if (empty? eq-set) sub
                          (let ([curr-eq (first eq-set)]
                                [ret-eq-set (cdr eq-set)])
                      (cond [(and (term-constant? (car curr-eq))
                                  (term-constant? (cdr curr-eq))
                                  (equal? (car curr-eq) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (string? (car curr-eq))
                                  (indom (car curr-eq) sub)
                                  (equal? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (string? (car curr-eq)) (not (indom (car curr-eq) sub))) (matches ret-eq-set
                                                                                                    (hash-set sub (car curr-eq)
                                                                                                              (cdr curr-eq)))]
                            [(and (sigma-term? (car curr-eq))
                                  (sigma-term? (cdr curr-eq))
                                  (equal? (sigma-term-symbol (car curr-eq))
                                          (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                 (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                            [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

(define (is-tvar-matching? v)
  (string-prefix? v "t"))
(define (is-non-tvar-matching? v)
  (string-prefix? v "n"))

;; matching for variable solver
;; assume all pattern variables are either target-variable matching or non-target-variable matching
;; tvar is the target variable that occurs in the obj/input expr
(define (varsolver-match patt obj tvar)
  (letrec ([matches (λ (eq-set sub)
                      (if (empty? eq-set) sub
                          (let ([curr-eq (first eq-set)]
                                [ret-eq-set (cdr eq-set)])
                      (cond [(and (term-constant? (car curr-eq))
                                  (term-constant? (cdr curr-eq))
                                  (equal? (car curr-eq) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (string? (car curr-eq))
                                  (indom (car curr-eq) sub)
                                  (equal? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (string? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-tvar-matching? (car curr-eq))
                                  (occurs tvar (cdr curr-eq))) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (string? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-non-tvar-matching? (car curr-eq))
                                  (not (occurs tvar (cdr curr-eq)))) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (sigma-term? (car curr-eq))
                                  (sigma-term? (cdr curr-eq))
                                  (equal? (sigma-term-symbol (car curr-eq))
                                          (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                 (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                            [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

;; rewrite: (term * term) list -> term -> term
(define (rewrite rules input)
  (letrec ([f (λ (ruleset)
                (if (empty? ruleset) 'fail ;; tried to match input to all rules and failed
                    (let* ([LHS (car (car ruleset))]
                           [RHS (cdr (car ruleset))]
                           [subst (match LHS input)])
                      (if (equal? subst 'fail) ;; this rule doesn't match input
                          (f (cdr ruleset))
                          (lift subst RHS)))))]) ;; this rule does match, return rewritten input
    (f rules)))

;; norm: (term * term) list -> term -> term
;; TRAT calls this norm but it's a kleine closure on -->_R
;; rewrites an expression bottom-up
(define (rewrite* rules input)
  (letrec ([f (λ (expr)
                (if (or (term-variable? expr) (term-constant? expr)) expr ;; when we hit a variable or constant, do nothing & go up the stack
                    ;; fully normalize all the subterms
                    (let ([rewritten-term (sigma-term (sigma-term-symbol expr) (map f (sigma-term-term-list expr)))])
                      ;; if we can rewrite the new term, recurse, else we're done
                      (let ([rewrite-output (rewrite rules rewritten-term)])
                        (if (symbol? rewrite-output)
                            rewritten-term
                            (f rewrite-output))))))])
    (f input)))
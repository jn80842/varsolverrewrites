#lang racket

(provide sigma-term match varsolver-match)

(struct vname (str int) #:transparent)

;; string representing symbol, followed by list of term arguments
(struct sigma-term (symbol term-list) #:transparent)

;; terms are either vname/variables or sigma-terms
;; let's make variables strings for now

;; subst is a hash-table mapping variables to 
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
                      [else (sigma-term (sigma-term-symbol t1) (map f (sigma-term-term-list t1)))]))])
    (f t)))

;; occurs: vname -> term -> bool
(define (occurs var t)
  (letrec ([f (λ (t1)
                (if (string? t1)
                    (eq? var t1)
                    (f (sigma-term-term-list t1))))])
    (f t)))

;; match: term -> term -> subst
;; given a pattern (LHS) and object (input term), find a substitution that will match one to the other
;; instead of throwing an exception, return 'fail symbol
(define (match patt obj)
  (letrec ([matches (λ (eq-set sub)
                      (if (empty? eq-set) sub
                          (let ([curr-eq (first eq-set)]
                                [ret-eq-set (cdr eq-set)])
                      (cond [(and (string? (car curr-eq))
                                  (indom (car curr-eq) sub)
                                  (eq? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (string? (car curr-eq)) (not (indom (car curr-eq) sub))) (matches ret-eq-set
                                                                                                    (hash-set sub (car curr-eq)
                                                                                                              (cdr curr-eq)))]
                            [(and (sigma-term? (car curr-eq))
                                  (sigma-term? (cdr curr-eq))
                                  (eq? (sigma-term-symbol (car curr-eq))
                                       (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                 (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                            [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

(define (is-tvar-matching? v)
  (string-prefix? v "t"))
(define (is-non-tvar-matching? v)
  (string-prefix? v "n"))
(define (contains-target-variable? term tvar)
  (letrec ([f (λ (term)
                (if (string? term)
                    (eq? term tvar)
                    (ormap f (sigma-term-term-list term))))])
    (f term)))
;; matching for variable solver
;; assume all pattern variables are either target-variable matching or non-target-variable matching
;; tvar is the target variable that occurs in the obj/input expr
(define (varsolver-match patt obj tvar)
  (letrec ([matches (λ (eq-set sub)
                      (if (empty? eq-set) sub
                          (let ([curr-eq (first eq-set)]
                                [ret-eq-set (cdr eq-set)])
                      (cond [(and (string? (car curr-eq))
                                  (indom (car curr-eq) sub)
                                  (eq? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (string? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-tvar-matching? (car curr-eq))
                                  (contains-target-variable? (cdr curr-eq) tvar)) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (string? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-non-tvar-matching? (car curr-eq))
                                  (not (contains-target-variable? (cdr curr-eq) tvar))) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (sigma-term? (car curr-eq))
                                  (sigma-term? (cdr curr-eq))
                                  (eq? (sigma-term-symbol (car curr-eq))
                                       (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                 (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                            [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

(define patt1 (sigma-term "+" (list "x" "y")))
(define obj1 (sigma-term "+" (list "a" "b")))
(define obj (sigma-term "+" (list (sigma-term "+" (list "a" "b")) "c")))

;; rewrite: (term * term) list -> term -> term
(define (rewrite rules input)
  (letrec ([f (λ (ruleset)
                (if (empty? ruleset) 'fail ;; tried to match input to all rules and failed
                    (let* ([LHS (car (car ruleset))]
                           [RHS (cdr (car ruleset))]
                           [subst (match LHS input)])
                      (if (eq? subst 'fail) ;; this rule doesn't match input
                          (f (cdr ruleset))
                          (lift subst RHS)))))]) ;; this rule does match, return rewritten input
    (f rules)))

(define rule1-LHS (sigma-term "+" (list "x" "x")))
(define rule1-RHS (sigma-term "*" (list "x" "2")))
(define input (sigma-term "+" (list "a" "a")))

;; norm: (term * term) list -> term -> term
;; TRAT calls this norm but it's a kleine closure on -->_R
;; rewrites an expression bottom-up
#;(define (rewrite* rules input)
  (letrec ([f (λ (expr)
                (if (string? expr) expr ;; when we hit a variable, do nothing & go up the stack
                    ;; fully normalize all the subterms
                    (let ([rewritten-term (sigma-term (sigma-term-symbol expr) (map f (sigma-term-term-list)))])
                      ;; attempt to rewrite 
                      (f (rewrite rules rewritten-term)))))])
    (f input)))
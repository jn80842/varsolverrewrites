#lang racket

(require "termIR.rkt")

(provide match rewrite rewrite*
         varsolver-match varsolver-rewrite varsolver-rewrite*
         unify varsolver-unify lift lift-pairs varsolver-rules-rewrite
         varsolver-rules-rewrite*)

;; the unify algorithm relies on applying substitutions in a particular order,
;; so there are alternate implementations of indom, app, lift
;; that use lists of pairs rather than hashes

;; indom: vname -> subst -> bool, returns true if variable has a mapping in subst
(define (indom v sub)
  (hash-has-key? sub v))
(define (indom-pairs v sub)
  (not (not (memf (λ (p) (equal? (car p) v)) sub))))

;; app: subst -> vname -> term
(define (app sub v)
  (hash-ref sub v))
(define (app-pairs sub v)
  (cdr (findf (λ (p) (equal? (car p) v)) sub)))

;; lift: subst -> term -> term
(define (lift sub t)
  (letrec ([f (λ (t1)
                (cond [(term-variable? t1) (if (indom t1 sub) (app sub t1) t1)]
                      [(term-constant? t1) t1]
                      [else (sigma-term (sigma-term-symbol t1) (map f (sigma-term-term-list t1)))]))])
    (f t)))

(define (lift-pairs sub t)
  (letrec ([f (λ (t1)
                (cond [(term-variable? t1) (if (indom-pairs t1 sub) (app-pairs sub t1) t1)]
                      [(term-constant? t1) t1]
                      [else (sigma-term (sigma-term-symbol t1) (map f (sigma-term-term-list t1)))]))])
    (f t)))

;; occurs: vname -> term -> bool
(define (occurs var t)
  (letrec ([f (λ (t1)
                (cond [(term-variable? t1) (equal? var t1)]
                      [(term-constant? t1) #f]
                      [else (ormap f (sigma-term-term-list t1))]))])
    (f t)))

;; solve: (term * term) list * subst -> subst
;; elim: vname -> term -> (term * term) list -> subst -> subst

;; unify: (term * term) -> subst
(define (unify term1 term2)
  (letrec ([solve (λ (pairs sub)
                    (if (empty? pairs)
                        sub
                        (let ([t1 (car (car pairs))]
                              [t2 (cdr (car pairs))]
                              [ts (cdr pairs)])
                          (cond [(and (term-variable? t1) (equal? t1 t2)) (solve ts sub)]
                                [(term-variable? t1) (elim t1 t2 ts sub)]
                                [(term-variable? t2) (elim t2 t1 ts sub)]
                                [(and (sigma-term? t1)
                                      (sigma-term? t2)
                                      (equal? (sigma-term-symbol t1)
                                              (sigma-term-symbol t2))) (solve (append (map cons (sigma-term-term-list t1)
                                                                                           (sigma-term-term-list t2)) ts) sub)]
                                [else 'fail]))))]
           [elim (λ (x t pairs sub)
                   (if (occurs x t)
                       'fail
                       (let ([push-through (curry lift-pairs (list (cons x t)))])
                         (solve (map (λ (p) (cons (push-through (car p)) (push-through (cdr p)))) pairs)
                                     (cons (cons x t) (map (λ (mapping) (cons (car mapping) (push-through (cdr mapping)))) sub))))))])
    (solve (list (cons term1 term2)) '())))

;; unify: (term * term) -> subst
(define (varsolver-unify term1 term2)
  (letrec ([solve (λ (pairs sub)
                    (if (empty? pairs)
                        sub
                        (let ([t1 (car (car pairs))]
                              [t2 (cdr (car pairs))]
                              [ts (cdr pairs)])
                          (cond [(and (term-variable? t1) (equal? t1 t2)) (solve ts sub)]
                                [(and (term-variable? t1) (can-match-var-to-term? t1 t2)) (elim t1 t2 ts sub)]
                                [(and (term-variable? t2) (can-match-var-to-term? t2 t1)) (elim t2 t1 ts sub)]
                                [(and (sigma-term? t1)
                                      (sigma-term? t2)
                                      (equal? (sigma-term-symbol t1)
                                              (sigma-term-symbol t2))) (solve (append (map cons (sigma-term-term-list t1)
                                                                                           (sigma-term-term-list t2)) ts) sub)]
                                [else 'fail]))))]
           [elim (λ (x t pairs sub)
                   (if (occurs x t)
                       'fail
                       (let ([push-through (curry lift-pairs (list (cons x t)))])
                         (solve (map (λ (p) (cons (push-through (car p)) (push-through (cdr p)))) pairs)
                                (cons (cons x t) (map (λ (mapping) (cons (car mapping) (push-through (cdr mapping)))) sub))))))])
    (solve (list (cons term1 term2)) '())))

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
                            [(and (term-variable? (car curr-eq))
                                  (indom (car curr-eq) sub)
                                  (equal? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (term-variable? (car curr-eq)) (not (indom (car curr-eq) sub))) (matches ret-eq-set
                                                                                                    (hash-set sub (car curr-eq)
                                                                                                              (cdr curr-eq)))]
                            [(and (sigma-term? (car curr-eq))
                                  (sigma-term? (cdr curr-eq))
                                  (equal? (sigma-term-symbol (car curr-eq))
                                          (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                 (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                            [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

;; matching for variable solver
;; assume all pattern variables are either target-variable matching or non-target-variable matching
;; tvar is the target variable that occurs in the obj/input expr
(define (varsolver-match tvar patt obj)
  (letrec ([matches (λ (eq-set sub)
                      (if (empty? eq-set) sub
                          (let ([curr-eq (first eq-set)]
                                [ret-eq-set (cdr eq-set)])
                            (cond [(and (term-constant? (car curr-eq))
                                  (term-constant? (cdr curr-eq))
                                  (equal? (car curr-eq) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (term-variable? (car curr-eq))
                                  (indom (car curr-eq) sub)
                                  (equal? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                            [(and (term-variable? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-tvar-matching? (car curr-eq))
                                  (occurs tvar (cdr curr-eq))) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (term-variable? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-non-tvar-matching? (car curr-eq))
                                  (not (occurs tvar (cdr curr-eq)))) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (term-variable? (car curr-eq))
                                  (not (indom (car curr-eq) sub))
                                  (is-general-matching? (car curr-eq))) (matches ret-eq-set (hash-set sub (car curr-eq) (cdr curr-eq)))]
                            [(and (sigma-term? (car curr-eq))
                                  (sigma-term? (cdr curr-eq))
                                  (equal? (sigma-term-symbol (car curr-eq))
                                          (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                 (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                            [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

;; match variable solver style rules against each other
;; target variables start with 't', non target variables with 'n', general with any other character
(define (varsolver-rules-match patt obj)
  (letrec ([matches (λ (eq-set sub)
                      (if (empty? eq-set) sub
                          (let ([curr-eq (first eq-set)]
                                [ret-eq-set (cdr eq-set)])
                            (cond [(and (term-constant? (car curr-eq))
                                        (term-constant? (cdr curr-eq))
                                        (equal? (car curr-eq) (cdr curr-eq))) (matches ret-eq-set sub)]
                                  [(and (term-variable? (car curr-eq))
                                        (indom (car curr-eq) sub)
                                        (equal? (app sub (car curr-eq)) (cdr curr-eq))) (matches ret-eq-set sub)]
                                  [(and (term-variable? (car curr-eq))
                                        (can-match-var-to-term? (car curr-eq) (cdr curr-eq))) (matches ret-eq-set (hash-set sub (car curr-eq)
                                                                                                                            (cdr curr-eq)))]
                                  [(and (sigma-term? (car curr-eq))
                                        (sigma-term? (cdr curr-eq))
                                        (equal? (sigma-term-symbol (car curr-eq))
                                                (sigma-term-symbol (cdr curr-eq)))) (matches (append (map cons (sigma-term-term-list (car curr-eq))
                                                                                                          (sigma-term-term-list (cdr curr-eq))) ret-eq-set) sub)]
                                  [else 'fail]))))])
    (matches (list (cons patt obj)) (make-immutable-hash '()))))

;; rewrite: (term * term) list -> term -> term
(define (rewrite-parameterize matcher rules input [rule->string (λ (r) "")])
  (letrec ([f (λ (ruleset)
                (if (empty? ruleset) 'fail ;; tried to match input to all rules and failed
                    (let* ([r (car ruleset)]
                           [subst (matcher (rule-lhs r) input)])
                      (if (equal? subst 'fail) ;; this rule doesn't match input
                          (f (cdr ruleset))
                          (let ([rewritten-input (lift subst (rule-rhs r))]) ;; this rule does match, return rewritten input
                            (displayln (format "~a -> ~a via ~a" (termIR->halide input) (termIR->halide rewritten-input)
                                               (rule->string r)))
                            rewritten-input)
                          ))))])
    (f rules)))

(define rewrite (curry rewrite-parameterize match))
(define (varsolver-rewrite tvar rules input [rule->string (λ (r) "")])
  ((curry rewrite-parameterize (curry varsolver-match tvar)) rules input rule->string))
(define (varsolver-rules-rewrite rules input [rule->string (λ (r) "")])
  ((curry rewrite-parameterize varsolver-rules-match) rules input))
;; norm: (term * term) list -> term -> term
;; TRAT calls this norm but it's a kleine closure on -->_R
;; rewrites an expression bottom-up
(define (rewrite*-parameterize rewriter rules input [rule->string (λ (r) "")])
  (letrec ([f (λ (expr)
                (if (or (term-variable? expr) (term-constant? expr)) expr ;; when we hit a variable or constant, do nothing & go up the stack
                    ;; fully normalize all the subterms
                    (let ([rewritten-term (sigma-term (sigma-term-symbol expr) (map f (sigma-term-term-list expr)))])
                      ;; if we can rewrite the new term, recurse, else we're done
                      (let ([rewrite-output (rewriter rules rewritten-term rule->string)])
                        (if (symbol? rewrite-output)
                            rewritten-term
                            (f rewrite-output))))))])
    (f input)))

(define rewrite* (curry rewrite*-parameterize rewrite))
(define (varsolver-rewrite* tvar rules input [rule->string (λ (r) "")])
  ((curry rewrite*-parameterize (curry varsolver-rewrite tvar)) rules input rule->string))
(define varsolver-rules-rewrite* (curry rewrite*-parameterize varsolver-rules-rewrite))
#lang racket

(require "termIR.rkt")
(require "matching.rkt")

(provide (struct-out critical-pair))
(provide CP CPs critical-pairs2 critical-pairs
         top-level-critical-pairs joinable? get-cp-divergent-terms)

(struct critical-pair (rule1 rule2 subst) #:transparent)

(define (CP context t rhs1 rule2 orig-rule1)
  (let ([sigma (varsolver-unify t (rule-lhs rule2))])
    (if (equal? sigma 'fail)
        (list)
        (critical-pair orig-rule1 rule2 sigma))))
       ; (eq-identity (lift sigma rhs1) (lift sigma (context (rule-rhs rule2)))))))

(define (CPs ruleset rule1)
  (let* ([varmap-term-pair (rename-to-tarvar-aware-vars (rule-lhs rule1) (make-hash '()))]
         [renamed-lhs (cdr varmap-term-pair)]
         [varmap-term-pair2 (rename-to-tarvar-aware-vars (rule-rhs rule1) (car varmap-term-pair))]
         [renamed-rhs (cdr varmap-term-pair2)]
         [orig-rule1 (rule renamed-lhs renamed-rhs (rule-name rule1))])
  (letrec ([cps (λ (context lhs rhs)
                  (if (or (term-variable? lhs) (term-constant? lhs))
                      (list)
                      (append (map (λ (rule2) (CP context lhs rhs rule2 orig-rule1)) ruleset)
                              (innercps context (sigma-term-symbol lhs) (list) (sigma-term-term-list lhs) rhs))))]
           [innercps (λ (context sym args1 args2 rhs)
                       (if (empty? args2)
                           (list)
                           (let ([cf (λ (s) (context (sigma-term sym (append args1 (list s) (cdr args2)))))])
                             (append (cps cf (car args2) rhs)
                                     (innercps context sym (append args1 (list (car args2))) (cdr args2) rhs)))))])
    (flatten (cps identity renamed-lhs renamed-rhs)))))

(define (critical-pairs2 R1 R2)
  (flatten (map (λ (r) (CPs R1 r)) R2)))

(define (critical-pairs ruleset)
  (filter (λ (c) (not (trivial-critical-pair? c))) (critical-pairs2 ruleset ruleset)))

(define (trivial-critical-pair? cp)
  (andmap (λ (p) (and (term-variable? (car p)) (term-variable? (cdr p)))) (critical-pair-subst cp)))

(define (top-level-CPs ruleset r)
  (let* ([varmap-term-pair (rename-to-tarvar-aware-vars (rule-lhs r) (make-hash '()))]
         [renamed-lhs (cdr varmap-term-pair)]
         [varmap-term-pair2 (rename-to-tarvar-aware-vars (rule-rhs r) (car varmap-term-pair))]
         [renamed-rhs (cdr varmap-term-pair2)]
         [orig-rule1 (rule renamed-lhs renamed-rhs (rule-name r))])
   (filter (λ (c) (not (trivial-critical-pair? c)))
           (filter identity (map (λ (r2)  (let ([sigma (varsolver-unify renamed-lhs (rule-lhs r2))])
                                            (if (equal? sigma 'fail)
                                                #f
                                                (critical-pair orig-rule1 r2 sigma)))) ruleset)))))

(define (top-level-critical-pairs ruleset)
  (flatten (map (λ (r) (top-level-CPs ruleset r)) ruleset)))

(define (get-cp-divergent-terms cp TRS)
  (let ([x (cdr (rename-to-tarvar-aware-vars (lift-pairs (critical-pair-subst cp) (rule-lhs (critical-pair-rule1 cp)))
                                              (make-hash '())
                                              (list "ta" "na" "va")))])
    (cons (varsolver-rules-rewrite* (list (critical-pair-rule1 cp)) x)
          (varsolver-rules-rewrite* (list (critical-pair-rule2 cp)) x))))

(define (joinable? cp TRS)
  (let* ([p (get-cp-divergent-terms cp TRS)]
         [y1 (car p)]
         [y2 (cdr p)])
    (equal? (varsolver-rules-rewrite* TRS y1) (varsolver-rules-rewrite* TRS y2))))
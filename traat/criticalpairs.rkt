#lang racket

(require "termIR.rkt")
(require "matching.rkt")

(provide CP CPs critical-pairs2 critical-pairs)

(define (CP context t rhs1 rule2)
  (let ([sigma (unify t (rule-lhs rule2))])
    (if (equal? sigma 'fail)
        (list)
        (eq-identity (lift sigma rhs1) (lift sigma (context (rule-rhs rule2)))))))

(define (CPs ruleset rule1)
  (letrec ([cps (λ (context lhs rhs)
                    (if (term-variable? lhs)
                        (list)
                        (append (map (λ (rule2) (CP context lhs rhs rule2)) ruleset)
                                (innercps context (sigma-term-symbol lhs) (list) (sigma-term-term-list lhs) rhs))))]
           [innercps (λ (context sym args1 args2 rhs)
                       (if (empty? args2)
                           (list)
                           (let ([cf (λ (s) (context (sigma-term sym (append args1 (list s) (cdr args2)))))])
                             (append (cps cf (car args2) rhs)
                                     (innercps context sym (append args1 (list (car args2))) (cdr args2) rhs)))))])
    (let* ([varmap-term-pair (rename-to-fresh-vars (rule-lhs rule1) (make-hash '()))]
           [renamed-lhs (cdr varmap-term-pair)]
           [varmap-term-pair2 (rename-to-fresh-vars (rule-rhs rule1) (car varmap-term-pair))]
           [renamed-rhs (cdr varmap-term-pair2)])
      (flatten (cps identity renamed-lhs renamed-rhs)))))

(define (critical-pairs2 R1 R2)
  (flatten (map (λ (r) (CPs R1 r)) R2)))

(define (critical-pairs ruleset)
  (critical-pairs2 ruleset ruleset))
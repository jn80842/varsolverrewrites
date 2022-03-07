#lang racket

(require "termIR.rkt")
(require "matching.rkt")
(require "../halide-parser.rkt")

(provide (struct-out critical-pair))
(provide CP CPs critical-pairs2 critical-pairs
         top-level-critical-pairs joinable? solved-joinable?
         get-cp-divergent-terms get-cp-equality
         trivial-critical-pair?)

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
 ; (filter (λ (c) (not (trivial-critical-pair? c))) (critical-pairs2 ruleset ruleset)))
  (critical-pairs2 ruleset ruleset))

;; this holds for mappings where one key is mapped to mutiple unique terms, which are not trivial
#;(define (trivial-critical-pair? cp)
  (andmap (λ (p) (and (term-variable? (car p)) (term-variable? (cdr p)))) (critical-pair-subst cp)))
(define (trivial-critical-pair? cp)
  (let ([subst-range (map cdr (critical-pair-subst cp))])
    (and (equal? (rule-name (critical-pair-rule1 cp)) (rule-name (critical-pair-rule2 cp)))
         (andmap term-variable? subst-range)
         (not (check-duplicates subst-range)))))

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

(define (get-cp-divergent-terms cp)
  (let ([x (cdr (rename-to-tarvar-aware-vars (lift-pairs (critical-pair-subst cp) (rule-lhs (critical-pair-rule1 cp)))
                                              (make-hash '())
                                              (list "ta" "na" "va")))])
    (cons (varsolver-rules-rewrite* (list (critical-pair-rule1 cp)) x)
          (varsolver-rules-rewrite* (list (critical-pair-rule2 cp)) x))))

(define (get-cp-equality cp name)
  (let* ([terms (get-cp-divergent-terms cp '())]
         [renamed-terms (rename-to-tarvar-aware-term-pairs terms (make-hash '()) (list "t" "n" "v"))])
    (eq-identity (car renamed-terms) (cdr renamed-terms) name)))

(define (joinable? cp TRS)
  (let* ([p (get-cp-divergent-terms cp)]
         [y1 (car p)]
         [y2 (cdr p)])
    (equal? (varsolver-rules-rewrite* TRS y1) (varsolver-rules-rewrite* TRS y2))))

(define (solved-joinable? cp TRS)
    (let* ([p (get-cp-divergent-terms cp)]
         [y1 (varsolver-rules-rewrite* TRS (car p))]
         [y2 (varsolver-rules-rewrite* TRS (cdr p))])
    (or (and (termIR->rule-in-solved-form? y1) (termIR->rule-in-solved-form? y2))
     (equal? y1 y2))))

(define (updated-ruleset ruleset Rnew)
  (append
   (for/list ([r (filter (λ (r) (equal? (rule-lhs r)
                                        (varsolver-rules-rewrite* (list Rnew) (rule-lhs r)))) ruleset)])
     (let ([new-rhs (varsolver-rules-rewrite* (append ruleset (list Rnew)) (rule-rhs r))])
       (unless (equal? new-rhs (rule-rhs r))
         (displayln (format "~a RHS has been rewritten" (rule-name r))))
       (rule (rule-lhs r) new-rhs (rule-name r))))
   (list Rnew)))

(define (updated-identities ruleset Rnew)
  (for/list ([r (filter (λ (r) (not (equal? (rule-lhs r)
                                            (varsolver-rules-rewrite* (list Rnew) (rule-lhs r))))) ruleset)])
    (let ([new-lhs (varsolver-rules-rewrite* (append ruleset (list Rnew)) (rule-lhs r))])
      (unless (equal? (rule-lhs r) new-lhs)
        (displayln (format "~a LHS has been rewritten" (rule-name r))))
      (rule new-lhs (rule-rhs r) (rule-name r)))))

(define (nonjoinable-CPs ruleset newrule)
  (filter (λ (cp) (and (not (joinable? cp ruleset))
                       (not (solved-joinable? cp ruleset)))) (CPs ruleset newrule)))

(define (find-rule-from-cp cp TRS order? rname)
  (let* ([p (get-cp-divergent-terms cp)]
         [side1 (varsolver-rules-rewrite* TRS (car p))]
         [side2 (varsolver-rules-rewrite* TRS (cdr p))]
         [rorient1 (rule side1 side2 rname)]
         [rorient2 (rule side2 side1 rname)])
    (cond [(order? rorient1) rorient1]
          [(order? rorient2) rorient2]
          [else (begin
                  (displayln (format "Could not orient ~a AND ~a" side1 side2))
                  'FAIL)])))

(define (print-cp-divergent-terms cp)
  (let ([p (get-cp-divergent-terms cp)])
    (displayln (format "~a AND ~a" (termIR->halide (car p)) (termIR->halide (cdr p))))))

(define (print-normalized-divergent-terms cp ruleset)
  (let ([p (get-cp-divergent-terms cp)])
    (displayln (format "~a AND ~a" (termIR->halide (varsolver-rules-rewrite* ruleset (car p)))
                       (termIR->halide (varsolver-rules-rewrite* ruleset (cdr p)))))))

(define exampleTRS-halide (list
                    (list "(n0 + t0)" "(t0 + n0)" "coqAdd1")
                    (list "((t0 + n0) + n1)" "(t0 + (n0 + n1))" "coqAdd6")
                    (list "(n0 + (t0 + n1))" "(t0 + (n0 + n1))" "coqAdd36")
                    (list "((n0 + n1) + (t0 + n2))" "((n0 + t0) + (n1 + n2))" "coqAdd15")
                    (list "((t0 - t1) + n0)" "(t0 - (t1 - n0))" "coqAdd68")
                    (list "((t0 + n0) - t1)" "((t0 - t1) + n0)" "coqSub155")
                    (list "(t0 - t0)" "0" "coqSub133")
                    (list "min(n0, t0)" "min(t0, n0)" "vsmin449")
                    (list "min(t0, t0)" "t0" "vsmin473")))

(define exampleTRS (map (λ (l) (rule (halide->termIR (first l))
                                     (halide->termIR (second l))
                                     (third l))) exampleTRS-halide))

(define completion-rules
  (list (rule (halide->termIR "(t0 - (t1 - n0)) - t2")
              (halide->termIR "((t0 - t1) - t2) + n0")
              "completionrule1")))



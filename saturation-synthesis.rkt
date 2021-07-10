#lang rosette

(require "traat/termIR.rkt")

(provide find-all-patterns)

;; find all patterns that can match the full input term
;; NB: we always replace the same expr with the same variable
;; so "((x*y) + (x*y))" will not produce the pattern "v0 + v1" even though it could match it
;; TODO: keep track of target variable
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
                                                       (format "v~a" (sub1 counter)))])
                                  (hash-set! expr-to-var e fresh-var)
                                  fresh-var))))]
           [outer (λ (t)
                    (cond [(term-variable? t) (list t)]
                          [(term-constant? t) (list (get-fresh-var t tvar) t)]
                          [else (cons (get-fresh-var t tvar) (flatten (inner (sigma-term-symbol t) '() (sigma-term-term-list t))))]))]
           [inner (λ (sym args1 args2)
                    (if (empty? args2)
                        (sigma-term sym args1)
                        (let ([arg-versions (outer (car args2))])
                          (map (λ (a) (inner sym (append args1 (list a)) (cdr args2))) arg-versions))))])
    (outer term)))
#lang racket

(provide (struct-out sigma-term))
(provide (struct-out rule))
(provide (struct-out eq-identity))
(provide make-rule term-constant? term-variable? termIR->halide
         termIR->variables termIR->in-solved-form? termIR->renamevars
         rename-to-fresh-vars term-size)

;; terms are vname/variables, integers, or sigma-terms
;; let's make variables strings for now

;; subst is a hash-table mapping variables to terms
(struct subst (mapping) #:transparent)

;; a rule has a lefthand side, a righthand side and a name
(struct rule (lhs rhs name) #:transparent)

(define (make-rule lhs rhs)
  (rule lhs rhs ""))

(struct trs (ruleset rule->string order-hash))

(struct eq-identity (lhs rhs) #:transparent)

;; terms can be variables (string representation), integers, booleans (symbols 'true and 'false), or sigma-terms

;; symbol representing Halide function symbol, followed by list of term arguments
(struct sigma-term (symbol term-list) #:transparent)

(define (get-sigma-term-type sterm)
  (if (equal? 'select (sigma-term-symbol sterm))
      'unknown ;; we could recursively check the select args, but do the cheap thing for now
      (if (member (sigma-term-symbol sterm) '(max min + - * / %))
          'integer
          'boolean)))

(define (term-constant? t)
  (or (integer? t) (equal? t 'true) (equal? t 'false)))

(define (term-variable? t)
  (string? t))

(define (termIR->halide t)
  (letrec ([f (λ (tprime)
                (cond [(integer? tprime) (number->string tprime)]
                      [(symbol? tprime) (symbol->string tprime)]
                      [(string? tprime) tprime]
                      [(and (sigma-term? tprime)
                            (or (equal? (sigma-term-symbol tprime) 'max)
                                (equal? (sigma-term-symbol tprime) 'min))) (format "~a(~a, ~a)"
                                                                                   (sigma-term-symbol tprime)
                                                                                   (f (list-ref (sigma-term-term-list tprime) 0))
                                                                                   (f (list-ref (sigma-term-term-list tprime) 1)))]
                      [(and (sigma-term? tprime)
                            (equal? (sigma-term-symbol tprime) 'select)) (format "select(~a, ~a, ~a)"
                                                                                 (f (list-ref (sigma-term-term-list tprime) 0))
                                                                                 (f (list-ref (sigma-term-term-list tprime) 1))
                                                                                 (f (list-ref (sigma-term-term-list tprime) 2)))]
                      [(and (sigma-term? tprime)
                            (equal? (sigma-term-symbol tprime) '!)) (format "!(~a)" (f (list-ref (sigma-term-term-list tprime) 0)))]
                      [(and (sigma-term? tprime)
                            (equal? (sigma-term-symbol tprime) 'or)) (format "(~a || ~a)" (f (list-ref (sigma-term-term-list tprime) 0))
                                                                            (f (list-ref (sigma-term-term-list tprime) 1)))]
                      [else (format "(~a ~a ~a)"
                                   (f (list-ref (sigma-term-term-list tprime) 0))
                                   (sigma-term-symbol tprime)
                                   (f (list-ref (sigma-term-term-list tprime) 1)))]))])
    (f t)))

(define (termIR->variables t)
  (letrec ([f (λ (tprime)
                (cond [(string? tprime) (list tprime)]
                      [(sigma-term? tprime) (flatten (map f (sigma-term-term-list tprime)))]
                      [else '()]))])
    (set->list (list->set (f t)))))

(define (is-target-variable? v)
  (string-prefix? v "t"))

(define (termIR->in-solved-form? term tvar)
  (or (and (term-variable? term) (equal? term tvar))
      (andmap (λ (v) (not (equal? tvar v))) (termIR->variables term))
      (and (sigma-term? term)
           (and (equal? (list-ref (sigma-term-term-list term) 0) tvar)
                (andmap (λ (v) (not (equal? tvar v))) (flatten (map termIR->variables (cdr (sigma-term-term-list term)))))))))

(define (termIR->renamevars term varmap)
  (letrec ([f (λ (tprime)
                (cond [(term-variable? tprime) (hash-ref varmap tprime)]
                      [(term-constant? tprime) tprime]
                      [else (sigma-term (sigma-term-symbol tprime) (map f (sigma-term-term-list tprime)))]))])
    (f term)))

(define (rename-to-fresh-vars term varmap)
  (letrec ([f (λ (tprime)
                (cond [(and (term-variable? tprime) (hash-has-key? varmap tprime)) (hash-ref varmap tprime)]
                      [(and (term-variable? tprime) (not (hash-has-key? varmap tprime))) (begin
                                                                                           (hash-set! varmap tprime (format "v~a" (hash-count varmap)))
                                                                                           (hash-ref varmap tprime))]
                      [(term-constant? tprime) tprime]
                      [else (sigma-term (sigma-term-symbol tprime) (map f (sigma-term-term-list tprime)))]))])
    (cons varmap (f term))))

(define (term-size input-term)
  (letrec ([f (λ (t)
                (if (or (term-variable? t) (term-constant? t))
                    1
                    (foldl + 1 (map f (sigma-term-term-list t)))))])
    (f input-term)))
                
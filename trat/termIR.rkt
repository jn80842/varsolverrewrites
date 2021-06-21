#lang racket

(provide (struct-out sigma-term))
(provide term-constant? term-variable? termIR->halide
         termIR->variables termIR->in-solved-form?)

(struct vname (str int) #:transparent)

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
  (or (andmap (λ (v) (not (equal? tvar v))) (termIR->variables term))
      (and (sigma-term? term)
           (and (equal? (list-ref (sigma-term-term-list term) 0) tvar)
                (andmap (λ (v) (not (equal? tvar v))) (flatten (map termIR->variables (cdr (sigma-term-term-list term)))))))))